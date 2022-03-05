unit Pospolite.View.HTML.Document;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
  ...
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Parser,
  Pospolite.View.HTML.Basics, Pospolite.View.Internet,
  Pospolite.View.Drawing.Renderer, Pospolite.View.DOM.Document;

type

  { TPLHTMLDocument }

  TPLHTMLDocument = class(TInterfacedObject, IPLHTMLDocument)
  private
    FBody: TPLHTMLObject;
    FFile: TPLString;
    FHead: TPLHTMLObject;
    FIsLocal: TPLBool;
    FRoot: TPLHTMLRootObject;
    FMimeType: TPLString;
    FIsLoading: TPLBool;
    function GetContent: TPLString;
    function GetMimeType: TPLString;
    function GetTitle: TPLString;
    procedure SetTitle(AValue: TPLString);
    function GetRoot: TPLHTMLObject;
  protected
    procedure InternalLoad(ASource: TPLString);
    procedure InternalLoadOther(AData: TPLString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromLocalFile(const AFileName: TPLString);
    procedure LoadFromURL(const AFileName: TPLString);
    procedure LoadFromString(const AText: TPLString);
    procedure SaveToLocalFile(const AFileName: TPLString);
    procedure Reload;
    function IsLoaded: TPLBool; inline;

    function querySelector(const AQuery: TPLString; AObject: TPLHTMLObject = nil): TPLHTMLObject; // if AObject = nil, then AObject = Root
    function querySelectorAll(const AQuery: TPLString; AObject: TPLHTMLObject = nil): TPLHTMLObjects;

    property Title: TPLString read GetTitle write SetTitle;
    property Content: TPLString read GetContent;
    property MimeType: TPLString read GetMimeType;
    property Root: TPLHTMLObject read GetRoot;
    property IsLoading: TPLBool read FIsLoading;
    property Body: TPLHTMLObject read FBody;
    property Head: TPLHTMLObject read FHead;
  end;

implementation

{ TPLHTMLDocument }

function TPLHTMLDocument.GetContent: TPLString;
begin
  if Assigned(FRoot) then Result := FRoot.ToHTML else Result := '';
end;

function TPLHTMLDocument.GetMimeType: TPLString;
begin
  Result := FMimeType;
end;

function TPLHTMLDocument.GetTitle: TPLString;
var
  obj: TPLHTMLObject;
begin
  obj := querySelector('head > title > internal_text_object');
  if Assigned(obj) then Result := obj.Text else Result := '';
end;

procedure TPLHTMLDocument.SetTitle(AValue: TPLString);
var
  obj: TPLHTMLObject;
begin
  obj := querySelector('head > title > internal_text_object');
  if Assigned(obj) then obj.Text := AValue;
end;

function TPLHTMLDocument.GetRoot: TPLHTMLObject;
begin
  Result := FRoot;
end;

const
  HTML_ERROR_TEMPLATE: TPLString = '<html><head><title>%s</title></head><body><h1>%s</h1><pre>%s</pre></body></html>';

procedure TPLHTMLDocument.InternalLoad(ASource: TPLString);
var
  p: TPLHTMLParser;
begin
  if not IsLoaded then exit;

  if Assigned(FRoot) then FreeAndNil(FRoot);
  FRoot := TPLHTMLRootObject.Create(nil);

  if not FMimeType.Exists('html') then begin
    InternalLoadOther(ASource);
    exit;
  end;

  p := TPLHTMLParser.Create;
  try
    try
      p.Parse(ASource, FRoot);
      if p.HasCriticalError then raise Exception.Create(p.Errors.AllInOneString);
    except
      on e: Exception do begin
        FRoot.Children.Clear;
        p.Parse(HTML_ERROR_TEMPLATE.Format(['Error', 'Error occured', e.Message]), FRoot);
      end;
    end;

    FBody := querySelector('internal_root_object > html > body');
    FHead := querySelector('internal_root_object > html > head');
  finally
    p.Free;
  end;
end;

procedure TPLHTMLDocument.InternalLoadOther(AData: TPLString);
begin
  // <> html napisać...
end;

constructor TPLHTMLDocument.Create;
begin
  inherited Create;

  FFile := '';
  FMimeType := 'text/html';
  FIsLocal := true;
  FRoot := nil;
  FIsLoading := false;
  FBody := nil;
  FHead := nil;
end;

destructor TPLHTMLDocument.Destroy;
begin
  if Assigned(FRoot) then FRoot.Free;

  inherited Destroy;
end;

procedure TPLHTMLDocument.LoadFromLocalFile(const AFileName: TPLString);
var
  sl: TStringList;
  t: TPLString;
begin
  FIsLoading := true;
  FIsLocal := true;
  FFile := '';
  FRoot := nil;
  FBody := nil;
  FHead := nil;

  FMimeType := LocalMimeType(AFileName);

  try
    sl := TStringList.Create;
    try
      sl.LoadFromFile(AFileName);
      t := sl.Text;
    finally
      sl.Free;
    end;
  except
    t := '';
    FMimeType := '';
    exit;
  end;

  FFile := AFileName;

  InternalLoad(t);
  FIsLoading := false;
end;

procedure TPLHTMLDocument.LoadFromURL(const AFileName: TPLString);
var
  oc: IPLHTTPClient;
  t: TPLString;
begin
  FIsLoading := true;
  FIsLocal := false;
  FFile := '';
  FRoot := nil;
  FBody := nil;
  FHead := nil;

  try
    oc := OnlineClient;
    t := oc.FileGetContents(AFileName);
    FMimeType := oc.MimeType.ToLower;

    if oc.ResponseStatusCode >= 300 then raise Exception.Create('');
  except
    t := '';
    FMimeType := '';
    exit;
  end;

  FFile := AFileName;

  InternalLoad(t);
  FIsLoading := false;
end;

procedure TPLHTMLDocument.LoadFromString(const AText: TPLString);
begin
  FIsLoading := true;
  FIsLocal := true;
  FFile := '<string>';
  FMimeType := 'text/html';
  FRoot := nil;
  FBody := nil;
  FHead := nil;

  InternalLoad(AText);
  FIsLoading := false;
end;

procedure TPLHTMLDocument.SaveToLocalFile(const AFileName: TPLString);
var
  sl: TStringList;
begin
  if FIsLoading then exit;

  sl := TStringList.Create;
  try
    sl.Text := FRoot.ToHTML;
    sl.SaveToFile(AFileName);
  finally
    sl.Free;
  end;
end;

procedure TPLHTMLDocument.Reload;
begin
  if FFile.IsEmpty or (FFile = '<string>') then exit;

  if FIsLocal then LoadFromLocalFile(FFile) else LoadFromURL(FFile);
end;

function TPLHTMLDocument.IsLoaded: TPLBool;
begin
  Result := not FFile.IsEmpty and not FMimeType.IsEmpty;
end;

function TPLHTMLDocument.querySelector(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObject;
begin
  if not Assigned(AObject) then AObject := FRoot;

  Result := TPLHTMLDocumentQueries.querySelector(AQuery, AObject);
end;

function TPLHTMLDocument.querySelectorAll(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObjects;
begin
  if not Assigned(AObject) then AObject := FRoot;

  Result := TPLHTMLDocumentQueries.querySelectorAll(AQuery, AObject);
end;

end.

