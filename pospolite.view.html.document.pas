unit Pospolite.View.HTML.Document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Parser,
  Pospolite.View.HTML.Basics, Pospolite.View.Internet,
  Pospolite.View.Drawing.Renderer;

type

  { TPLHTMLDocument }

  TPLHTMLDocument = class(TInterfacedObject, IPLHTMLDocument)
  private
    FFile: TPLString;
    FIsLocal: TPLBool;
    FRoot: IPLHTMLObject;
    FMimeType: TPLString;
    FRenderer: TPLDrawingRenderer;
    function GetContent: TPLString;
    function GetMimeType: TPLString;
    function GetTitle: TPLString;
    procedure SetTitle(AValue: TPLString);
    function GetRoot: IPLHTMLObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromLocalFile(const AFileName: TPLString);
    procedure LoadFromURL(const AFileName: TPLString);
    procedure LoadFromString(const AText: TPLString);
    procedure SaveToLocalFile(const AFileName: TPLString);
    procedure Reload;
    function IsLoaded: TPLBool;

    property Title: TPLString read GetTitle write SetTitle;
    property Content: TPLString read GetContent;
    property MimeType: TPLString read GetMimeType;
    property Root: IPLHTMLObject read GetRoot;
    property Renderer: TPLDrawingRenderer read FRenderer write FRenderer;
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
begin

end;

procedure TPLHTMLDocument.SetTitle(AValue: TPLString);
begin

end;

function TPLHTMLDocument.GetRoot: IPLHTMLObject;
begin
  Result := FRoot;
end;

constructor TPLHTMLDocument.Create;
begin
  inherited Create;

  FFile := '';
  FMimeType := 'text/html';
  FIsLocal := true;
  FRoot := nil;
end;

destructor TPLHTMLDocument.Destroy;
begin
  inherited Destroy;
end;

procedure TPLHTMLDocument.LoadFromLocalFile(const AFileName: TPLString);
begin
  FIsLocal := true;

end;

procedure TPLHTMLDocument.LoadFromURL(const AFileName: TPLString);
begin
  FIsLocal := false;

end;

procedure TPLHTMLDocument.LoadFromString(const AText: TPLString);
var
  p: IPLHTMLParser;
begin
  FIsLocal := true;
  FFile := '<string>';
  FMimeType := 'text/html';
  FRoot := TPLHTMLBasicObject.Create(nil, FRenderer);
  FRoot.Name := 'root_object';

  p := TPLHTMLParser.Create;
  p.Parse(AText, FRoot);
end;

procedure TPLHTMLDocument.SaveToLocalFile(const AFileName: TPLString);
begin

end;

procedure TPLHTMLDocument.Reload;
begin
  if FFile.IsEmpty or (FFile = '<string>') then exit;

  if FIsLocal then LoadFromLocalFile(FFile) else LoadFromURL(FFile);
end;

function TPLHTMLDocument.IsLoaded: TPLBool;
begin
  Result := not FFile.IsEmpty;
end;

end.

