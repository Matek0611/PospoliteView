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
    FRoot: TPLHTMLRootObject;
    FMimeType: TPLString;
    FRenderer: TPLDrawingRenderer;
    function GetContent: TPLString;
    function GetMimeType: TPLString;
    function GetTitle: TPLString;
    procedure SetTitle(AValue: TPLString);
    function GetRoot: IPLHTMLObject;
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

const
  HTML_ERROR_TEMPLATE: TPLString = '<html><head><title>%s</title></head><body><h1>%s</h1><p>%s</p></body></html>';

procedure TPLHTMLDocument.InternalLoad(ASource: TPLString);
var
  p: IPLHTMLParser;
begin
  if not IsLoaded then exit;

  if Assigned(FRoot) then FRoot.Free;
  FRoot := TPLHTMLRootObject.Create(nil, FRenderer);

  if not FMimeType.Exists('html') then begin
    InternalLoadOther(ASource);
    exit;
  end;

  p := TPLHTMLParser.Create;
  try
    p.Parse(ASource, FRoot);
    if p.HasCriticalError then raise Exception.Create(p.Errors.AllInOneString);
  except
    on e: Exception do begin
      FRoot.Children.Clear;
      p.Parse(HTML_ERROR_TEMPLATE.Format(['Error', 'Error occured', e.Message]), FRoot);
    end;
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
end;

destructor TPLHTMLDocument.Destroy;
begin
  inherited Destroy;
end;

procedure TPLHTMLDocument.LoadFromLocalFile(const AFileName: TPLString);
var
  sl: TStringList;
  t: TPLString;
begin
  FIsLocal := true;
  FFile := '';
  FRoot := nil;

  case TPLString(ExtractFileExt(AFileName)).ToLower.Replace('.', '') of
    'html', 'htm': FMimeType := 'text/html';
    'xml': FMimeType := 'text/xml';
    'txt': FMimeType := 'text/plain';
    'css': FMimeType := 'text/css';
    'csv': FMimeType := 'text/csv';
    'php': FMimeType := 'text/php';
    'png': FMimeType := 'image/png';
    'webp': FMimeType := 'image/webp';
    'jpg', 'jpeg', 'jfif', 'pjpeg', 'pjp': FMimeType := 'image/jpeg';
    'tif', 'tiff': FMimeType := 'image/tiff';
    'ico': FMimeType := 'image/vnd.microsoft.icon';
    'mp3', 'mpeg': FMimeType := 'audio/mpeg';
    'pdf': FMimeType := 'application/pdf';
    'sql': FMimeType := 'application/sql';
    'json': FMimeType := 'application/json';
    'js': FMimeType := 'application/javascript';
    'gif': FMimeType := 'image/gif';
    else exit; // download
  end;

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
    FMimeType := '';  //application/octet-stream
    exit;
  end;

  FFile := AFileName;

  InternalLoad(t);
end;

procedure TPLHTMLDocument.LoadFromURL(const AFileName: TPLString);
var
  oc: IPLHTTPClient;
  t: TPLString;
begin
  FIsLocal := false;
  FFile := '';
  FRoot := nil;

  try
    oc := OnlineClient;
    t := oc.Download(AFileName);
    FMimeType := oc.MimeType.ToLower;

    if (oc.ResponseStatusCode >= 300) or not FMimeType.Exists(['html',
      'htm', 'xml', 'plain', 'css', 'csv', 'php', 'sql', 'json', 'sql', 'pdf',
      'png', 'tiff', 'javascript', 'icon', 'mpeg', 'gif'])
      then raise Exception.Create('');
  except
    t := '';
    FMimeType := '';
    exit;
  end;

  FFile := AFileName;

  InternalLoad(t);
end;

procedure TPLHTMLDocument.LoadFromString(const AText: TPLString);
begin
  FIsLocal := true;
  FFile := '<string>';
  FMimeType := 'text/html';
  FRoot := nil;

  InternalLoad(AText);
end;

procedure TPLHTMLDocument.SaveToLocalFile(const AFileName: TPLString);
var
  sl: TStringList;
begin
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

end.

