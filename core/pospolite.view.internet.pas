unit Pospolite.View.Internet;

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
  Classes, SysUtils, fphttpclient, Forms, openssl, sslsockets, fpopenssl,
  Pospolite.View.Basics, Pospolite.View.Version, URIParser, StrUtils
  {$if (FPC_VERSION = 3) and (FPC_RELEASE >= 2)}, opensslsockets{$endif};

type

  { IPLHTTPClient }

  IPLHTTPClient = interface
    ['{86FCCD07-E0D4-48D0-A4CE-7AD4F91A8CCD}']
    function GetMimeType: TPLString;
    function GetResponseStatusCode: integer;

    function Download(const AURL: TPLString; out AStream: TMemoryStream): TPLBool; overload;
    function Download(const AURL: TPLString; out AStream: TStringStream): TPLBool; overload;
    function Download(const AURL: TPLString): TPLString; overload;

    function FileGetContents(const AURL: TPLString; var AStream: TMemoryStream): TPLBool; overload;
    function FileGetContents(const AURL: TPLString): TPLString; overload;

    property ResponseStatusCode: integer read GetResponseStatusCode;
    property MimeType: TPLString read GetMimeType;
  end;

  { TPLFPHTTPClient }

  TPLFPHTTPClient = class(TFPHTTPClient, IPLHTTPClient)
  private
    FResponseStatusCode: integer;

    procedure EventClientPassword(Sender: TObject; var {%H-}RepeatRequest: boolean);
    procedure EventClientRedirect(Sender: TObject; const {%H-}ASrc: TPLString;
      var {%H-}ADest: TPLString);
    function GetResponseStatusCode: integer;
    function GetMimeType: TPLString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Download(const AURL: TPLString; out AStream: TMemoryStream): TPLBool; overload;
    function Download(const AURL: TPLString; out AStream: TStringStream): TPLBool; overload;
    function Download(const AURL: TPLString): TPLString; overload;

    function FileGetContents(const AURL: TPLString; var AStream: TMemoryStream): TPLBool; overload;
    function FileGetContents(const AURL: TPLString): TPLString; overload;
  end;

function OnlineClient: IPLHTTPClient;
function LocalMimeType(const AFileName: TPLString): TPLString;

implementation

function OnlineClient: IPLHTTPClient;
begin
  Result := TPLFPHTTPClient.Create(Application);
end;

function LocalMimeType(const AFileName: TPLString): TPLString;
begin
  // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
  case TPLString(ExtractFileExt(AFileName)).ToLower.Replace('.', '') of
    '7z': Result := 'application/x-7z-compressed';
    'aac': Result := 'audio/aac';
    'abw': Result := 'application/x-abiword';
    'approj': Result := 'application/algopoint'; // online: text/xml
    'arc': Result := 'application/x-freearc';
    'avi': Result := 'video/x-msvideo';
    'avif': Result := 'image/avif';
    'azw': Result := 'application/vnd.amazon.ebook';
    'bmp': Result := 'image/bmp';
    'bz': Result := 'application/x-bzip';
    'bz2': Result := 'application/x-bzip2';
    'cda': Result := 'application/x-cdf';
    'csh': Result := 'application/x-csh';
    'css': Result := 'text/css';
    'csv': Result := 'text/csv';
    'doc': Result := 'application/msword';
    'docx': Result := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
    'eot': Result := 'application/vnd.ms-fontobject';
    'epub': Result := 'application/epub+zip';
    'gif': Result := 'image/gif';
    'gz': Result := 'application/gzip';
    'html', 'htm': Result := 'text/html';
    'ico': Result := 'image/vnd.microsoft.icon';
    'ics': Result := 'text/calendar';
    'jar': Result := 'application/java-archive';
    'jpg', 'jpeg', 'jfif', 'pjpeg', 'pjp': Result := 'image/jpeg';
    'js', 'mjs': Result := 'application/javascript';
    'json': Result := 'application/json';
    'jsonld': Result := 'application/ld+json';
    'mid': Result := 'audio/midi';
    'midi': Result := 'audio/x-midi';
    'mp3', 'mpeg': Result := 'audio/mpeg';
    'mp4': Result := 'video/mp4';
    'mpkg': Result := 'application/vnd.apple.installer+xml';
    'odp': Result := 'application/vnd.oasis.opendocument.presentation';
    'ods': Result := 'application/vnd.oasis.opendocument.spreadsheet';
    'odt': Result := 'application/vnd.oasis.opendocument.text';
    'oga': Result := 'audio/ogg';
    'ogg': Result := 'application/ogg';
    'ogv': Result := 'video/ogg';
    'opus': Result := 'audio/opus';
    'otf': Result := 'font/otf';
    'pdf': Result := 'application/pdf';
    'php': Result := 'text/php';
    'png': Result := 'image/png';
    'ppt': Result := 'application/vnd.ms-powerpoint';
    'pptx': Result := 'application/vnd.openxmlformats-officedocument.presentationml.presentation';
    'rar': Result := 'application/vnd.rar';
    'rtf': Result := 'application/rtf';
    'sh': Result := 'application/x-sh';
    'sql': Result := 'application/sql';
    'svg': Result := 'image/svg+xml';
    'swf': Result := 'application/x-shockwave-flash';
    'tar': Result := 'application/x-tar';
    'tif', 'tiff': Result := 'image/tiff';
    'ts': Result := 'video/mp2t';
    'ttf': Result := 'font/ttf';
    'txt': Result := 'text/plain';
    'vsd': Result := 'application/vnd.visio';
    'wav': Result := 'audio/wav';
    'weba', 'webm': Result := 'audio/webm';
    'webp': Result := 'image/webp';
    'woff': Result := 'font/woff';
    'woff2': Result := 'font/woff2';
    'xhtml': Result := 'application/xhtml+xml';
    'xls': Result := 'application/vnd.ms-excel';
    'xlsx': Result := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';
    'xml': Result := 'text/xml';
    'xul': Result := 'application/vnd.mozilla.xul+xml';
    'zip': Result := 'application/zip';
    else Result := 'application/octet-stream';
  end;
end;

{ TPLFPHTTPClient }

procedure TPLFPHTTPClient.EventClientPassword(Sender: TObject;
  var RepeatRequest: boolean);
begin
  //
end;

procedure TPLFPHTTPClient.EventClientRedirect(Sender: TObject;
  const ASrc: TPLString; var ADest: TPLString);
begin
  //
end;

function TPLFPHTTPClient.GetResponseStatusCode: integer;
begin
  if FResponseStatusCode > 0 then
    Result := FResponseStatusCode
  else
    Result := inherited ResponseStatusCode;
end;

function TPLFPHTTPClient.GetMimeType: TPLString;
begin
  Result := inherited ResponseHeaders.Values['Content-Type'];
end;

constructor TPLFPHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ResponseHeaders.NameValueSeparator := ':';
  AllowRedirect := true;
  MaxRedirects := High(Byte);
  HTTPversion := '1.1';
  AddHeader('User-Agent', TPLViewVersion.UserAgent);

  OnPassword := @EventClientPassword;
  OnRedirect := @EventClientRedirect;

  FResponseStatusCode := 0;
end;

destructor TPLFPHTTPClient.Destroy;
begin
  if Assigned(RequestBody) then RequestBody.Free;

  inherited Destroy;
end;

function TPLFPHTTPClient.Download(const AURL: TPLString; out
  AStream: TMemoryStream): TPLBool;
begin
  Result := true;
  FResponseStatusCode := 0;

  AStream := TMemoryStream.Create;
  try
    Get(AURL, AStream);
  except
    on e: Exception do begin
      Result := false;
      AStream.Free;
      AStream := nil;
    end;
  end;
end;

function TPLFPHTTPClient.Download(const AURL: TPLString;
  out AStream: TStringStream): TPLBool;
begin
  Result := true;
  FResponseStatusCode := 0;

  AStream := TStringStream.Create('');
  try
    Get(AURL, AStream);
  except
    on e: Exception do begin
      Result := false;
      AStream.Free;
      AStream := nil;
    end;
  end;
end;

function TPLFPHTTPClient.Download(const AURL: TPLString): TPLString;
var
  ss: TStringStream;
begin
  Result := '';
  FResponseStatusCode := 0;

  try
    Download(AURL, ss);
    if Assigned(ss) then
    try
      Result := ss.DataString;
    finally
      ss.Free;
    end;
  except
    on e: Exception do Result := '';
  end;
end;

function TPLFPHTTPClient.FileGetContents(const AURL: TPLString;
  var AStream: TMemoryStream): TPLBool;
var
  url: TURI;
  fn: TPLString;
  ms: TMemoryStream;
begin
  if not Assigned(AStream) or (AURL.Trim = '') then exit(false);
  Result := true;
  AStream.Clear;
  FResponseStatusCode := 0;

  try
    if FileExists(AURL) then begin
      AStream.LoadFromFile(AURL);
      ResponseHeaders.Values['Content-Type'] := LocalMimeType(AURL);
      FResponseStatusCode := 200;
    end else begin
      url := ParseURI(AURL);

      case url.Protocol.ToLower of
        'file': begin
          fn := url.Path.TrimLeft(['/', '\']).Replace('/', PathDelim) + url.Document;

          if FileExists(fn) then begin
            AStream.LoadFromFile(fn);
            ResponseHeaders.Values['Content-Type'] := LocalMimeType(fn);
            FResponseStatusCode := 200;
          end else begin
            Result := false;
            FResponseStatusCode := 404;
          end;
        end;
        'http', 'https': begin
          Result := Download(AURL, ms);

          if Result then begin
            AStream.LoadFromStream(ms);
            ms.Free;
          end;
        end;
      end;
    end;
  except
    on e: Exception do begin
      Result := false;
      AStream.Clear;
      ResponseHeaders.Values['Content-Type'] := '';
      FResponseStatusCode := 400;
    end;
  end;

  AStream.Position := 0;
end;

function TPLFPHTTPClient.FileGetContents(const AURL: TPLString): TPLString;
var
  ms: TMemoryStream;
  ss: TStringStream;
begin
  Result := '';

  try
    ms := TMemoryStream.Create;
    ss := TStringStream.Create('');
    try
      if FileGetContents(AURL, ms) then begin
        {$if (FPC_VERSION = 3) and (FPC_RELEASE >= 2)}
          ss.LoadFromStream(ms);
        {$else}
          ss.CopyFrom(ms, ms.Size);
        {$endif}

        ss.Position := 0;
        Result := ss.DataString;
      end;
    finally
      ms.Free;
      ss.Free;
    end;
  except
    on e: Exception do Result := '';
  end;
end;

end.

