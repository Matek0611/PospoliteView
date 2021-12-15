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
  Pospolite.View.Basics, Pospolite.View.Version;

type

  { IPLHTTPClient }

  IPLHTTPClient = interface
    ['{86FCCD07-E0D4-48D0-A4CE-7AD4F91A8CCD}']
    function GetMimeType: TPLString;
    function GetResponseStatusCode: integer;

    function Download(const AURL: TPLString; out AStream: TStream): TPLBool; overload;
    function Download(const AURL: TPLString; out AStream: TStringStream): TPLBool; overload;
    function Download(const AURL: TPLString): TPLString; overload;

    property ResponseStatusCode: integer read GetResponseStatusCode;
    property MimeType: TPLString read GetMimeType;
  end;

  { TPLFPHTTPClient }

  TPLFPHTTPClient = class(TFPHTTPClient, IPLHTTPClient)
  private
    procedure EventClientPassword(Sender: TObject; var {%H-}RepeatRequest: boolean);
    procedure EventClientRedirect(Sender: TObject; const {%H-}ASrc: string; var {%H-}ADest: string);
    function GetResponseStatusCode: integer;
    function GetMimeType: TPLString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Download(const AURL: TPLString; out AStream: TStream): TPLBool; overload;
    function Download(const AURL: TPLString; out AStream: TStringStream): TPLBool; overload;
    function Download(const AURL: TPLString): TPLString; overload;
  end;

function OnlineClient: IPLHTTPClient;

implementation

function OnlineClient: IPLHTTPClient;
begin
  Result := TPLFPHTTPClient.Create(Application);
end;

{ TPLFPHTTPClient }

procedure TPLFPHTTPClient.EventClientPassword(Sender: TObject;
  var RepeatRequest: boolean);
begin
  //
end;

procedure TPLFPHTTPClient.EventClientRedirect(Sender: TObject;
  const ASrc: string; var ADest: string);
begin
  //
end;

function TPLFPHTTPClient.GetResponseStatusCode: integer;
begin
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
end;

destructor TPLFPHTTPClient.Destroy;
begin
  if Assigned(RequestBody) then RequestBody.Free;

  inherited Destroy;
end;

function TPLFPHTTPClient.Download(const AURL: TPLString; out AStream: TStream):
  TPLBool;
begin
  Result := true;
  AStream := TStream.Create;
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

end.

