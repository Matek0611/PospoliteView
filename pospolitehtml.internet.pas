unit PospoLiteHTML.Internet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil, Controls, strutils, math,
  fphttpclient, openssl, sslsockets, fpopenssl, URIParser, RegExpr,
  PospoLiteHTML.Version, BGRABitmap, BGRABitmapTypes;

  function OnlineClient: TFPHTTPClient;

  procedure CheckAndCorrectURL(var AURL: string; const ALocation: string);
  function GetStringFromURL(const AURL: string): string;

  function LoadOnlineImage(const AUrl: string; out ABitmap: TBGRABitmap): boolean;
  function GetBlankImage: TBGRABitmap;

implementation

var
  AClient: TFPHTTPClient;

function GetBlankImage: TBGRABitmap;
begin
  Result := TBGRABitmap.Create(1, 1, BGRAPixelTransparent);
end;

function OnlineClient: TFPHTTPClient;
begin
  Result := AClient;
end;

function URLExists(AURL: string): boolean;
begin
  Result := ExecRegExpr('((http|https)://)(www.)?[a-zA-Z0-9@:%._\\+~#?&//=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%._\\+~#?&//=]*)', AURL);
end;

procedure CheckAndCorrectURL(var AURL: string; const ALocation: string);
begin
  AURL := AURL.Trim;

  if FileExists(AURL) then exit;
  if URLExists(AURL) then exit;

  if ALocation <> '' then AURL := ALocation.Trim + PathDelim + AURL;

  AURL := AURL.Replace('/', PathDelim);
  if AURL.ToLower.StartsWith('file:' + PathDelim + PathDelim) then begin
    AURL := Copy(AURL, Length('file:' + PathDelim + PathDelim), Length(AURL));
    exit;
  end;

  AURL := AURL.Replace(PathDelim, '/');
  if AURL.StartsWith('//') then AURL := 'https:' + AURL;
end;

function GetStringFromURL(const AURL: string): string;
var
  sl: TStringList;
begin
  if AURL = '' then exit;

  if FileExists(AURL) then begin
    try
      sl := TStringList.Create;
      try
        sl.LoadFromFile(AURL);
        Result := sl.Text;
      finally
        sl.Free;
      end;
    except
      on e: Exception do Result := '';
    end;
  end else begin
    try
      Result := AClient.Get(AURL);
    except
      on e: Exception do Result := '';
    end;
  end;
end;

function LoadOnlineImage(const AUrl: string; out ABitmap: TBGRABitmap): boolean;
var
  AStream: TStream;
begin
  Result := false;
  ABitmap := nil;

  AStream := TMemoryStream.Create;
  try
    try
      try
        AClient.Get(AUrl, AStream);
      except
      end;

      if (AClient.ResponseStatusCode = 200) and (Pos('image/', AClient.ResponseHeaders.Values['Content-Type']) > 0) then begin
        try
          AStream.Seek(0, soFromBeginning);
          ABitmap := TBGRABitmap.Create;
          ABitmap.LoadFromStream(AStream);
          Result := true;
        except
          on e: Exception do
          begin
            ABitmap.Free;
            ABitmap := nil;
            Result := false;
          end;
        end;
      end else Result := false;
    finally
      AStream.Free;
    end;
  except
    on e: Exception do begin
      Result := false;
    end;
  end;
end;

type

  { TClientEvents }

  TClientEvents = class
  private
    procedure EventClientPassword(Sender: TObject; var RepeatRequest: boolean);
    procedure EventClientRedirect(Sender: TObject; const ASrc: string; var ADest: string);
  public
    constructor Create;
  end;

{ TClientEvents }

procedure TClientEvents.EventClientPassword(Sender: TObject;
  var RepeatRequest: boolean);
begin
  //
end;

procedure TClientEvents.EventClientRedirect(Sender: TObject;
  const ASrc: string; var ADest: string);
begin
  //
end;

constructor TClientEvents.Create;
begin
  inherited Create;

  AClient.OnPassword := @EventClientPassword;
  AClient.OnRedirect := @EventClientRedirect;
end;

var
  ClientEvents: TClientEvents;

initialization
  AClient := TFPHTTPClient.Create(nil);
  AClient.ResponseHeaders.NameValueSeparator := ':';
  AClient.AllowRedirect := true;
  AClient.MaxRedirects := High(Byte);
  AClient.HTTPversion := '1.1';
  AClient.AddHeader('User-Agent', TPLHTMLBrowserVersion.UserAgent);
  ClientEvents := TClientEvents.Create;

finalization
  ClientEvents.Free;
  AClient.Free;

end.

