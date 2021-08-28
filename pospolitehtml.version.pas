unit PospoLiteHTML.Version;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, sha1;

type

  TPLHTMLBrowserVersionField = (bvfName, bvfVersion, bvfExVersion, bvfOS,
    bvfJavaScript, bvfUserAgent, bvfModules, bvfAuthor);

  { TPLHTMLBrowserVersion }

  TPLHTMLBrowserVersion = class sealed
  public
    class function GetFieldInfo(AField: TPLHTMLBrowserVersionField): string;

    class function Name: string;
    class function Version: string;
    class function ExVersion: string;
    class function OS: string;
    class function JavaScript: string;
    class function UserAgent: string;
    class function Modules: string;
    class function Author: string;
  end;

implementation

uses BESENVersionConstants {$IFDEF WINDOWS}, Windows, ActiveX, comobj{$ENDIF};

{$IFDEF WINDOWS}
type
  TWin32_OperatingSystemInfo = record
    Caption, BuildNumber: Variant;
  end;

var
  _Win32_OperatingSystemInfo: TWin32_OperatingSystemInfo;

function GetWMIObject(const objectName: String): IDispatch;
var
  chEaten: PULONG;
  BindCtx: IBindCtx;
  Moniker: IMoniker;
begin
  OleCheck(CreateBindCtx(0, bindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
end;

procedure GetWin32_OperatingSystem;
var
  objWMIService : OLEVariant;
  colItems      : OLEVariant;
  colItem       : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;

begin
  objWMIService := GetWMIObject('winmgmts:\\localhost\root\cimv2');
  colItems      := objWMIService.ExecQuery('SELECT * FROM Win32_OperatingSystem', 'WQL', 0);
  oEnum         := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem, iValue) = 0 then
    with _Win32_OperatingSystemInfo do begin
      Caption := colItem.Caption;
      BuildNumber := colItem.BuildNumber;
    end;
end;

{$ENDIF}

{ TPLHTMLBrowserVersion }

class function TPLHTMLBrowserVersion.GetFieldInfo(AField: TPLHTMLBrowserVersionField
  ): string;
begin
  Result := '';

  {$IFDEF WINDOWS}
  case AField of
    bvfName: Result := 'PospoLiteHTML';
    bvfVersion: Result := '2020.3';
    bvfExVersion: Result := SHA1Print(SHA1String(Version));
    bvfOS: Result := _Win32_OperatingSystemInfo.Caption + ' (Build ' + _Win32_OperatingSystemInfo.BuildNumber + ')';
    bvfJavaScript: Result := 'PospoLiteJS ' + Version + ', BESEN ' + BESENVersion + '.' +  IntToStr(BESENCodeFormatRevisionNumber);
    bvfUserAgent: Result := 'Mozilla/5.0 ' + '(Windows NT ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '; ' + {$IFDEF WIN64} 'Win64; x64' {$ELSE} 'Win32; x86' {$ENDIF} + ') PospoLiteHTML/' + Version + ' AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.66 Safari/537.36';
    bvfModules: Result :=
      'BGRABitmap: <a href="//wiki.freepascal.org/BGRABitmap">wiki.freepascal.org/BGRABitmap</a><br>' + LineEnding +
      'BESEN: A ECMAScript Fifth Edition Object Pascal Implementation - Copyright (C) 2009-2016, Benjamin ''BeRo'' Rosseaux';
    bvfAuthor: Result := 'Marcin Stefanowicz (Matek)';
  end;
  {$ELSE}
  case AField of
    bvfName: Result := 'PospoLiteHTML';
    bvfVersion: Result := '2020.3';
    bvfExVersion: Result := SHA1Print(SHA1String(Version));
    bvfOS: Result := '';
    bvfJavaScript: Result := 'PospoLiteJS ' + Version + ', BESEN ' + BESENVersion + '.' +  IntToStr(BESENCodeFormatRevisionNumber);
    bvfUserAgent: Result := 'Mozilla/5.0 ' + '' + '; (' + {$IFDEF CPU64} 'x64' {$ELSE} 'x86' {$ENDIF} + ') PospoLiteHTML/' + Version + ' AppleWebKit/537.36 (KHTML, like Gecko) Chrome/87.0.4280.66 Safari/537.36';
    bvfModules: Result :=
      'BGRABitmap: <a href="//wiki.freepascal.org/BGRABitmap">wiki.freepascal.org/BGRABitmap</a><br>' + LineEnding +
      'BESEN: A ECMAScript Fifth Edition Object Pascal Implementation - Copyright (C) 2009-2016, Benjamin ''BeRo'' Rosseaux';
    bvfAuthor: Result := 'Marcin Stefanowicz (Matek)';
  end;
  {$ENDIF}
end;

class function TPLHTMLBrowserVersion.Name: string;
begin
  Result := GetFieldInfo(bvfName);
end;

class function TPLHTMLBrowserVersion.Version: string;
begin
  Result := GetFieldInfo(bvfVersion);
end;

class function TPLHTMLBrowserVersion.ExVersion: string;
begin
  Result := GetFieldInfo(bvfExVersion);
end;

class function TPLHTMLBrowserVersion.OS: string;
begin
  Result := GetFieldInfo(bvfOS);
end;

class function TPLHTMLBrowserVersion.JavaScript: string;
begin
  Result := GetFieldInfo(bvfJavaScript);
end;

class function TPLHTMLBrowserVersion.UserAgent: string;
begin
  Result := GetFieldInfo(bvfUserAgent);
end;

class function TPLHTMLBrowserVersion.Modules: string;
begin
  Result := GetFieldInfo(bvfModules);
end;

class function TPLHTMLBrowserVersion.Author: string;
begin
  Result := GetFieldInfo(bvfAuthor);
end;

{$IFDEF WINDOWS}

initialization

  try
    CoInitialize(nil);
    try
      GetWin32_OperatingSystem;
    finally
      CoUninitialize;
    end;
  except
    on E:Exception do begin end;
  end;

{$ENDIF}

end.

