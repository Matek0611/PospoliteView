unit Pospolite.View.Version;

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
  Classes, SysUtils, Pospolite.View.Basics;

type

  TPLViewVersionField = (vvfName, vvfVersion, vvfExVersion, vvfOS,
    vvfJavaScript, vvfUserAgent, vvfModules, vvfAuthor);

  { TPLViewVersion }

  TPLViewVersion = class sealed
  public
    class function GetFieldInfo(AField: TPLViewVersionField): TPLString;

    class function Name: TPLString;
    class function Version: TPLString;
    class function ExVersion: TPLString;
    class function OS: TPLString;
    class function JavaScript: TPLString;
    class function UserAgent: TPLString;
    class function Modules: TPLString;
    class function Author: TPLString;
  end;

implementation

uses sha1 {$IFDEF WINDOWS}, Windows, ActiveX, comobj{$ENDIF};

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
  {$if (FPC_VERSION = 3) and (FPC_RELEASE >= 2)}New(chEaten);{$endif}
  OleCheck(CreateBindCtx(0, bindCtx));
  OleCheck(MkParseDisplayName(BindCtx, StringToOleStr(objectName), chEaten, Moniker));
  OleCheck(Moniker.BindToObject(BindCtx, nil, IDispatch, Result));
  {$if (FPC_VERSION = 3) and (FPC_RELEASE >= 2)}Dispose(chEaten);{$endif}
end;

procedure GetWin32_OperatingSystem;
var
  objWMIService : OLEVariant;
  colItems      : OLEVariant;
  colItem       : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;

begin
  objWMIService := GetWMIObject('winmgmts:\\.\root\CIMV2');
  colItems      := objWMIService.ExecQuery('SELECT * FROM Win32_OperatingSystem', 'WQL', 0);
  oEnum         := IUnknown(colItems._NewEnum) as IEnumVariant;
  if oEnum.Next(1, colItem, iValue) = 0 then
    with _Win32_OperatingSystemInfo do begin
      Caption := colItem.Caption;
      BuildNumber := colItem.BuildNumber;
    end;
end;

{$ENDIF}

{ TPLViewVersion }

class function TPLViewVersion.GetFieldInfo(AField: TPLViewVersionField
  ): TPLString;
begin
  Result := '';

  case AField of
    vvfName: Result := 'Pospolite View';
    vvfVersion: Result := '2022.1.' + {$I %FPCVERSION%};
    vvfExVersion: Result := SHA1Print(SHA1String(Version));
    vvfAuthor: Result := 'Marcin Stefanowicz (Matek)';
    vvfModules: Result := '[1] Modified D2D1 Fragment From Codebot Pascal Library by Anthony Walter';
    vvfJavaScript: Result := 'PospoliteJS ' + Version;
    vvfOS: Result :=
      {$IFDEF WINDOWS}
        _Win32_OperatingSystemInfo.Caption + ' OS (Build ' + _Win32_OperatingSystemInfo.BuildNumber + ')'
      {$ELSE}
      {$IFDEF LINUX} 'Linux' {$ENDIF}{$IFDEF DARWIN} 'MacOS' {$ENDIF}{$IFDEF UNIX} 'UNIX' {$ELSE} 'Undefined' {$ENDIF}
      {$ENDIF};
    vvfUserAgent: Result := 'Mozilla/5.0 ' +
      {$IFDEF WINDOWS}
       '(Windows NT ' + IntToStr(Win32MajorVersion) + '.' + IntToStr(Win32MinorVersion) + '; ' + {$IFDEF WIN64} 'Win64; ' {$ELSE} 'Win32; ' {$ENDIF} + {$IFDEF CPU64} 'x64' {$ELSE} 'x86' {$ENDIF} + ')'
      {$ELSE}
       '(' + OS + '; ' + {$IFDEF CPU64} 'x64' {$ELSE} 'x86' {$ENDIF} + ')'
      {$ENDIF}
      + ' PospoliteView/' + Version;
  end;
end;

class function TPLViewVersion.Name: TPLString;
begin
  Result := GetFieldInfo(vvfName);
end;

class function TPLViewVersion.Version: TPLString;
begin
  Result := GetFieldInfo(vvfVersion);
end;

class function TPLViewVersion.ExVersion: TPLString;
begin
  Result := GetFieldInfo(vvfExVersion);
end;

class function TPLViewVersion.OS: TPLString;
begin
  Result := GetFieldInfo(vvfOS);
end;

class function TPLViewVersion.JavaScript: TPLString;
begin
  Result := GetFieldInfo(vvfJavaScript);
end;

class function TPLViewVersion.UserAgent: TPLString;
begin
  Result := GetFieldInfo(vvfUserAgent);
end;

class function TPLViewVersion.Modules: TPLString;
begin
  Result := GetFieldInfo(vvfModules);
end;

class function TPLViewVersion.Author: TPLString;
begin
  Result := GetFieldInfo(vvfAuthor);
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
    on e: Exception do ;
  end;

{$ENDIF}

end.

