unit Pospolite.View.JS.Basics;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
  Goal: ES2019 Parser & Interpreter
}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, strutils, math, Pospolite.View.Basics;

type

  TPLJSInt = integer;
  TPLJSLong = Int64;

type

  { TPLJSCodePosition }

  TPLJSCodePosition = packed record
  private
    FColumn: SizeInt;
    FLine: SizeInt;
  public
    constructor Create(const ALine, AColumn: SizeInt);

    class operator =(const a, b: TPLJSCodePosition) r: TPLBool; inline;
    class operator :=(const a: TPLJSCodePosition) r: TPLString; inline;

    function ToString: TPLString; inline;

    property Line: SizeInt read FLine;
    property Column: SizeInt read FColumn;
  end;

  { TPLJSCodeLocation }

  TPLJSCodeLocation = packed record
  private
    FFinish: TPLJSCodePosition;
    FSource: TPLString;
    FStart: TPLJSCodePosition;
  public
    constructor Create(const AStart, AFinish: TPLJSCodePosition;
      const ASource: TPLString = '');

    class operator =(const a, b: TPLJSCodeLocation) r: TPLBool; inline;
    class operator :=(const a: TPLJSCodeLocation) r: TPLString; inline;

    function ToString: TPLString; inline;
    function Extract(out AStart, AFinish: TPLJSCodePosition; out ASource: TPLString): TPLJSCodeLocation;

    property Start: TPLJSCodePosition read FStart write FStart;
    property Finish: TPLJSCodePosition read FFinish write FFinish;
    property Source: TPLString read FSource write FSource;
  end;

implementation

{ TPLJSCodePosition }

constructor TPLJSCodePosition.Create(const ALine, AColumn: SizeInt);
begin
  FLine := ALine;
  FColumn := AColumn;
end;

class operator TPLJSCodePosition.=(const a, b: TPLJSCodePosition) r: TPLBool;
begin
  r := (a.FLine = b.FLine) and (a.FColumn = b.FColumn);
end;

class operator TPLJSCodePosition.:=(const a: TPLJSCodePosition) r: TPLString;
begin
  r := a.ToString;
end;

function TPLJSCodePosition.ToString: TPLString;
begin
  Result := '(%d, %d)'.Format([FLine, FColumn]);
end;

{ TPLJSCodeLocation }

constructor TPLJSCodeLocation.Create(const AStart, AFinish: TPLJSCodePosition;
  const ASource: TPLString);
begin
  FStart := AStart;
  FFinish := AFinish;
  FSource := ASource;
end;

class operator TPLJSCodeLocation.=(const a, b: TPLJSCodeLocation) r: TPLBool;
begin
  r := (a.FStart = b.FStart) and (a.FFinish = b.FFinish) and (a.FSource = b.FSource);
end;

class operator TPLJSCodeLocation.:=(const a: TPLJSCodeLocation) r: TPLString;
begin
  r := a.ToString;
end;

function TPLJSCodeLocation.ToString: TPLString;
begin
  Result := '[%s .. %s]: %s'.Format([FStart.ToString, FFinish.ToString, FSource]);
end;

function TPLJSCodeLocation.Extract(out AStart, AFinish: TPLJSCodePosition; out
  ASource: TPLString): TPLJSCodeLocation;
begin
  AStart := FStart;
  AFinish := FFinish;
  ASource := FSource;

  Result := TPLJSCodeLocation.Create(AStart, AFinish, ASource);
end;

end.

