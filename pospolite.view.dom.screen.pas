unit Pospolite.View.DOM.Screen;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, LCLType, LCLIntf, Pospolite.View.Basics;

type

  { TPLDOMScreen }

  TPLDOMScreen = record
  public
    function height: TPLInt; inline;
    function width: TPLInt; inline;
    function availHeight: TPLInt; inline;
    function availWidth: TPLInt; inline;
    function colorDepth: TPLShortInt;
    function pixelDepth: TPLShortInt; inline;
    function deviceScaleFactor: TPLFloat; inline;
  end;

implementation

{ TPLDOMScreen }

function TPLDOMScreen.height: TPLInt;
begin
  Result := Screen.Height;
end;

function TPLDOMScreen.width: TPLInt;
begin
  Result := Screen.Width;
end;

function TPLDOMScreen.availHeight: TPLInt;
begin
  Result := Screen.WorkAreaHeight;
end;

function TPLDOMScreen.availWidth: TPLInt;
begin
  Result := Screen.WorkAreaWidth;
end;

function TPLDOMScreen.colorDepth: TPLShortInt;
var
  dc: HDC;
begin
  dc := GetDC(0);
  Result := GetDeviceCaps(dc, BITSPIXEL);
  ReleaseDC(0, dc);
end;

function TPLDOMScreen.pixelDepth: TPLShortInt;
begin
  Result := colorDepth;
end;

function TPLDOMScreen.deviceScaleFactor: TPLFloat;
begin
  Result := Screen.PixelsPerInch / 96;
end;

end.

