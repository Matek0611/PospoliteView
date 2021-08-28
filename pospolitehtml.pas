{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit PospoLiteHTML;

{$warn 5023 off : no warning about unused units}
interface

uses
  PospoLiteHTML.CSS.Classes, PospoLiteHTML.CSS.Parser, 
  PospoliteHTML.ECMAScript5, PospoLiteHTML.HTML.Canvas, 
  PospoLiteHTML.HTML.Canvas.BGRA, PospoLiteHTML.HTML.Scrollbar, 
  PospoLiteHTML.HTML.Canvas.Base, PospoLiteHTML.HTML.Scrolling, 
  PospoLiteHTML.Internet, PospoLiteHTML.Version, PospoLiteHTML.HTML.Document, 
  PospoLiteHTML.Localization, PospoLiteHTML.CSS.Animations, 
  PospoLiteHTML.HTML.Elements, PospoLiteHTML.HTML.Viewer, 
  PospoLiteHTML.CSS.Properties, PospoLiteHTML.CSS.Rules, 
  PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Selector, 
  PospoLiteHTML.CSS.Media, PospoLiteHTML.CSS.Stylesheet, 
  PospoLiteHTML.CSS.Values, PospoLiteHTML.Cache, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('PospoLiteHTML', @Register);
end.
