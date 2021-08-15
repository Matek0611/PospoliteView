{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pospolite_view;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pospolite.View.Basics, Pospolite.View.JS.Basics, Pospolite.View.DOM.Screen, 
  Pospolite.View.DOM.Window, Pospolite.View.CSS.Basics, 
  Pospolite.View.CSS.Declaration, Pospolite.View.CSS.Selector, 
  Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Drawer, 
  Pospolite.View.Drawing.DrawerD2D1, 
  Pospolite.View.Drawing.DrawerD2D1.Definitions, 
  Pospolite.View.Drawing.ImageCodecs, Pospolite.View.Drawing.Renderer, 
  Pospolite.View.HTML.Basics, Pospolite.View.HTML.Parser, 
  Pospolite.View.HTML.Document, Pospolite.View.DOM.Document, 
  Pospolite.View.Internet, Pospolite.View.Version, Pospolite.View.Threads, 
  Pospolite.View.CSS.Binder, Pospolite.View.CSS.StyleSheet, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pospolite_view', @Register);
end.
