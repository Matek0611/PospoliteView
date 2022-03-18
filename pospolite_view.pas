{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pospolite_view;

{$warn 5023 off : no warning about unused units}
interface

uses
  Pospolite.View.RegisterAll, Pospolite.View.JS.AST.Basics, 
  Pospolite.View.JS.AST.Visitors, Pospolite.View.JS.AST.Interfaces, 
  Pospolite.View.JS.AST.Expressions, Pospolite.View.Drawing.NativeDrawer, 
  Pospolite.View.Controls, Pospolite.View.Popups, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Pospolite.View.RegisterAll', 
    @Pospolite.View.RegisterAll.Register);
end;

initialization
  RegisterPackage('pospolite_view', @Register);
end.
