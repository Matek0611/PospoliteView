unit Pospolite.View.RegisterAll;

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Pospolite.View.Frame;

type
  TPLHTMLViewer = class(TPLHTMLFrame)
  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I pospolite.view.registerall_icon.lrs}
  RegisterComponents('PospoLite', [TPLHTMLViewer]);
end;

end.
