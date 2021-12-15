unit Pospolite.View.DOM.Window;

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms;

type

  TPLDOMWindowDisplay = (wdStandalone, wdMinimalUI, wdBrowser);

  { TPLDOMWindow }

  TPLDOMWindow = packed record
  private
    FDisplay: TPLDOMWindowDisplay;
    FHook: TCustomForm;
  public
    constructor Create(AHook: TCustomForm);

    function closed: boolean;

    property Hook: TCustomForm read FHook write FHook;
    property Display: TPLDOMWindowDisplay read FDisplay write FDisplay;
  end;

implementation

{ TPLDOMWindow }

constructor TPLDOMWindow.Create(AHook: TCustomForm);
begin
  Hook := AHook;
  FDisplay := wdStandalone;
end;

function TPLDOMWindow.closed: boolean;
begin
  if Assigned(FHook) then Result := FHook.Visible else Result := false;
end;

end.

