unit Pospolite.View.DOM.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TPLDOMWindow }

  TPLDOMWindow = class
  private
    FHook: TCustomForm;
  public
    // helpers
    property Hook: TCustomForm read FHook write FHook;

    // from js
    function closed: boolean;
  end;

implementation

{ TPLDOMWindow }

function TPLDOMWindow.closed: boolean;
begin
  if Assigned(FHook) then Result := FHook.Visible else Result := false;
end;

end.

