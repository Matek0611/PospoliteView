unit Pospolite.View.Popups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Pospolite.View.Basics,
  Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Renderer;

type

  TPLPopupItemKind = (pikButton, pikDropDown, pikButtonDrop, pikSeparator,
    pikSmallButton, pikLabel);

  { TPLPopupItem }

  TPLPopupItem = class(TCollectionItem)
  private
    FKind: TPLPopupItemKind;
    procedure SetKind(AValue: TPLPopupItemKind);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Kind: TPLPopupItemKind read FKind write SetKind;
  end;

implementation

{ TPLPopupItem }

procedure TPLPopupItem.SetKind(AValue: TPLPopupItemKind);
begin
  if FKind = AValue then exit;
  FKind := AValue;
end;

constructor TPLPopupItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

destructor TPLPopupItem.Destroy;
begin
  inherited Destroy;
end;

end.

