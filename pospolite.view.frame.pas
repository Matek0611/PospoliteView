unit Pospolite.View.Frame;

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
  Classes, SysUtils, Controls, Graphics, Forms, Pospolite.View.Basics,
  Pospolite.View.HTML.Events, Pospolite.View.HTML.Document,
  Pospolite.View.HTML.Layout, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Renderer, Pospolite.View.CSS.StyleSheet,
  Pospolite.View.CSS.MediaQuery;

type

  { TPLHTMLFrame }

  TPLHTMLFrame = class(TPLCustomControl)
  private
    FDocument: TPLHTMLDocument;
    FEventManager: TPLHTMLEventManager;
    FRenderingManager: TPLDrawingRendererManager;
    FStylesManager: TPLCSSStyleSheetManager;
  protected
    procedure Paint; override;
    procedure UpdateEnvironment;
    procedure DoOnChangeBounds; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function QuerySelector(const AQuery: TPLString): TPLHTMLObject; inline;
    function QuerySelectorAll(const AQuery: TPLString): TPLHTMLObjects; inline;
    function GetElementById(const AId: TPLString): TPLHTMLObject; inline;
    function GetElementsByClassName(const AName: TPLString): TPLHTMLObjects; inline;
    function GetElementsByTagName(const AName: TPLString): TPLHTMLObjects; inline;

    property Document: TPLHTMLDocument read FDocument;

    property EventManager: TPLHTMLEventManager read FEventManager;
    property RenderingManager: TPLDrawingRendererManager read FRenderingManager;
    property StylesManager: TPLCSSStyleSheetManager read FStylesManager;
  end;

implementation

{ TPLHTMLFrame }

procedure TPLHTMLFrame.Paint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);

  if csDesigning in ComponentState then exit;

  if Assigned(FDocument) and Assigned(FDocument.Root) then
    FDocument.Root.Draw;
end;

procedure TPLHTMLFrame.UpdateEnvironment;
begin
  if not Assigned(FStylesManager) then exit;

  FStylesManager.Environment.DocumentBody := FDocument.querySelector('internal_root_object > html > body');
  FStylesManager.Environment.Viewport.Width := Width;
  FStylesManager.Environment.Viewport.Height := Height;
end;

procedure TPLHTMLFrame.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  UpdateEnvironment;
end;

constructor TPLHTMLFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csTripleClicks, csQuadClicks,
    csReplicatable] - [csAcceptsControls, csNoFocus, csNoStdEvents];

  FDocument := TPLHTMLDocument.Create;
  FDocument.Renderer := TPLDrawingRenderer.Create(Self.Canvas);

  FEventManager := TPLHTMLEventManager.Create;
  FRenderingManager := TPLDrawingRendererManager.Create(self);
  FStylesManager := TPLCSSStyleSheetManager.Create;
  FStylesManager.Environment := TPLCSSMediaQueriesEnvironment.Create(0, 0);
  if AOwner is TCustomForm then FStylesManager.Environment.Hook.Hook := AOwner as TCustomForm;
end;

destructor TPLHTMLFrame.Destroy;
begin
  FDocument.Free;

  FEventManager.StopEvents;
  FEventManager.Free;
  FRenderingManager.StopRendering;
  FRenderingManager.Free;
  FStylesManager.Free;

  inherited Destroy;
end;

function TPLHTMLFrame.QuerySelector(const AQuery: TPLString): TPLHTMLObject;
begin
  Result := FDocument.querySelector(AQuery);
end;

function TPLHTMLFrame.QuerySelectorAll(const AQuery: TPLString): TPLHTMLObjects;
begin
  Result := FDocument.querySelectorAll(AQuery);
end;

function TPLHTMLFrame.GetElementById(const AId: TPLString): TPLHTMLObject;
begin
  Result := FDocument.querySelector('#' + AId);
end;

function TPLHTMLFrame.GetElementsByClassName(const AName: TPLString
  ): TPLHTMLObjects;
begin
  Result := FDocument.querySelectorAll('[class="%s"]'.Format([AName]));
end;

function TPLHTMLFrame.GetElementsByTagName(const AName: TPLString
  ): TPLHTMLObjects;
begin
  Result := FDocument.querySelectorAll(AName);
end;

end.

