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
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Controls, Graphics, Forms, LCLType, LCLProc, LMessages,
  Pospolite.View.Basics, Pospolite.View.HTML.Basics,
  Pospolite.View.HTML.Events, Pospolite.View.HTML.Document,
  Pospolite.View.HTML.Layout, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Renderer, Pospolite.View.CSS.StyleSheet,
  Pospolite.View.CSS.MediaQuery, Pospolite.View.Threads,
  Pospolite.View.Version;

type

  { TPLHTMLFrame }

  TPLHTMLFrame = class(TPLCustomControl)
  private
    FDocument: TPLHTMLDocument;
    FEventManager: TPLHTMLEventManager;
    FRenderingManager: TPLDrawingRendererManager;
    FStylesManager: TPLCSSStyleSheetManager;
    FPointer: TPLPointF;
    FBuffer: Graphics.TBitmap;
    function GetVersion: TPLString;
  protected
    procedure Paint; override;
    procedure UpdateEnvironment;
    procedure DoOnChangeBounds; override;
    procedure ManagersStop;
    procedure ManagersStart;
    procedure Resize; override;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint
      ): Boolean; override;
    procedure KeyPress(var Key: char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Click; override;
    procedure DblClick; override;
    procedure TripleClick; override;
    procedure QuadClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure EnumObjects(const AProc: TPLNestedHTMLObjectProc; const AObject: TPLHTMLObject);
    procedure ChangeFocus(ATo: TPLHTMLBasicObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Redraw; override;

    function QuerySelector(const AQuery: TPLString): TPLHTMLObject; inline;
    function QuerySelectorAll(const AQuery: TPLString): TPLHTMLObjects; inline;
    function GetElementById(const AId: TPLString): TPLHTMLObject; inline;
    function GetElementsByClassName(const AName: TPLString): TPLHTMLObjects; inline;
    function GetElementsByTagName(const AName: TPLString): TPLHTMLObjects; inline;

    procedure LoadFromLocalFile(const AFileName: TPLString);
    procedure LoadFromURL(const AURL: TPLString);
    procedure LoadFromString(const AText: TPLString);
    procedure SaveToLocalFile(const AFileName: TPLString);
    procedure Reload;
    function IsLoading: TPLBool; inline;

    property Document: TPLHTMLDocument read FDocument;

    property EventManager: TPLHTMLEventManager read FEventManager;
    property RenderingManager: TPLDrawingRendererManager read FRenderingManager;
    property StylesManager: TPLCSSStyleSheetManager read FStylesManager;

    property Version: TPLString read GetVersion;
  end;

implementation

uses variants, dialogs, Pospolite.View.CSS.Declaration;

{ TPLHTMLFrame }

function TPLHTMLFrame.GetVersion: TPLString;
begin
  Result := TPLViewVersion.Name + ' ' + TPLViewVersion.Version + ' by ' + TPLViewVersion.Author;
end;

procedure TPLHTMLFrame.Paint;
begin
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(ClientRect);

  if (csDesigning in ComponentState) then exit;

  Canvas.Draw(0, 0, FBuffer);
end;

procedure TPLHTMLFrame.UpdateEnvironment;
begin
  if not Assigned(FStylesManager) then exit;

  FStylesManager.Environment.DocumentBody := FDocument.Body;
  FStylesManager.Environment.Viewport.Width := Width;
  FStylesManager.Environment.Viewport.Height := Height;
end;

procedure TPLHTMLFrame.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  UpdateEnvironment;
end;

procedure TPLHTMLFrame.ManagersStop;
begin
  FRenderingManager.StopRendering;
  FStylesManager.StopStyling;
  FEventManager.StopEvents;
end;

procedure TPLHTMLFrame.ManagersStart;
begin
  FEventManager.StartEvents;
  FStylesManager.StartStyling;
  FRenderingManager.StartRendering;
end;

procedure TPLHTMLFrame.Resize;
begin
  inherited Resize;

  if csDesigning in ComponentState then begin
    Invalidate;
    exit;
  end;
end;

procedure TPLHTMLFrame.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited WMSetFocus(Message);

  FEventManager.Focused := true;
end;

procedure TPLHTMLFrame.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited WMKillFocus(Message);

  FEventManager.Focused := false;
end;

procedure TPLHTMLFrame.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(X, Y) then begin
      if (obj.State = esNormal) then
        FEventManager.DoEvent(obj, 'mouseenter', [X, Y, ShiftStateToInt(Shift)])
      else
        FEventManager.DoEvent(obj, 'mouseover', [X, Y, ShiftStateToInt(Shift)]);

      obj.State := esHover;
    end else if obj.State = esHover then begin
      FEventManager.DoEvent(obj, 'mouseleave', [X, Y, ShiftStateToInt(Shift)]);
      if not obj.CoordsInObject(X, Y) then
        FEventManager.DoEvent(obj, 'mouseout', [X, Y, ShiftStateToInt(Shift)]);

      obj.State := esNormal;
    end;
  end;

begin
  inherited MouseMove(Shift, X, Y);

  FPointer := TPLPointF.Create(X, Y);
  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.MouseLeave;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.State = esHover then begin
      FEventManager.DoEvent(obj, 'mouseleave', [-1, -1, 0]);
      FEventManager.DoEvent(obj, 'mouseout', [-1, -1, 0]);

      obj.State := esNormal;
    end;
  end;

begin
  inherited MouseLeave;

  FPointer := TPLPointF.Create(-1, -1);
  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(X, Y) then
      FEventManager.DoEvent(obj, 'mousedown', [X, Y, Button, ShiftStateToInt(Shift)]);
  end;

begin
  inherited MouseDown(Button, Shift, X, Y);

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(X, Y) then
      FEventManager.DoEvent(obj, 'mouseup', [X, Y, Button, ShiftStateToInt(Shift)]);
  end;

begin
  inherited MouseUp(Button, Shift, X, Y);

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

function TPLHTMLFrame.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(MousePos.X, MousePos.Y) then
      FEventManager.DoEvent(obj, 'mousewheel', [MousePos.X, MousePos.Y, ShiftStateToInt(Shift), WheelDelta]);
  end;

begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);

  if Assigned(FEventManager.FocusedElement) then
    FEventManager.DoEvent(FEventManager.FocusedElement, 'keypress', [Key]);
end;

procedure TPLHTMLFrame.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if Assigned(FEventManager.FocusedElement) then
    FEventManager.DoEvent(FEventManager.FocusedElement, 'keydown', [Key, ShiftStateToInt(Shift)]);
end;

procedure TPLHTMLFrame.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  if Assigned(FEventManager.FocusedElement) then
    FEventManager.DoEvent(FEventManager.FocusedElement, 'keyup', [Key, ShiftStateToInt(Shift)]);
end;

procedure TPLHTMLFrame.Click;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(FPointer.X, FPointer.Y) then
      FEventManager.DoEvent(obj, 'click', [FPointer.X, FPointer.Y, 1]);
  end;

begin
  inherited Click;

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.DblClick;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(FPointer.X, FPointer.Y) then
      FEventManager.DoEvent(obj, 'dblclick', [FPointer.X, FPointer.Y]);
  end;

begin
  inherited DblClick;

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.TripleClick;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(FPointer.X, FPointer.Y) then begin
      FEventManager.DoEvent(obj, 'tripleclick', [FPointer.X, FPointer.Y]);
      //FEventManager.DoEvent(obj, 'click', [FPointer.X, FPointer.Y, 3]);
    end;
  end;

begin
  inherited TripleClick;

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.QuadClick;

  procedure AnalyzeProc(obj: TPLHTMLObject);
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(FPointer.X, FPointer.Y) then begin
      FEventManager.DoEvent(obj, 'quadclick', [FPointer.X, FPointer.Y]);
      //FEventManager.DoEvent(obj, 'click', [FPointer.X, FPointer.Y, 4]);
    end;
  end;

begin
  inherited QuadClick;

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.DoContextPopup(MousePos: TPoint; var Handled: Boolean);

  procedure AnalyzeProc(obj: TPLHTMLObject);
  var
    ls: TPLHTMLEventListeners = nil;
  begin
    if not Assigned(obj) then exit;

    if obj.CoordsInObjectOnly(MousePos.X, MousePos.Y) then begin
      if not Handled then begin
        ls := TPLHTMLBasicObject(obj).EventTarget.GetEventListeners('contextmenu');
        if Assigned(ls) and (ls.Count > 1) then Handled := true;
      end;

      FEventManager.DoEvent(obj, 'contextmenu', [MousePos.X, MousePos.Y]);
    end;
  end;

begin
  inherited DoContextPopup(MousePos, Handled);

  EnumObjects(@AnalyzeProc, FDocument.Body);
end;

procedure TPLHTMLFrame.EnumObjects(const AProc: TPLNestedHTMLObjectProc;
  const AObject: TPLHTMLObject);

  procedure EnumChildren(obj: TPLHTMLObject);
  var
    ch: TPLHTMLObject;
  begin
    if not Assigned(obj) then exit;

    AProc(obj);

    for ch in obj.Children do
      EnumChildren(ch);
  end;

begin
  EnumChildren(AObject);
end;

procedure TPLHTMLFrame.ChangeFocus(ATo: TPLHTMLBasicObject);
begin
  if Assigned(FEventManager.FocusedElement) then
    FEventManager.DoEvent(FEventManager.FocusedElement, 'blur', []);

  FEventManager.FocusedElement := ATo;

  if Assigned(ATo) then
    FEventManager.DoEvent(ATo, 'focus', []);
end;

constructor TPLHTMLFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csClickEvents, csTripleClicks, csQuadClicks,
    csReplicatable] - [csAcceptsControls, csNoFocus, csNoStdEvents];
  Parent := AOwner as TWinControl;
  TabStop := true;
  DoubleBuffered := true;

  FBuffer := Graphics.TBitmap.Create;
  FBuffer.PixelFormat := pf32bit;
  FBuffer.SetSize(Width, Height);

  FDocument := TPLHTMLDocument.Create;
  FPointer := TPLPointF.Create(-1, -1);

  FEventManager := TPLHTMLEventManager.Create;
  FStylesManager := TPLCSSStyleSheetManager.Create(FDocument);
  FStylesManager.Environment := TPLCSSMediaQueriesEnvironment.Create(0, 0);
  if not (AOwner is TCustomForm) then AOwner := AOwner.Owner;
  FStylesManager.Environment.Hook.Hook := AOwner as TCustomForm;
end;

destructor TPLHTMLFrame.Destroy;
begin
  ManagersStop;

  FEventManager.Free;
  FreeAndNil(FRenderingManager);
  FStylesManager.Free;

  FDocument.Free;

  FBuffer.Free;

  inherited Destroy;
end;

procedure TPLHTMLFrame.AfterConstruction;
begin
  inherited AfterConstruction;

  FRenderingManager := TPLDrawingRendererManager.Create(self);
end;

procedure TPLHTMLFrame.Redraw;
var
  dr: TPLDrawingRenderer;
begin
  try
    if (Width <> FBuffer.Width) or (Height <> FBuffer.Height) then
      FBuffer.SetSize(Width, Height);
    FBuffer.Canvas.Brush.Color := clWhite;
    FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);

    //FBuffer.Canvas.TextOut(0, 0, FormatDateTime('hh:nn:ss,zzz', Now)); // fps test

    dr := TPLDrawingRenderer.Create(FBuffer.Canvas);
    try
      dr.Drawer.Surface.Clear(TPLColor.White); // NA RAZIE NAPRAWIONY BUG #D1 DZIĘKI TEJ LINII (#D1: Krytyczny - użycie CPU wzrasta co chwilę o kilka setnych MB mimo, że nie ma wycieków pamięci)

      if Assigned(FDocument) and Assigned(FDocument.Root) then
        FDocument.Root.Draw(dr);

      // test 2.
      dr.DrawBox(TPLRectF.Create(50, 50, 200, 50), TPLCSSDeclarations.Create('border-color: red; background-color: #bbb;'), nil, false, true); // rendering
    finally
      dr.Free;
    end;
  except
    on e: exception do FBuffer.Canvas.TextOut(10, 10, e.Message);
  end;
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

procedure TPLHTMLFrame.LoadFromLocalFile(const AFileName: TPLString);
begin
  if IsLoading then exit;

  ManagersStop;
  FDocument.LoadFromLocalFile(AFileName);
  if FDocument.IsLoaded then ManagersStart;
end;

procedure TPLHTMLFrame.LoadFromURL(const AURL: TPLString);
begin
  if IsLoading then exit;

  ManagersStop;
  FDocument.LoadFromURL(AURL);
  if FDocument.IsLoaded then ManagersStart;
end;

procedure TPLHTMLFrame.LoadFromString(const AText: TPLString);
begin
  if IsLoading then exit;

  ManagersStop;
  FDocument.LoadFromString(AText);
  if FDocument.IsLoaded then ManagersStart;
end;

procedure TPLHTMLFrame.SaveToLocalFile(const AFileName: TPLString);
begin
  if IsLoading then exit;

  FDocument.SaveToLocalFile(AFileName);
end;

procedure TPLHTMLFrame.Reload;
begin
  if IsLoading then exit;

  FDocument.Reload;
end;

function TPLHTMLFrame.IsLoading: TPLBool;
begin
  Result := FDocument.IsLoading;
end;

end.

