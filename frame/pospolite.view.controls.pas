unit Pospolite.View.Controls;

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
{$macro on}
{.$define tabdebug}

interface

uses
  Classes, SysUtils, Graphics, Controls, dateutils, Forms, Pospolite.View.Basics,
  Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.CSS.Animation, Pospolite.View.Drawing.Drawer,
  Pospolite.View.Threads;

type

  { TPLUIControl }

  TPLUIControl = class(TPLCustomControl)
  private
    FMousePos: TPLPointI;
    FMouseDown: TPLBool;
    FMouseButton: TMouseButton;
    FShiftState: TShiftState;
    FZoom: TPLFloat;
    function GetScale: TPLFloat; inline;
    procedure SetZoom(AValue: TPLFloat);
  protected
    procedure DoDraw(const ARenderer: TPLDrawingRenderer); virtual;
    procedure Paint; override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Redraw; override;
    procedure Rezoom; virtual;

    property MousePosition: TPLPointI read FMousePos;
    property MouseIsDown: TPLBool read FMouseDown;
    property MouseButton: TMouseButton read FMouseButton;
    property ShiftState: TShiftState read FShiftState;

    property Scale: TPLFloat read GetScale;
  published
    property OnClick;
    property OnEnter;
    property OnExit;
    property Zoom: TPLFloat read FZoom write SetZoom;
  end;

  { TPLUIAnimationLoopItem }

  TPLUIAnimationLoopItem = class(TObject)
  public
    Started: TPLBool;
    Finished: TPLBool;
    CurrentPoint: TPLInt;
    Duration: Cardinal;
    StartingValues, TargetValues: array of TPLFloat;
    Variables: array of TPLPFloat;
    TimingFunction: TPLCSSTimingFunction;
    Controller: Pointer;
  public
    constructor Create(const ADuration: Cardinal; const AVariables: array of TPLPFloat;
      const ATargetValues: array of TPLFloat; const ATimingFunction: TPLCSSTimingFunction;
      const AController: Pointer = nil);

    procedure Next;
    procedure Start;
    procedure Disable;

    //class operator =(a, b: TPLUIAnimationLoopItem) r: TPLBool;
  end;

  TPLUIAnimationQueue = class(specialize TPLObjectList<TPLUIAnimationLoopItem>);

  { TPLUIAnimationLoop }

  TPLUIAnimationLoop = class(TPersistent)
  private
    FQueue: TPLUIAnimationQueue;
    FControl: TPLUIControl;
    FThread: TPLAsyncTask;
  protected
    procedure AsyncExecute(const AArguments: array of const);
  public const
    DefaultAnimationDuration = 100;
    AnimationFrames = 1000 div 120;
  public
    constructor Create(const AControl: TPLUIControl);
    destructor Destroy; override;

    procedure Add(const AEvent: TPLUIAnimationLoopItem);
    procedure DisableByController(const AController: Pointer);
    procedure Clear;

    function IsAnimating: TPLBool; inline;
  end;

  TPLUITabsState = (tsNormal, tsHover, tsDown);
  TPLUITabsStateColor = array[TPLUITabsState] of TPLColor;

  { TPLUITabsTheme }

  TPLUITabsTheme = record
  public
    Background: TPLColor;
    Separators: TPLColor;
    Navigation: array[TPLBool] of record // is active?
      Icons: TPLUITabsStateColor;
      Buttons: TPLUITabsStateColor;
      Text: TPLUITabsStateColor;
      Background: TPLUITabsStateColor;
      CloseButtons: record
        Symbol: TPLUITabsStateColor;
        Background: TPLUITabsStateColor;
      end;
    end;
  public
    class function Light: TPLUITabsTheme; static;
    class function Dark: TPLUITabsTheme; static;

    procedure ToDark; inline;
    procedure ToLight; inline;
  end;

  TPLUITab = class;
  TPLUITabsManager = class;
  TPLUITabs = class;

  { TPLUITabList }

  TPLUITabList = class(specialize TPLObjectList<TPLUITab>)
  private
    FTabs: TPLUITabsManager;
  public
    constructor Create(const ATabs: TPLUITabsManager);

    procedure UpdateTabs;
  end;

  { TPLUITab }

  TPLUITab = class(TPersistent)
  private
    FBusy: TPLBool;
    FCaption: TPLString;
    FCloseable: TPLBool;
    FColor: TPLColor;
    FFavIcon: IPLDrawingBitmap;
    FHint: TPLString;
    FList: TPLUITabList;

    procedure SetBusy(AValue: TPLBool);
    procedure SetCaption(AValue: TPLString);
    procedure SetCloseable(AValue: TPLBool);
    procedure SetColor(AValue: TPLColor);
    procedure SetFavIcon(AValue: IPLDrawingBitmap);
  protected
    FState: TPLUITabsState;
    FRect: TPLRectF;
    FPinned: TPLBool;
    FActive: TPLBool;

    function CloseButtonRect: TPLRectF; inline;
    function Theme: TPLUITabsTheme; inline;
  public
    constructor Create(const AList: TPLUITabList);
    destructor Destroy; override;

    procedure Exchange(const AList: TPLUITabList; const AMoveLast: TPLBool = true); // for pin/unpin
    procedure Draw(const ARenderer: TPLDrawingRenderer);
    function MouseInTab(const X, Y: TPLFloat): TPLBool; inline;
    function MouseInCloseButton(const X, Y: TPLFloat): TPLBool; inline;
    procedure Close;

    property Caption: TPLString read FCaption write SetCaption;
    property Active: TPLBool read FActive;
    property Pinned: TPLBool read FPinned;
    property Color: TPLColor read FColor write SetColor;
    property FavIcon: IPLDrawingBitmap read FFavIcon write SetFavIcon;
    property Hint: TPLString read FHint write FHint;
    property Busy: TPLBool read FBusy write SetBusy;
    property Closeable: TPLBool read FCloseable write SetCloseable;
  end;

  { TPLUITabsManager }

  TPLUITabsManager = class(TPersistent)
  private
    FActiveTabIndex: SizeInt;
    FAnimate: TPLBool;
    FAnimations: TPLUIAnimationLoop;
    FTabs: TPLUITabs;
    FNormalTabs: TPLUITabList;
    FPinnedTabs: TPLUITabList;

    function GetActiveTab: TPLUITab;
    procedure SetActiveTabIndex(AValue: SizeInt);
  protected
    FTabsRect, FScrollLeftRect, FScrollRightRect, FViewTabsRect, FNewTabRect: TPLRectF;
    FScrolling: TPLBool;
    FScrollPos: TPLInt;

    procedure MoveTabRect(const ATab: TPLUITab; const ARect: TPLRectF);
    function TileSize: TPLFloat; inline;
    function TileBtnSize: TPLFloat; inline;
    function TileMargin: TPLFloat; inline;
  public const
    DefaultTabWidth = 220;
    MinTabWidth = 72;
  public
    constructor Create(const ATabs: TPLUITabs);
    destructor Destroy; override;

    function AddTab: TPLUITab;
    procedure RemoveTab(AIndex: SizeInt);
    procedure ChangeTabPinning(const ATab: TPLUITab);
    procedure UpdateTabs;
    function GetTab(AIndex: SizeInt): TPLUITab;

    property PinnedTabs: TPLUITabList read FPinnedTabs;
    property NormalTabs: TPLUITabList read FNormalTabs;
    property Animations: TPLUIAnimationLoop read FAnimations;
    property ActiveTabIndex: SizeInt read FActiveTabIndex write SetActiveTabIndex;
    property ActiveTab: TPLUITab read GetActiveTab;
    property Animate: TPLBool read FAnimate write FAnimate;
  end;

  { TPLUITabs }

  TPLUITabs = class(TPLUIControl)
  private
    FManager: TPLUITabsManager;
    FTheme: TPLUITabsTheme;
    FNotifyResize: TPLBool;
    FHoverID: SizeInt;

    function GetAlign: TAlign;
    procedure SetTheme(AValue: TPLUITabsTheme);
  protected
    procedure DoDraw(const ARenderer: TPLDrawingRenderer); override;
    procedure Resize; override;
    procedure Click; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Redraw; override;

    procedure UpdateTabs;
    function AddTab: TPLUITab; inline;

    property Theme: TPLUITabsTheme read FTheme write SetTheme;
  published
    property Align: TAlign read GetAlign;
    property Manager: TPLUITabsManager read FManager;
  end;

implementation

uses Dialogs, math, strutils;

{ TPLUIControl }

procedure TPLUIControl.SetZoom(AValue: TPLFloat);
begin
  if FZoom = AValue then exit;
  FZoom := AValue;

  Rezoom;
end;

function TPLUIControl.GetScale: TPLFloat;
begin
  Result := FZoom * (Screen.PixelsPerInch / 96);
end;

procedure TPLUIControl.DoDraw(const ARenderer: TPLDrawingRenderer);
begin
  // ...
end;

procedure TPLUIControl.Paint;
var
  dr: TPLDrawingRenderer;
begin
  inherited Paint;
  if csDesigning in ComponentState then exit;

  dr := TPLDrawingRenderer.Create(Canvas);
  try
    DoDraw(dr);
  finally
    dr.Free;
  end;
end;

procedure TPLUIControl.MouseEnter;
begin
  inherited MouseEnter;

  Redraw;
end;

procedure TPLUIControl.MouseLeave;
begin
  inherited MouseLeave;

  FMousePos := TPLPointI.Create(-1, -1);
  Redraw;
end;

procedure TPLUIControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FMousePos := TPLPointI.Create(X, Y);
  FShiftState := Shift;
  Redraw;
end;

procedure TPLUIControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMousePos := TPLPointI.Create(X, Y);
  FMouseDown := true;
  FMouseButton := Button;
  FShiftState := Shift;
  Redraw;
end;

procedure TPLUIControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMousePos := TPLPointI.Create(X, Y);
  FMouseDown := false;
  FMouseButton := Button;
  FShiftState := Shift;
  Redraw;
end;

constructor TPLUIControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csOpaque] + [csReplicatable, csParentBackground];

  FMousePos := TPLPointI.Create(-1, -1);
  FMouseDown := false;
  FMouseButton := mbLeft;
  FShiftState := [];
  FZoom := 1;
end;

destructor TPLUIControl.Destroy;
begin
  inherited Destroy;
end;

procedure TPLUIControl.Redraw;
begin
  Refresh;
end;

procedure TPLUIControl.Rezoom;
begin
  //
end;

{ TPLUIControlLoopItem }

constructor TPLUIAnimationLoopItem.Create(const ADuration: Cardinal;
  const AVariables: array of TPLPFloat; const ATargetValues: array of TPLFloat;
  const ATimingFunction: TPLCSSTimingFunction; const AController: Pointer);
var
  i: SizeInt;
begin
  Started := false;
  Finished := false;
  Duration := ADuration;
  TimingFunction := ATimingFunction;
  Controller := AController;

  SetLength(Variables, Length(AVariables));
  SetLength(StartingValues, Length(AVariables));
  SetLength(TargetValues, Length(AVariables));

  for i := Low(Variables) to High(Variables) do begin
    Variables[i] := AVariables[i];
    StartingValues[i] := AVariables[i]^;
    TargetValues[i] := ATargetValues[i];
  end;
end;

procedure TPLUIAnimationLoopItem.Next;
var
  i: SizeInt;
begin
  if Finished then exit;

  if not Started then Start;

  CurrentPoint += TPLUIAnimationLoop.AnimationFrames;
  Finished := CurrentPoint > Duration;
  if CurrentPoint > Duration then CurrentPoint := Duration;

  if Finished then begin
    Started := false;
    for i := Low(Variables) to High(Variables) do
      Variables[i]^ := TargetValues[i];
  end;

  if not Finished then
  for i := Low(Variables) to High(Variables) do
    Variables[i]^ := StartingValues[i] + (TargetValues[i] - StartingValues[i]) * TimingFunction(CurrentPoint / Duration, []).Y;
end;

procedure TPLUIAnimationLoopItem.Start;
begin
  Started := true;
  Finished := false;

  CurrentPoint := -TPLUIAnimationLoop.AnimationFrames;
end;

procedure TPLUIAnimationLoopItem.Disable;
begin
  Finished := true;
end;

//class operator TPLUIAnimationLoopItem.=(a, b: TPLUIAnimationLoopItem) r: TPLBool;
//var
//  i: SizeInt;
//begin
//  r := (a.Started = b.Started) and (a.Finished = b.Finished) and
//    (a.Duration = b.Duration) and (a.TimingFunction = b.TimingFunction) and
//    (Length(a.TargetValues) = Length(b.TargetValues)) and (Length(a.Variables) = Length(b.Variables));
//
//  if r then
//  for i := Low(a.Variables) to High(a.Variables) do begin
//    r := r and (a.Variables[i] = b.Variables[i]);
//    if not r then break;
//  end;
//end;

{ TPLUIAnimationLoop }

procedure TPLUIAnimationLoop.AsyncExecute(const AArguments: array of const);
var
  item: TPLUIAnimationLoopItem;
begin
  while not FQueue.Empty do begin
    for item in FQueue do begin
      if item.Finished then FQueue.Remove(item)
      else item.Next;
    end;

    if Assigned(FControl) then FControl.Repaint;

    FThread.Sleep(AnimationFrames);
  end;
end;

constructor TPLUIAnimationLoop.Create(const AControl: TPLUIControl);
begin
  inherited Create;

  FControl := AControl;
  FQueue := TPLUIAnimationQueue.Create;
  FThread := nil;
end;

destructor TPLUIAnimationLoop.Destroy;
begin
  if Assigned(FThread) then FThread.Cancel;
  FThread.Free;
  Clear;
  FQueue.Free;

  inherited Destroy;
end;

procedure TPLUIAnimationLoop.Add(const AEvent: TPLUIAnimationLoopItem);
begin
  FQueue.Add(AEvent);

  if Assigned(FThread) then begin
    FThread.Cancel;
    FThread.Free;
    FThread := nil;
  end;

  FThread := TPLAsyncTask.Create(@AsyncExecute, false);
  FThread.Async([]);
end;

procedure TPLUIAnimationLoop.DisableByController(const AController: Pointer);
var
  i: SizeInt;
begin
  for i := 0 to FQueue.Count-1 do begin
    if FQueue[i].Controller = AController then
      FQueue[i].Disable;
  end;
end;

procedure TPLUIAnimationLoop.Clear;
begin
  FQueue.Clear;
end;

function TPLUIAnimationLoop.IsAnimating: TPLBool;
begin
  if not Assigned(FThread) then exit(false);
  Result := FThread.IsRunning and not (FThread.IsCancelled or FThread.IsFailed);
end;

{ TPLUITabsTheme }

class function TPLUITabsTheme.Light: TPLUITabsTheme;
begin
  with Result do begin
    Background := '#f0f0f0';
    Separators := '#cdcdcd';

    with Navigation[false] do begin
      Icons[tsNormal] := '#767676';
      Icons[tsHover] := '#767676';
      Icons[tsDown] := '#767676';

      Buttons[tsNormal] := TPLColor.Transparent;
      Buttons[tsHover] := '#dadae0';
      Buttons[tsDown] := '#cfcfd8';

      Text[tsNormal] := '#484848';
      Text[tsHover] := '#484848';
      Text[tsDown] := '#484848';

      Background[tsNormal] := '#f0f0f0';
      Background[tsHover] := '#d6d6d6';
      Background[tsDown] := '#c0c0c0';

      with CloseButtons do begin
        Symbol[tsNormal] := '#333333';
        Symbol[tsHover] := '#333333';
        Symbol[tsDown] := '#333333';

        Background[tsNormal] := '#f0f0f0';
        Background[tsHover] := '#c0c0c0';
        Background[tsDown] := '#cccccc';
      end;
    end;

    with Navigation[true] do begin
      Icons[tsNormal] := '#767676';
      Icons[tsHover] := '#767676';
      Icons[tsDown] := '#767676';

      Buttons[tsNormal] := TPLColor.Transparent;
      Buttons[tsHover] := '#dadae0';
      Buttons[tsDown] := '#cfcfd8';

      Text[tsNormal] := '#484848';
      Text[tsHover] := '#484848';
      Text[tsDown] := '#484848';

      Background[tsNormal] := '#ffffff';
      Background[tsHover] := '#e9e9e9';
      Background[tsDown] := '#e0e0e0';

      with CloseButtons do begin
        Symbol[tsNormal] := '#333333';
        Symbol[tsHover] := '#333333';
        Symbol[tsDown] := '#333333';

        Background[tsNormal] := '#f0f0f0';
        Background[tsHover] := '#c0c0c0';
        Background[tsDown] := '#cccccc';
      end;
    end;
  end;
end;

class function TPLUITabsTheme.Dark: TPLUITabsTheme;
begin
  with Result do begin
    Background := '#1c1b22';
    Separators := '#111111';

    with Navigation[false] do begin
      Icons[tsNormal] := TPLColor.White;
      Icons[tsHover] := TPLColor.White;
      Icons[tsDown] := TPLColor.White;

      Buttons[tsNormal] := TPLColor.Transparent;
      Buttons[tsHover] := '#525252';
      Buttons[tsDown] := '#5b5b5b';

      Text[tsNormal] := TPLColor.White;
      Text[tsHover] := TPLColor.White;
      Text[tsDown] := TPLColor.White;

      Background[tsNormal] := '#1c1b22';
      Background[tsHover] := '#353535';
      Background[tsDown] := '#4a494e';

      with CloseButtons do begin
        Symbol[tsNormal] := TPLColor.White;
        Symbol[tsHover] := TPLColor.White;
        Symbol[tsDown] := TPLColor.White;

        Background[tsNormal] := '#1c1b22';
        Background[tsHover] := '#4a494e';
        Background[tsDown] := '#5c5c61';
      end;
    end;

    with Navigation[true] do begin
      Icons[tsNormal] := TPLColor.White;
      Icons[tsHover] := TPLColor.White;
      Icons[tsDown] := TPLColor.White;

      Buttons[tsNormal] := TPLColor.Transparent;
      Buttons[tsHover] := '#525252';
      Buttons[tsDown] := '#5b5b5b';

      Text[tsNormal] := TPLColor.White;
      Text[tsHover] := TPLColor.White;
      Text[tsDown] := TPLColor.White;

      Background[tsNormal] := '#42414d';
      Background[tsHover] := '#4e4d59';
      Background[tsDown] := '#3b3a46';

      with CloseButtons do begin
        Symbol[tsNormal] := TPLColor.White;
        Symbol[tsHover] := TPLColor.White;
        Symbol[tsDown] := TPLColor.White;

        Background[tsNormal] := '#42414d';
        Background[tsHover] := '#55545f';
        Background[tsDown] := '#676671';
      end;
    end;
  end;
end;

procedure TPLUITabsTheme.ToDark;
begin
  Self := Dark;
end;

procedure TPLUITabsTheme.ToLight;
begin
  Self := Light;
end;

{ TPLUITabList }

constructor TPLUITabList.Create(const ATabs: TPLUITabsManager);
begin
  inherited Create(true);

  FTabs := ATabs;
end;

procedure TPLUITabList.UpdateTabs;
begin
  FTabs.UpdateTabs;
end;

{ TPLUITab }

procedure TPLUITab.SetCaption(AValue: TPLString);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;

  FList.UpdateTabs;
end;

procedure TPLUITab.SetCloseable(AValue: TPLBool);
begin
  if FCloseable = AValue then exit;
  FCloseable := AValue;

  FList.UpdateTabs;
end;

procedure TPLUITab.SetColor(AValue: TPLColor);
begin
  if FColor = AValue then exit;
  FColor := AValue;

  FList.UpdateTabs;
end;

procedure TPLUITab.SetFavIcon(AValue: IPLDrawingBitmap);
begin
  if FFavIcon = AValue then exit;
  FFavIcon := AValue;

  FList.UpdateTabs;
end;

function TPLUITab.CloseButtonRect: TPLRectF;
var
  xsize, xmove: TPLFloat;
begin
  if FPinned then exit(TPLRectF.Empty);

  xsize := FRect.Height / 2;
  xmove := xsize / 2;

  Result := TPLRectF.Create(FRect.Right - xsize - xmove, FRect.Top + xmove, xsize, xsize);
end;

function TPLUITab.Theme: TPLUITabsTheme;
begin
  Result := FList.FTabs.FTabs.Theme;
end;

procedure TPLUITab.SetBusy(AValue: TPLBool);
begin
  if FBusy = AValue then exit;
  FBusy := AValue;

  FList.UpdateTabs;
end;

constructor TPLUITab.Create(const AList: TPLUITabList);
begin
  inherited Create;

  FState := tsNormal;
  FRect := TPLRectF.Empty;

  FList := AList;
  FCaption := 'Tab';
  FActive := false;
  FPinned := false;
  FFavIcon := nil;
  FHint := '';
  FBusy := false;
  FCloseable := true;
  FColor := TPLColor.Transparent;
end;

destructor TPLUITab.Destroy;
begin
  inherited Destroy;
end;

procedure TPLUITab.Exchange(const AList: TPLUITabList; const AMoveLast: TPLBool
  );
begin
  FList.FreeObjects := false;
  FList.Remove(Self);
  FList.FreeObjects := true;

  FList := AList;
  if AMoveLast then FList.Add(Self) else FList.Insert(0, Self);

  FList.UpdateTabs;
end;

procedure TPLUITab.Draw(const ARenderer: TPLDrawingRenderer);
var
  r, rt: TPLRectF;
  s: TPLFloat;
  fd: TPLDrawingFontData;
begin
  r := FRect.Inflate(FList.FTabs.TileMargin, FList.FTabs.TileMargin);
  if FActive then r.Height := r.Height + FList.FTabs.TileMargin - 0.5;

  if FPinned then begin

  end else begin
    if FActive then begin
      s := r.Height / 2;
      rt := TPLRectF.Create(r.Left - r.Height, r.Top, r.Height, r.Height);
      ARenderer.Drawer.Surface.MoveTo(0, r.Bottom);
      ARenderer.Drawer.Surface.LineTo(r.Left - s, r.Bottom);
      ARenderer.Drawer.Surface.ArcTo(rt, pi, pi/2);
      rt.Left := r.Left;
      ARenderer.Drawer.Surface.ArcTo(rt, 3/2*pi, 2*pi);
      ARenderer.Drawer.Surface.LineTo(r.Right - s, r.Top);
      rt.Left := r.Right - r.Height;
      ARenderer.Drawer.Surface.ArcTo(rt, 0, pi/2);
      rt.Left := r.Right;
      ARenderer.Drawer.Surface.ArcTo(rt, 3/2*pi, pi);
      ARenderer.Drawer.Surface.LineTo(FList.FTabs.FTabs.Width, r.Bottom);

      ARenderer.Drawer.Surface.Fill(ARenderer.NewBrushSolid(Theme.Navigation[true].Background[FState]), true);
      ARenderer.Drawer.Surface.Stroke(ARenderer.NewPen(ifthen(FColor <> TPLColor.Transparent, FColor.ToString(), Theme.Separators.ToString()), 1.5));
    end else begin
      ARenderer.Drawer.Surface.RoundRectangle(r, r.Height / 2);
      ARenderer.Drawer.Surface.Fill(ARenderer.NewBrushSolid(Theme.Navigation[false].Background[FState]));
    end;
  end;

  fd := PLDrawingFontDataDef;
  fd.Quality := fqAntialiased;
  fd.Color := Theme.Navigation[FActive].Text[FState];
  ARenderer.Drawer.Surface.TextOut(ARenderer.NewFont(fd), FCaption, FRect, TPLTextDirections.Create(tdLeft, tdCenter));

  {$ifdef tabdebug}
  ARenderer.Drawer.Surface.Rectangle(r);
  ARenderer.Drawer.Surface.Stroke(ARenderer.NewPen(ifthen(FActive, '#ff000055', '#00ff0055')));
  {$endif}
end;

function TPLUITab.MouseInTab(const X, Y: TPLFloat): TPLBool;
begin
  Result := TPLPointF.Create(X, Y) in FRect;
end;

function TPLUITab.MouseInCloseButton(const X, Y: TPLFloat): TPLBool;
begin
  Result := TPLPointF.Create(X, Y) in CloseButtonRect;
end;

procedure TPLUITab.Close;
begin
  FList.FreeObjects := false;
  FList.Remove(Self);
  FList.FreeObjects := true;

  FList.UpdateTabs;
  Free;
end;

{ TPLUITabsManager }

function TPLUITabsManager.GetActiveTab: TPLUITab;
begin
  if (FActiveTabIndex < 0) or (FActiveTabIndex > FPinnedTabs.Count + FNormalTabs.Count) then exit(nil);

  Result := nil;

  if FActiveTabIndex < FPinnedTabs.Count then
    Result := FPinnedTabs[FActiveTabIndex]
  else begin
    FActiveTabIndex -= FPinnedTabs.Count;
    if FActiveTabIndex < FNormalTabs.Count then
      Result := FNormalTabs[FActiveTabIndex];
    FActiveTabIndex += FPinnedTabs.Count;
  end;
end;

procedure TPLUITabsManager.SetActiveTabIndex(AValue: SizeInt);
var
  tab: TPLUITab;
begin
  tab := ActiveTab;
  if Assigned(tab) then tab.FActive := true;

  if FActiveTabIndex = AValue then exit;

  tab := ActiveTab;
  if Assigned(tab) then tab.FActive := false;

  FActiveTabIndex := AValue;

  tab := ActiveTab;
  if Assigned(tab) then tab.FActive := true;

  // scroll pos update

  UpdateTabs;
end;

procedure TPLUITabsManager.MoveTabRect(const ATab: TPLUITab;
  const ARect: TPLRectF);
begin
  if FTabs.IsResizing or FTabs.FNotifyResize or not FAnimate then begin
    ATab.FRect := ARect;
  end else begin
    FAnimations.DisableByController(ATab);
    FAnimations.Add(TPLUIAnimationLoopItem.Create(
      TPLUIAnimationLoop.DefaultAnimationDuration, ATab.FRect.PointsPointers,
      ARect.Points, TPLCSSTimingFunctions.GetFunction('ease-in'), ATab
    ));
  end;
end;

function TPLUITabsManager.TileSize: TPLFloat;
begin
  Result := FTabs.Height;
end;

function TPLUITabsManager.TileBtnSize: TPLFloat;
begin
  Result := FTabs.Height * 0.9;
end;

function TPLUITabsManager.TileMargin: TPLFloat;
begin
  Result := TileSize - TileBtnSize;
end;

constructor TPLUITabsManager.Create(const ATabs: TPLUITabs);
begin
  inherited Create;

  FTabs := ATabs;
  FNormalTabs := TPLUITabList.Create(Self);
  FPinnedTabs := TPLUITabList.Create(Self);
  FAnimations := TPLUIAnimationLoop.Create(ATabs);

  FScrolling := false;
  FScrollPos := 0;

  FActiveTabIndex := 0;

  FAnimate := false;
end;

destructor TPLUITabsManager.Destroy;
begin
  FAnimations.Free;
  FNormalTabs.Free;
  FPinnedTabs.Free;

  inherited Destroy;
end;

function TPLUITabsManager.AddTab: TPLUITab;
begin
  Result := TPLUITab.Create(FNormalTabs);
  Result.FRect := TPLRectF.Create(FTabsRect.Left, 0, 0, FTabsRect.Height);
  if not FPinnedTabs.Empty then Result.FRect.Left := FPinnedTabs.Last.FRect.Right;
  if not FNormalTabs.Empty then Result.FRect.Left := FNormalTabs.Last.FRect.Right;

  FNormalTabs.Add(Result);
  ActiveTabIndex := FPinnedTabs.Count + FNormalTabs.Count-1;

  UpdateTabs;
end;

procedure TPLUITabsManager.RemoveTab(AIndex: SizeInt);
var
  tab: TPLUITab;
begin
  tab := GetTab(AIndex);
  if not Assigned(tab) or not tab.Closeable then exit;

  if AIndex >= FPinnedTabs.Count then FNormalTabs.Remove(tab)
  else FPinnedTabs.Remove(tab);

  ActiveTabIndex := ifthen(ActiveTabIndex-1 < 0, 0, ActiveTabIndex-1);

  UpdateTabs;
end;

procedure TPLUITabsManager.ChangeTabPinning(const ATab: TPLUITab);
begin

end;

procedure TPLUITabsManager.UpdateTabs;
var
  tw, xw: TPLFloat;
  i: SizeInt;
  r: TPLRectF;
begin
  FViewTabsRect := TPLRectF.Create(FTabs.Width - TileSize, 0, TileSize, TileSize);
  FNewTabRect := TPLRectF.Create(FViewTabsRect.Left - TileSize, 0, TileSize, TileSize);
  FScrollLeftRect := TPLRectF.Create(0, 0, TileSize, TileSize);
  FScrollRightRect := TPLRectF.Create(FNewTabRect.Left - TileSize, 0, TileSize, TileSize);

  FTabsRect := TPLRectF.Create(FScrollLeftRect.Right, 0, FScrollRightRect.Left - FScrollLeftRect.Right, TileSize);
  FScrolling := (TileSize * FPinnedTabs.Count + DefaultTabWidth * FNormalTabs.Count) * FTabs.Scale > FTabsRect.Width;

  if FNormalTabs.Count > 0 then begin
    tw := (FTabsRect.Width - TileSize * FPinnedTabs.Count * FTabs.Scale) / FNormalTabs.Count;
    if tw < MinTabWidth * FTabs.Scale then tw := MinTabWidth * FTabs.Scale;
    if tw > DefaultTabWidth * FTabs.Scale then tw := DefaultTabWidth * FTabs.Scale;
  end else tw := DefaultTabWidth * FTabs.Scale;

  if FScrolling then begin

  end else begin
    FScrollPos := 0;
  end;

  xw := FScrollLeftRect.Right;
  r := TPLRectF.Create(FTabsRect.Left - FScrollPos, FTabsRect.Top, 0, FTabsRect.Height);
  for i := 0 to FPinnedTabs.Count-1 do begin
    r := TPLRectF.Create(r.Right, r.Top, TileSize * FTabs.Scale, r.Height);
    MoveTabRect(FPinnedTabs[i], r);
  end;
  if FNormalTabs.Empty then xw := r.Right+1;
  for i := 0 to FNormalTabs.Count-1 do begin
    r := TPLRectF.Create(r.Right, r.Top, tw, r.Height);
    MoveTabRect(FNormalTabs[i], r);
    if i = FNormalTabs.Count-1 then xw := r.Right+1;
  end;

  if not FScrolling then FNewTabRect.Left := xw;
end;

function TPLUITabsManager.GetTab(AIndex: SizeInt): TPLUITab;
begin
  if (AIndex < 0) or (AIndex > FPinnedTabs.Count + FNormalTabs.Count) then exit(nil);

  Result := nil;

  if AIndex < FPinnedTabs.Count then
    Result := FPinnedTabs[AIndex]
  else begin
    AIndex -= FPinnedTabs.Count;
    if AIndex < FNormalTabs.Count then
      Result := FNormalTabs[AIndex];
  end;
end;

{ TPLUITabs }

procedure TPLUITabs.SetTheme(AValue: TPLUITabsTheme);
begin
  FTheme := AValue;
  FManager.UpdateTabs;
end;

function TPLUITabs.GetAlign: TAlign;
begin
  Result := alTop;
end;

procedure TPLUITabs.DoDraw(const ARenderer: TPLDrawingRenderer);
var
  tab: TPLUITab;
  st: TPLUITabsState;
  r: TPLRectF;
begin
  // tło
  with ARenderer.Drawer.Surface do begin
    Rectangle(Self.ClientRect);
    Fill(ARenderer.NewBrushSolid(FTheme.Background));
  end;

  // karty
  for tab in FManager.FPinnedTabs do tab.Draw(ARenderer);
  for tab in FManager.FNormalTabs do tab.Draw(ARenderer);

  // przewijanie


  // nowa karta
  with ARenderer.Drawer.Surface do begin
    if FMousePos in FManager.FNewTabRect then begin
      if FMouseDown then st := tsDown else st := tsHover;
    end else st := tsNormal;

    r := FManager.FNewTabRect.Inflate(FManager.TileMargin, FManager.TileMargin);

    Ellipse(r);
    Fill(ARenderer.NewBrushSolid(FTheme.Navigation[false].Buttons[st]));

    r := r.Inflate(FManager.TileMargin * 2.5, FManager.TileMargin * 2.5);

    MoveTo(r.Middle.X, r.Top);
    LineTo(r.Middle.x, r.Bottom);
    Stroke(ARenderer.NewPen(FTheme.Navigation[false].Icons[st], Scale * 1.5));
    MoveTo(r.Left, r.Middle.Y);
    LineTo(r.Right, r.Middle.Y);
    Stroke(ARenderer.NewPen(FTheme.Navigation[false].Icons[st], Scale * 1.5));
  end;

  // lista kart

end;

procedure TPLUITabs.Resize;
begin
  inherited Resize;

  FNotifyResize := true;
  FManager.UpdateTabs;
  FNotifyResize := false;
end;

procedure TPLUITabs.Click;
var
  tab: TPLUITab;
begin
  inherited Click;

  if FMousePos in FManager.FNewTabRect then FManager.AddTab else begin
    tab := FManager.GetTab(FHoverID);
    if Assigned(tab) then begin
      FManager.ActiveTabIndex := FHoverID;
      // on tab click

    end;
  end;

  Redraw;
end;

procedure TPLUITabs.MouseLeave;
var
  tab: TPLUITab;
begin
  inherited MouseLeave;

  tab := FManager.GetTab(FHoverID);
  if Assigned(tab) then
    tab.FState := tsNormal;

  FHoverID := -1;

  Redraw;
end;

procedure TPLUITabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, bi: SizeInt;
  tab: TPLUITab;
begin
  inherited MouseMove(Shift, X, Y);

  bi := FHoverID;
  FHoverID := -1;

  for i := 0 to FManager.PinnedTabs.Count-1 do begin
    if FMousePos in FManager.PinnedTabs[i].FRect then begin
      FHoverID := i;
      break;
    end;
  end;
  if FHoverID = -1 then begin
    for i := 0 to FManager.NormalTabs.Count-1 do begin
      if FMousePos in FManager.NormalTabs[i].FRect then begin
        FHoverID := FManager.PinnedTabs.Count + i;
        break;
      end;
    end;
  end;

  if bi <> FHoverID then begin
    tab := FManager.GetTab(bi);
    if Assigned(tab) then tab.FState := tsNormal;
    tab := FManager.GetTab(FHoverID);
    if Assigned(tab) then begin
      if FMouseDown then tab.FState := tsDown else tab.FState := tsHover;
    end;
  end;
end;

procedure TPLUITabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  tab: TPLUITab;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbMiddle then FManager.RemoveTab(FHoverID);

  if Button <> mbLeft then exit;

  tab := FManager.GetTab(FHoverID);
  if Assigned(tab) then begin
    if FMouseDown then tab.FState := tsDown else tab.FState := tsHover;
    Redraw;
  end;
end;

procedure TPLUITabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  tab: TPLUITab;
begin
  inherited MouseUp(Button, Shift, X, Y);

  tab := FManager.GetTab(FHoverID);
  if Assigned(tab) then begin
    if FMouseDown then tab.FState := tsDown else tab.FState := tsHover;
    Redraw;
  end;
end;

constructor TPLUITabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FManager := TPLUITabsManager.Create(Self);
  FTheme := TPLUITabsTheme.Light;

  FManager.UpdateTabs;

  inherited Align := alTop;
  Height := 44;
end;

destructor TPLUITabs.Destroy;
begin
  FManager.Free;

  inherited Destroy;
end;

procedure TPLUITabs.Redraw;
begin
  //if FManager.FAnimations.IsAnimating then exit; // coś nie działa jak trzeba

  inherited Redraw;
end;

procedure TPLUITabs.UpdateTabs;
begin
  FManager.UpdateTabs;
end;

function TPLUITabs.AddTab: TPLUITab;
begin
  Result := FManager.AddTab;
end;

end.

