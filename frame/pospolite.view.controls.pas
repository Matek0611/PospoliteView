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

interface

uses
  Classes, SysUtils, Graphics, Controls, dateutils, Pospolite.View.Basics,
  Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.CSS.Animation, Pospolite.View.Drawing.Drawer;

type

  { TPLUIControl }

  TPLUIControl = class(TPLCustomControl)
  private
    FMousePos: TPLPointI;
    FMouseDown: TPLBool;
    FMouseButton: TMouseButton;
    FShiftState: TShiftState;
  protected
    procedure DoDraw; virtual;
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

    procedure Redraw; override; final;

    property MousePosition: TPLPointI read FMousePos;
    property MouseIsDown: TPLBool read FMouseDown;
    property MouseButton: TMouseButton read FMouseButton;
    property ShiftState: TShiftState read FShiftState;
  published
    property OnClick;
    property OnEnter;
    property OnExit;
  end;

  { TPLUIAnimationLoopItem }

  TPLUIAnimationLoopItem = record
  public
    Started: TPLBool;
    Finished: TPLBool;
    StartingTime: TDateTime;
    Duration: Cardinal;
    StartingValues: array of TPLFloat;
    Variables: array of TPLPFloat;
    TimingFunction: TPLCSSTimingFunction;
  public
    constructor Create(const ADuration: Cardinal; const AVariables: array of TPLPFloat;
      ATimingFunction: TPLCSSTimingFunction);

    procedure Next;

    class operator =(a, b: TPLUIAnimationLoopItem) r: TPLBool;
  end;

  TPLUIAnimationQueue = class(specialize TPLList<TPLUIAnimationLoopItem>);

  { TPLUIAnimationLoop }

  TPLUIAnimationLoop = class(TThread)
  private
    FQueue: TPLUIAnimationQueue;
    FControl: TPLUIControl;
  public
    constructor Create(const AControl: TPLUIControl);
    destructor Destroy; override;

    procedure Execute; override;

    procedure Add(const AEvent: TPLUIAnimationLoopItem);
    procedure Clear;
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

  TPLUITabsState = (tsNormal, tsHover, tsDown);

  { TPLUITab }

  TPLUITab = class(TPersistent)
  private
    FActive: TPLBool;
    FCaption: TPLString;
    FColor: TPLColor;
    FFavIcon: IPLDrawingBitmap;
    FHint: TPLString;
    FList: TPLUITabList;
    FPinned: TPLBool;

    procedure SetActive(AValue: TPLBool);
    procedure SetCaption(AValue: TPLString);
    procedure SetColor(AValue: TPLColor);
    procedure SetFavIcon(AValue: IPLDrawingBitmap);
    procedure SetPinned(AValue: TPLBool);
  protected
    FState: TPLUITabsState;
    FRect: TPLRectF;

    function CloseButtonRect: TPLRectF; inline;
  public
    constructor Create(const AList: TPLUITabList);
    destructor Destroy; override;

    procedure Exchange(const AList: TPLUITabList; const AMoveLast: TPLBool = true); // for pin/unpin
    procedure Draw(const ARenderer: TPLDrawingRenderer);
    function MouseInTab(const X, Y: TPLFloat): TPLBool; inline;
    function MouseInCloseButton(const X, Y: TPLFloat): TPLBool; inline;
    procedure Close;

    property Caption: TPLString read FCaption write SetCaption;
    property Active: TPLBool read FActive write SetActive;
    property Pinned: TPLBool read FPinned write SetPinned;
    property Color: TPLColor read FColor write SetColor;
    property FavIcon: IPLDrawingBitmap read FFavIcon write SetFavIcon;
    property Hint: TPLString read FHint write FHint;
  end;

  { TPLUITabsManager }

  TPLUITabsManager = class(TPersistent)
  private
    FAnimations: TPLUIAnimationLoop;
    FTabs: TPLUITabs;
    FNormalTabs: TPLUITabList;
    FPinnedTabs: TPLUITabList;
  public
    constructor Create(const ATabs: TPLUITabs);
    destructor Destroy; override;

    property PinnedTabs: TPLUITabList read FPinnedTabs;
    property NormalTabs: TPLUITabList read FNormalTabs;
    property Animations: TPLUIAnimationLoop read FAnimations;
  end;

  { TPLUITabs }

  TPLUITabs = class(TPLUIControl)
  private
    FManager: TPLUITabsManager;
  protected
    procedure DoDraw; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TPLUIControl }

procedure TPLUIControl.DoDraw;
begin
  // ...
end;

procedure TPLUIControl.Paint;
begin
  inherited Paint;

  DoDraw;
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
end;

destructor TPLUIControl.Destroy;
begin
  inherited Destroy;
end;

procedure TPLUIControl.Redraw;
begin
  Refresh;
end;

{ TPLUIControlLoopItem }

constructor TPLUIAnimationLoopItem.Create(const ADuration: Cardinal;
  const AVariables: array of TPLPFloat; ATimingFunction: TPLCSSTimingFunction);
var
  i: SizeInt;
begin
  Started := false;
  Finished := false;
  Duration := ADuration;
  TimingFunction := ATimingFunction;

  SetLength(Variables, Length(AVariables));
  SetLength(StartingValues, Length(AVariables));

  for i := Low(Variables) to High(Variables) do begin
    Variables[i] := AVariables[i];
    StartingValues[i] := AVariables[i]^;
  end;
end;

procedure TPLUIAnimationLoopItem.Next;
var
  m: Int64 = 0;
  i: SizeInt;
begin
  if Finished then exit;

  if not Started then begin
    Started := true;
    Finished := false;

    StartingTime := Now;
  end else begin
    m := MilliSecondsBetween(Now, StartingTime);
    Finished := m < Duration;
    if m > Duration then m := Duration;

    if Finished then Started := false;
  end;

  if not Finished then
  for i := Low(Variables) to High(Variables) do
    Variables[i]^ := StartingValues[i] * TimingFunction(m / Duration, []).Y;
end;

class operator TPLUIAnimationLoopItem.=(a, b: TPLUIAnimationLoopItem) r: TPLBool;
var
  i: SizeInt;
begin
  r := (a.Started = b.Started) and (a.Finished = b.Finished) and
    (a.Duration = b.Duration) and (a.TimingFunction = b.TimingFunction) and
    (Length(a.StartingValues) = Length(b.StartingValues)) and (Length(a.Variables) = Length(b.Variables));

  if r then
  for i := Low(a.Variables) to High(a.Variables) do begin
    r := r and (a.Variables[i] = b.Variables[i]);
    if not r then break;
  end;
end;

{ TPLUIAnimationLoop }

constructor TPLUIAnimationLoop.Create(const AControl: TPLUIControl);
begin
  inherited Create(false);

  FControl := AControl;
  FQueue := TPLUIAnimationQueue.Create;
end;

destructor TPLUIAnimationLoop.Destroy;
begin
  Suspended := true;
  Clear;

  inherited Destroy;
end;

procedure TPLUIAnimationLoop.Execute;
var
  item: TPLUIAnimationLoopItem;
begin
  while not Suspended and not Terminated do begin
    for item in FQueue do begin
      if item.Finished then FQueue.Remove(item)
      else item.Next;
    end;

    if Assigned(FControl) then FControl.Redraw;

    Sleep(33); // 1000/30 fps
  end;
end;

procedure TPLUIAnimationLoop.Add(const AEvent: TPLUIAnimationLoopItem);
begin
  FQueue.Add(AEvent);
end;

procedure TPLUIAnimationLoop.Clear;
begin
  FQueue.Clear;
end;

{ TPLUITabList }

constructor TPLUITabList.Create(const ATabs: TPLUITabsManager);
begin
  inherited Create(true);

  FTabs := ATabs;
end;

procedure TPLUITabList.UpdateTabs;
begin

end;

{ TPLUITab }

procedure TPLUITab.SetCaption(AValue: TPLString);
begin
  if FCaption = AValue then exit;
  FCaption := AValue;

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

procedure TPLUITab.SetPinned(AValue: TPLBool);
begin
  if FPinned = AValue then exit;
  FPinned := AValue;

  FList.UpdateTabs;
end;

function TPLUITab.CloseButtonRect: TPLRectF;
var
  xsize, xmove: TPLFloat;
begin
  xsize := FRect.Height / 2;
  xmove := xsize / 2;

  Result := TPLRectF.Create(FRect.Right - xsize - xmove, FRect.Top + xmove, xsize, xsize);
end;

procedure TPLUITab.SetActive(AValue: TPLBool);
begin
  if FActive = AValue then exit;
  FActive := AValue;

  FList.UpdateTabs;
end;

constructor TPLUITab.Create(const AList: TPLUITabList);
begin
  inherited Create;

  FState := tsNormal;
  FRect := TPLRectF.Create(0, 0, 0, 0);

  FList := AList;
  FCaption := 'Tab';
  FActive := false;
  FPinned := true;
  FFavIcon := nil;
  FHint := '';
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
begin

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

constructor TPLUITabsManager.Create(const ATabs: TPLUITabs);
begin
  inherited Create;

  FTabs := ATabs;
  FNormalTabs := TPLUITabList.Create(Self);
  FPinnedTabs := TPLUITabList.Create(Self);
  FAnimations := TPLUIAnimationLoop.Create(ATabs);
end;

destructor TPLUITabsManager.Destroy;
begin
  FAnimations.Free;
  FNormalTabs.Free;
  FPinnedTabs.Free;

  inherited Destroy;
end;

{ TPLUITabs }

procedure TPLUITabs.DoDraw;
begin

end;

constructor TPLUITabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FManager := TPLUITabsManager.Create(Self);
end;

destructor TPLUITabs.Destroy;
begin
  FManager.Free;

  inherited Destroy;
end;

end.

