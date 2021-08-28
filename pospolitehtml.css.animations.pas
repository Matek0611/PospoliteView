unit PospoLiteHTML.CSS.Animations;

{$mode delphi}{$H+}
{$hints off}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, math, Types, strutils, fgl, syncobjs, PospoLiteHTML.CSS.Basics,
  PospoLiteHTML.CSS.Values, PospoLiteHTML.CSS.Classes;

type

  // https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function
  TPLCSSTimingFunction = function (APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat;

  { TPLCSSTimingFunctions }

  TPLCSSTimingFunctions = record
  public
    class function CubicBezier(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;
    class function Steps(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;

    class function Linear(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;
    class function Ease(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;
    class function EaseIn(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;
    class function EaseOut(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;
    class function EaseInOut(APercent: TPLCSSFloat; const AParams: array of TPLCSSFloat): TPLCSSFloat; static;

    class function AsFunction(AValue: string): TPLCSSTimingFunction; static;
    class function DefaultFunction: TPLCSSTimingFunction; static;
  end;

  TPLCSSStepProc = procedure of object;
  TPLCSSStepProcList = TFPGList<TPLCSSStepProc>;

  { TPLCSSAnimationTimerThread }

  TPLCSSAnimationTimerThread = class(TThread)
  private
    FInterval: Cardinal;
    FEnabled: boolean;
    FActive: boolean;
    FEnableEvent: TEvent;
    FID: Integer;
    FOnStep: TPLCSSStepProc;
    procedure SetEnabled(AValue: boolean);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    property Active: boolean read FActive;
    property Enabled: boolean read  FEnabled write SetEnabled;
    property ID: Integer read FID write FID;
    property Interval: Cardinal read FInterval write FInterval;
    property OnStep: TPLCSSStepProc read FOnStep write FOnStep;
  end;

  { TPLCSSAnimationTimer }

  TPLCSSAnimationTimer = class(TComponent)
  private
    FStepsEvents: TPLCSSStepProcList;
    FThread: TPLCSSAnimationTimerThread;
    FID: Integer;
    FInterval: Cardinal;
    function GetEnabled: boolean;
    function GetInterval: Cardinal;
    procedure SetID(AValue: Integer);
    procedure SetInterval(AValue: Cardinal);
    procedure SetEnabled(AValue: Boolean);
    procedure Step;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ID: Integer read FID write SetID;
    property Interval: Cardinal read GetInterval write SetInterval;
    property Enabled: boolean read GetEnabled write SetEnabled default False;
    property StepsEvents: TPLCSSStepProcList read FStepsEvents write FStepsEvents;
  end;

  TPLCSSAnimationKind = (akLinear, akEase, akEaseIn, akEaseOut, akEaseInOut,
    akSteps, akCubicBezier, akKeyFrames);
  TPLCSSAnimationDirection = (adNormal, adReverse, adAlternate, adAlternateReverse);
  TPLCSSAnimationFillMode = (afmNone, afmForwards, afmBackwards, afmBoth);
  TPLCSSAnimationPlayState = (apsPaused, apsRunning);

  function PLCSSAnimationKind(a: string): TPLCSSAnimationKind;

  type

  { TPLCSSAnimation }

  //TPLCSSAnimation = record
  //private
  //  FDelay: TPLCSSFloat;
  //  FDirection: TPLCSSAnimationDirection;
  //  FDuration: TPLCSSFloat;
  //  FFillMode: TPLCSSAnimationFillMode;
  //  FIterationCount: TPLCSSFloat;
  //  FKind: TPLCSSAnimationKind;
  //  FFunction: TPLCSSTimingFunction;
  //  FName: string;
  //  FParams: array of TPLCSSFloat;
  //  FPlayState: TPLCSSAnimationPlayState;
  //  procedure SetDelay(AValue: TPLCSSFloat);
  //  procedure SetDirection(AValue: TPLCSSAnimationDirection);
  //  procedure SetDuration(AValue: TPLCSSFloat);
  //  procedure SetFillMode(AValue: TPLCSSAnimationFillMode);
  //  procedure SetIterationCount(AValue: TPLCSSFloat);
  //  procedure SetKind(AValue: TPLCSSAnimationKind);
  //  procedure SetPlayState(AValue: TPLCSSAnimationPlayState);
  //
  //  procedure Analyze(AValue: string);
  //  procedure ResetDefaults;
  //public
  //  constructor Create(AValue: string);
  //
  //  function Evaluate(t: TPLCSSFloat): TPLCSSFloat;
  //
  //  property Delay: TPLCSSFloat read FDelay write SetDelay;
  //  property Direction: TPLCSSAnimationDirection read FDirection write SetDirection;
  //  property Duration: TPLCSSFloat read FDuration write SetDuration;
  //  property FillMode: TPLCSSAnimationFillMode read FFillMode write SetFillMode;
  //  property IterationCount: TPLCSSFloat read FIterationCount write SetIterationCount;
  //  property Name: string read FName write FName;
  //  property PlayState: TPLCSSAnimationPlayState read FPlayState write SetPlayState;
  //  property TimingFunction: TPLCSSAnimationKind read FKind write SetKind;
  //end;

  //TPLCSSAnimationList = TFPGObjectList<TPLCSSAnimation>;

  TPLCSSTransitionKind = (tkInt, tkFloat, tkColor);

  { TPLCSSTransition }

  TPLCSSTransition = class
  strict private
    FDelay: TPLCSSTimeUnit;
    FDuration: TPLCSSTimeUnit;
    FFrom: string;
    FKind: TPLCSSAnimationKind;
    FProperty: string;
    FTo: string;
    procedure SetDelay(AValue: TPLCSSTimeUnit);
    procedure SetDuration(AValue: TPLCSSTimeUnit);
    procedure SetKind(AValue: TPLCSSAnimationKind);
    procedure SetProperty(AValue: string);
  private
    FActive: boolean;
    FActual: string;
    FRaw: string;
    FParams: array of TPLCSSFloat;
    FType: TPLCSSTransitionKind;
    FStep: TPLCSSFloat;
    procedure Analyze;
    function EvaluateFunction(ATime: TPLCSSFloat): TPLCSSFloat;
  public
    constructor Create;

    class function Empty: TPLCSSTransition; static;
    function IsEmpty: boolean;

    function Finished: boolean;
    procedure Step;
    procedure Prepare;
    function Equals(a: TPLCSSTransition): boolean; reintroduce;

    property Delay: TPLCSSTimeUnit read FDelay write SetDelay;
    property Duration: TPLCSSTimeUnit read FDuration write SetDuration;
    property &Property: string read FProperty write SetProperty;
    property TimingFunction: TPLCSSAnimationKind read FKind write SetKind;

    property Active: boolean read FActive write FActive;
    property From: string read FFrom write FFrom;
    property &To: string read FTo write FTo;
    property Actual: string read FActual;
    property Raw: string read FRaw;
    property &Type: TPLCSSTransitionKind read FType write FType;
  end;

type

  TPLCSSTransitionList = TFPGObjectList<TPLCSSTransition>;

  { TPLCSSTransitions }

  TPLCSSTransitions = class(TObject)
  private
    FEnabled: boolean;
    FControl: TPLCSSCustomControl;
    FTransitions: TPLCSSTransitionList;
    function GetTransition(AIndex: integer): TPLCSSTransition;
    function GetTransitionByName(AIndex: string): TPLCSSTransition;
    procedure SetTransition(AIndex: integer; AValue: TPLCSSTransition);
    procedure PrepareAll;
    procedure SetTransitionByName(AIndex: string; AValue: TPLCSSTransition);
  protected
    FCustomStart: boolean;
    procedure Step;
    //procedure Assign(AProperties: TPLCSSProperties);
  public
    constructor Create(AControl: TPLCSSCustomControl);
    destructor Destroy; override;

    procedure Clear;
    procedure Stop;
    procedure Start;
    function IndexOf(AName: string): integer;
    function TryGetTransitionActual(AName: string; ADef: string): string;

    property Transition[AIndex: integer]: TPLCSSTransition read GetTransition write SetTransition; default;
    property TransitionByName[AIndex: string]: TPLCSSTransition read GetTransitionByName write SetTransitionByName;
    property Transitions: TPLCSSTransitionList read FTransitions;
  end;

implementation

function PointF(AX, AY: Single): TPointF;
begin
  Result.x := AX;
  Result.y := AY;
end;

function PLCSSAnimationKind(a: string): TPLCSSAnimationKind;
begin
  Result := akKeyFrames;

  a := a.Trim.ToLower;
  if a = 'linear' then Result := akLinear
  else if a = 'ease' then Result := akEase
  else if a = 'ease-in' then Result := akEaseIn
  else if a = 'ease-out' then Result := akEaseOut
  else if a = 'ease-in-out' then Result := akEaseInOut
  else if Pos('step', a) > 0 then Result := akSteps
  else if Pos('cubic-bezier', a) > 0 then Result := akCubicBezier;
end;

procedure CorrectTransitionFunctions(var s: string);
var
  i, p, x: integer;
begin
  i := 1;
  while i <= Length(s) do begin
    p := PosEx('(', s, i);
    x := PosEx(')', s, i);
    s := Copy(s, 1, p) + Copy(s, p + 1, Length(s) - x - 1).Replace('  ', ' ').Replace(', ', ',') + Copy(s, x, Length(s) - x);
  end;
end;

var
  InternalTimer: TPLCSSAnimationTimer;

{ TPLCSSAnimationTimerThread }

procedure TPLCSSAnimationTimerThread.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;

  if FEnabled then FEnableEvent.SetEvent else FEnableEvent.ResetEvent;
end;

procedure TPLCSSAnimationTimerThread.Execute;
var
  AStart, AElapsed: Int64;
  ASleepTime: integer = 10;
  ATimeOut: integer = 500;
  AEventResult: TWaitResult;
begin
  AStart := GetTickCount64;
  FActive := true;

  while not Terminated do begin
    if not FEnabled then begin
      AEventResult := FEnableEvent.WaitFor(ATimeOut);

      while AEventResult = wrTimeout do
        AEventResult := FEnableEvent.WaitFor(ATimeOut);

      if Terminated then break;

      if AEventResult = wrSignaled then begin
        AStart := GetTickCount64;
        continue;
      end;

      break;
    end;

    AElapsed := GetTickCount64 - AStart;
    if AElapsed >= FInterval then begin
      if Assigned(FOnStep) then FOnStep;
      if Terminated then break;
      if not FEnabled then continue;

      AStart := GetTickCount64;
    end;

    sleep(ASleepTime);
  end;

  FActive := false;
end;

constructor TPLCSSAnimationTimerThread.Create;
begin
  inherited Create(true);

  FreeOnTerminate := false;

  FEnableEvent := TEvent.Create(nil, true, true, '');
  FEnabled := false;
  FInterval := 33; // 30 fps
  FActive := false;
  FID := 0;
end;

destructor TPLCSSAnimationTimerThread.Destroy;
begin
  FreeAndNil(FEnableEvent);

  inherited Destroy;
end;

{ TPLCSSAnimationTimer }

procedure TPLCSSAnimationTimer.Step;
var
  i: integer;
begin
  for i := 0 to FStepsEvents.Count-1 do FStepsEvents[i];
end;

function TPLCSSAnimationTimer.GetInterval: Cardinal;
begin
  Result := FInterval;
end;

function TPLCSSAnimationTimer.GetEnabled: boolean;
begin
  Result := FThread.Enabled;
end;

procedure TPLCSSAnimationTimer.SetID(AValue: Integer);
begin
  if FID = AValue then exit;
  FID := AValue;

  if Assigned(FThread) then FThread.ID := FID;
end;

procedure TPLCSSAnimationTimer.SetInterval(AValue: Cardinal);
begin
  if (not Assigned(FThread)) or (AValue = FInterval) then exit;

  FInterval := AValue;
  FThread.Interval := AValue;
end;

procedure TPLCSSAnimationTimer.SetEnabled(AValue: Boolean);
begin
  if (not Assigned(FThread)) or (AValue = FThread.Enabled) then exit;

  FThread.Enabled := AValue;
  if AValue then begin
    FThread.Interval := FInterval;
    if not FThread.Active then
      FThread.Start
    else
      FThread.FEnableEvent.SetEvent;
    FThread.Enabled := True
  end else FThread.Enabled := False;
end;

constructor TPLCSSAnimationTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStepsEvents := TPLCSSStepProcList.Create;

  FThread := TPLCSSAnimationTimerThread.Create;
  FThread.FreeOnTerminate := false;
  FThread.OnStep := Self.Step;
end;

destructor TPLCSSAnimationTimer.Destroy;
var
  AElapsed, AThreadTimeout: Cardinal;
begin
  AThreadTimeout := 100;

  if Assigned(FThread) then begin
    if FThread.Active then begin
      FThread.FEnableEvent.SetEvent;
      FThread.Terminate;
    end;
    AElapsed := 0;
    while FThread.Active do begin
      WaitForThreadTerminate(FThread.Handle, AThreadTimeout);
      if AElapsed > 5000 then break;
      Inc(AElapsed, AThreadTimeout)
    end;
    FreeAndNil(FThread);
  end;

  FStepsEvents.Free;

  inherited Destroy;
end;

{ TPLCSSTransitions }

function TPLCSSTransitions.GetTransition(AIndex: integer): TPLCSSTransition;
begin
  if (AIndex < 0) or (AIndex >= FTransitions.Count) then exit(nil);

  Result := FTransitions[AIndex];
end;

function TPLCSSTransitions.GetTransitionByName(AIndex: string
  ): TPLCSSTransition;
var
  id: integer;
begin
  id := IndexOf(AIndex);
  if id > -1 then Result := Transition[id] else Result := nil;
end;

procedure TPLCSSTransitions.SetTransition(AIndex: integer;
  AValue: TPLCSSTransition);
begin
  if (AIndex < 0) or (AIndex >= FTransitions.Count) then exit;

  FTransitions[AIndex] := AValue;
end;

procedure TPLCSSTransitions.PrepareAll;
var
  from_copy: array of string;
  i: sizeint;

  procedure AddTransition(AProperty: string; AKind: TPLCSSTransitionKind; APos: sizeint);
  var
    t: TPLCSSTransition;
  begin
    t := TPLCSSTransition.Empty;
    if FCustomStart then t.From := from_copy[APos]
    else t.From := FControl.Style(AProperty, FControl.FPrevState);
    t.&To := FControl.Style(AProperty, FControl.FNextState);
    t.&Type := AKind;
    t.&Property := AProperty;

    FTransitions.Add(t);
  end;

begin
  if FCustomStart then begin
    SetLength(from_copy, FTransitions.Count);

    for i := 0 to FTransitions.Count-1 do begin
      from_copy[i] := FTransitions[i].Actual;
    end;
  end;
  Clear;

  // colors
  AddTransition('background-color', tkColor, 0);

  SetLength(from_copy, 0);
  FCustomStart := false;
end;

procedure TPLCSSTransitions.SetTransitionByName(AIndex: string;
  AValue: TPLCSSTransition);
var
  id: integer;
begin
  id := IndexOf(AIndex);
  if id > -1 then Transition[id] := AValue;
end;

procedure TPLCSSTransitions.Step;
var
  i, x: integer;
begin
  if not FEnabled then exit;

  x := 0;
  for i := 0 to FTransitions.Count-1 do begin
    if FTransitions[i].Finished or not FTransitions[i].Active then begin
      inc(x);
      if FTransitions[i].Actual <> FTransitions[i].&To then FTransitions[i].FActual := FTransitions[i].&To;
    end else FTransitions[i].Step;
  end;

  FControl.Invalidate;

  if x = FTransitions.Count then Stop;
end;

constructor TPLCSSTransitions.Create(AControl: TPLCSSCustomControl);
begin
  inherited Create;

  FTransitions := TPLCSSTransitionList.Create(true);
  FControl := AControl;
  FCustomStart := false;
  FEnabled := false;

  InternalTimer.StepsEvents.Add(Self.Step);
end;

destructor TPLCSSTransitions.Destroy;
begin
  Stop;
  FTransitions.Free;

  InternalTimer.StepsEvents.Remove(Self.Step);

  inherited Destroy;
end;

//procedure TPLCSSTransitions.Assign(AProperties: TPLCSSProperties);
//var
//  w, z: string;
//  t, tmp: TPLCSSTransition;
//  i, p: integer;
//  err: boolean = false;
//  tab: array of string;
//begin
//  Stop;
//  PrepareAll;
//
//  t := TPLCSSTransition.Empty;
//  try
//    w := AProperties['transition-delay'];
//    ProcessImportant(w);
//    if not IsInitValue(w) then t.Delay := w;
//
//    w := AProperties['transition-duration'];
//    ProcessImportant(w);
//    if not IsInitValue(w) then t.Duration := w;
//
//    w := AProperties['transition-property'];
//    ProcessImportant(w);
//    if not IsInitValue(w) then t.&Property := w else t.&Property := '';
//
//    w := AProperties['transition-timing-function'];
//    ProcessImportant(w);
//    if not IsInitValue(w) then begin
//      t.TimingFunction := PLCSSAnimationKind(w);
//      t.FRaw := w.Trim.ToLower;
//      t.Analyze;
//    end;
//
//    w := AProperties['transition'].ToLower;
//    ProcessImportant(w);
//    if not IsInitValue(w) then begin
//      SetLength(tab, 4);
//      i := 1;
//      CorrectTransitionFunctions(w);
//      while i <= Length(w) do begin
//        tab[0] := '';
//        tab[1] := '0s';
//        tab[2] := 'ease';
//        tab[3] := '0s';
//
//        p := PosEx(' ', w, i);
//        if p < 0 then begin
//          err := true;
//          break;
//        end;
//        tab[0] := Copy(w, i, p - i).Trim;
//
//        i := p + 1;
//        p := PosEx(' ', w, i);
//        if p < 0 then begin
//          err := true;
//          break;
//        end;
//        tab[1] := Copy(w, i, p - i).Trim;
//
//        i := p + 1;
//        z := '';
//        //while i <= Length(w) do begin
//        //  if w[i] =
//        //end;
//      end;
//
//      if err then begin
//        for i := 0 to FTransitions.Count-1 do begin
//          FTransitions[i].Active := true;
//          FTransitions[i].Delay := t.Delay;
//          FTransitions[i].Duration := t.Duration;
//          FTransitions[i].TimingFunction := t.TimingFunction;
//          FTransitions[i].FRaw := t.Raw;
//        end;
//      end;
//    end else if t.&Property <> '' then begin
//      if t.&Property.ToLower = 'all' then begin
//        for i := 0 to FTransitions.Count-1 do begin
//          FTransitions[i].Active := true;
//          FTransitions[i].Delay := t.Delay;
//          FTransitions[i].Duration := t.Duration;
//          FTransitions[i].TimingFunction := t.TimingFunction;
//          FTransitions[i].FRaw := t.Raw;
//        end;
//      end else begin
//        tab := t.&Property.Split(',');
//
//        for i := 0 to Length(tab)-1 do begin
//          tmp := TransitionByName[tab[i].Trim.ToLower];
//          if Assigned(tmp) then begin
//            tmp.Active := true;
//            tmp.Delay := t.Delay;
//            tmp.Duration := t.Duration;
//            tmp.TimingFunction := t.TimingFunction;
//            tmp.FRaw := t.Raw;
//          end;
//        end;
//      end;
//    end;
//  finally
//    t.Free;
//  end;
//end;

procedure TPLCSSTransitions.Clear;
begin
  FTransitions.Clear;
end;

procedure TPLCSSTransitions.Stop;
begin
  FEnabled := false;
end;

procedure TPLCSSTransitions.Start;
var
  i: integer;
begin
  if FEnabled then FCustomStart := true;

  //Assign(FControl.Styles['::self::'].Properties);

  for i := 0 to FTransitions.Count-1 do
    FTransitions[i].Prepare;

  FEnabled := true;
end;

function TPLCSSTransitions.IndexOf(AName: string): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to FTransitions.Count-1 do
    if FTransitions[i].&Property = AName then exit(i);
end;

function TPLCSSTransitions.TryGetTransitionActual(AName: string; ADef: string
  ): string;
var
  t: TPLCSSTransition;
begin
  t := TransitionByName[AName];
  if Assigned(t) and t.Active then Result := t.Actual else Result := ADef;
end;

{ TPLCSSTransition }

procedure TPLCSSTransition.SetDelay(AValue: TPLCSSTimeUnit);
begin
  if FDelay = AValue then exit;
  FDelay := AValue;
end;

procedure TPLCSSTransition.SetDuration(AValue: TPLCSSTimeUnit);
begin
  if FDuration = AValue then exit;
  FDuration := AValue;
end;

procedure TPLCSSTransition.SetKind(AValue: TPLCSSAnimationKind);
begin
  if FKind = AValue then exit;
  FKind := AValue;
end;

procedure TPLCSSTransition.SetProperty(AValue: string);
begin
  if FProperty = AValue then exit;
  FProperty := AValue;
end;

procedure TPLCSSTransition.Analyze;
var
  s: string;
  tab: array of string;
  i, p: integer;
begin
  SetLength(FParams, 0);

  try
    case FKind of
      akCubicBezier: begin
        SetLength(FParams, 4);

        s := Copy(FRaw, Pos('(', FRaw) + 1, Length(FRaw) - Pos('(', FRaw) - 1);
        tab := s.Split(',');

        for i := 0 to 3 do begin
          tab[i] := Trim(tab[i]);
          p := Pos('.', tab[i]);
          if (p > 0) then begin
            if tab[i][1] in ['-', '+'] then p := 2 else p := 1;
            tab[i] := Copy(tab[i], 1, p - 1) + '0' + Copy(tab[i], p, Length(tab[i]));
          end;
          FParams[i] := StrToFloatDef(tab[i], 0, PLCSSFormatSettingsDef);
        end;
      end;
      akSteps: begin
        SetLength(FParams, 2);

        if FRaw = 'step-start' then begin
          FParams[0] := 1;
          FParams[1] := 1;
        end else if FRaw = 'step-end' then begin
          FParams[0] := 1;
          FParams[1] := 2;
        end else begin
          s := Copy(FRaw, Pos('(', FRaw) + 1, Length(FRaw) - Pos('(', FRaw) - 1);
          tab := s.Split(',');

          FParams[0] := 1;
          TryStrToFloat(tab[0].Trim, FParams[0], PLCSSFormatSettingsDef);
          case IndexStr(tab[1].Trim, ['jump-start', 'start', 'jump-end', 'end', 'jump-both']) of
            0, 1: FParams[1] := 1;
            2, 3: FParams[1] := 2;
            4: FParams[1] := 3;
            else FParams[1] := 4;
          end;
        end;
      end;
      akKeyFrames: begin
        // akKeyFrames in transition = none
      end;
    end;
  except
    on e: Exception do FKind := akLinear;
  end;
end;

function TPLCSSTransition.EvaluateFunction(ATime: TPLCSSFloat): TPLCSSFloat;
begin
  case FKind of
    akLinear: Result := TPLCSSTimingFunctions.Linear(ATime, []);
    akEase: Result := TPLCSSTimingFunctions.Ease(ATime, []);
    akEaseIn: Result := TPLCSSTimingFunctions.EaseIn(ATime, []);
    akEaseOut: Result := TPLCSSTimingFunctions.EaseOut(ATime, []);
    akEaseInOut: Result := TPLCSSTimingFunctions.EaseInOut(ATime, []);
    akSteps: Result := TPLCSSTimingFunctions.Steps(ATime, FParams);
    akCubicBezier: Result := TPLCSSTimingFunctions.CubicBezier(ATime, FParams);
  end;
end;

constructor TPLCSSTransition.Create;
begin
  inherited Create;

  FActive := false;
end;

class function TPLCSSTransition.Empty: TPLCSSTransition;
begin
  Result := TPLCSSTransition.Create;
  Result.Delay := '0s';
  Result.Duration := '0s';
  Result.&Property := 'all';
  Result.TimingFunction := akEase;
end;

function TPLCSSTransition.IsEmpty: boolean;
begin
  Result := (Delay.AsMilliseconds = 0) and (Duration.AsMilliseconds = 0) and (&Property = 'all')
    and (TimingFunction in [akEase, akKeyFrames]);
end;

function TPLCSSTransition.Finished: boolean;
begin
  Result := (FStep - FDelay.AsMilliseconds > FDuration.AsMilliseconds);
end;

procedure TPLCSSTransition.Step;
begin
  FStep += 100 / 3;

  if Finished then exit;

  if FStep < FDelay.AsMilliseconds then exit;

  case FType of
    tkColor: begin
      FActual := TPLCSSColor.Create(FFrom).MixWithColor(FTo, EvaluateFunction((FStep - FDelay.AsMilliseconds) / FDuration.AsMilliseconds)).AsHex;
    end;
  end;
end;

procedure TPLCSSTransition.Prepare;
begin
  Analyze;

  FStep := 0; // step in ms
  FActual := FFrom;
end;

function TPLCSSTransition.Equals(a: TPLCSSTransition): boolean;
begin
  Result := (self.FDelay = a.FDelay) and (self.FDuration = a.FDuration) and (self.FKind = a.FKind)
    and (self.FProperty = a.FProperty);
end;

{ TPLCSSAnimation }

//procedure TPLCSSAnimation.SetDelay(AValue: TPLCSSFloat);
//begin
//  if FDelay = AValue then exit;
//  FDelay := AValue;
//end;
//
//procedure TPLCSSAnimation.SetDirection(AValue: TPLCSSAnimationDirection);
//begin
//  if FDirection = AValue then exit;
//  FDirection := AValue;
//end;
//
//procedure TPLCSSAnimation.SetDuration(AValue: TPLCSSFloat);
//begin
//  if FDuration = AValue then exit;
//  FDuration := AValue;
//end;
//
//procedure TPLCSSAnimation.SetFillMode(AValue: TPLCSSAnimationFillMode);
//begin
//  if FFillMode = AValue then exit;
//  FFillMode := AValue;
//end;
//
//procedure TPLCSSAnimation.SetIterationCount(AValue: TPLCSSFloat);
//begin
//  if FIterationCount = AValue then exit;
//  FIterationCount := AValue;
//end;
//
//procedure TPLCSSAnimation.SetKind(AValue: TPLCSSAnimationKind);
//begin
//  if FKind = AValue then exit;
//  FKind := AValue;
//end;
//
//procedure TPLCSSAnimation.SetPlayState(AValue: TPLCSSAnimationPlayState);
//begin
//  if FPlayState = AValue then exit;
//  FPlayState := AValue;
//end;
//
//procedure TPLCSSAnimation.Analyze(AValue: string);
//begin
//
//end;
//
//procedure TPLCSSAnimation.ResetDefaults;
//begin
//  FKind := akLinear;
//  FFunction := TPLCSSTimingFunctions.AsFunction('linear');
//  SetLength(FParams, 0);
//end;
//
//constructor TPLCSSAnimation.Create(AValue: string);
//begin
//  ResetDefaults;
//  Analyze(AValue);
//end;
//
//function TPLCSSAnimation.Evaluate(t: TPLCSSFloat): TPLCSSFloat;
//begin
//  Result := FFunction(t, FParams);
//end;

{ TPLCSSTimingFunctions }

class function TPLCSSTimingFunctions.Linear(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
begin
  Result := APercent; // CubicBezier(APercent, [0.0, 0.0, 1.0, 1.0]);
end;

class function TPLCSSTimingFunctions.Ease(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
begin
  Result := CubicBezier(APercent, [0.25, 0.1, 0.25, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseIn(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
begin
  Result := CubicBezier(APercent, [0.42, 0.0, 1.0, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseOut(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
begin
  Result := CubicBezier(APercent, [0.0, 0.0, 0.58, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseInOut(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
begin
  Result := CubicBezier(APercent, [0.42, 0.0, 0.58, 1.0]);
end;

class function TPLCSSTimingFunctions.AsFunction(AValue: string
  ): TPLCSSTimingFunction;
begin
  Result := DefaultFunction;

  case AnsiIndexStr(AValue.ToLower, ['linear', 'ease', 'ease-in', 'ease-out', 'ease-in-out', 'cubic-bezier', 'steps', 'step-start', 'step-end']) of
    0: Result := @Linear;
    1: Result := @Ease;
    2: Result := @EaseIn;
    3: Result := @EaseOut;
    4: Result := @EaseInOut;
    5: Result := @CubicBezier;
    6, 7, 8: Result := @Steps;
  end;
end;

class function TPLCSSTimingFunctions.DefaultFunction: TPLCSSTimingFunction;
begin
  Result := @Linear;
end;

function CheckRange(AValue: TPLCSSFloat): boolean;
begin
  Result := (AValue >= 0) and (AValue <= 1);
end;

class function TPLCSSTimingFunctions.CubicBezier(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;
var
   B: TPointF;
   t: TPLCSSFloat absolute APercent;
begin
  if not (CheckRange(t) and (Length(AParams) = 4) and CheckRange(AParams[0])
    and CheckRange(AParams[2])) then exit(0);

  B := 3 * power(1 - t, 2) * t * PointF(AParams[0], AParams[1]) +
       3 * (1 - t) * power(t, 2) * PointF(AParams[2], AParams[3]) +
       power(t, 3) * PointF(1, 1);

  Result := B.y;
end;

// https://www.w3.org/TR/css-easing-1/#step-easing-functions
class function TPLCSSTimingFunctions.Steps(APercent: TPLCSSFloat;
  const AParams: array of TPLCSSFloat): TPLCSSFloat;

  function offset(const AType: string): TPLCSSInt;
  begin
    case AnsiIndexStr(AType, ['jump-end', 'end', 'jump-none']) of
      0, 1, 2: Result := 0;
      else Result := 1;
    end;
  end;

  function step_count(const ANumber: TPLCSSInt; const AType: string): TPLCSSInt;
  begin
    case AnsiIndexStr(AType, ['jump-both', 'jump-none']) of
      0: Result := ANumber + 1;
      1: Result := ANumber - 1;
      else Result := ANumber;
    end;
  end;

var
  n: TPLCSSInt;
  k: TPLCSSFloat;
  j: TPLCSSInt;
  o: string;
  t: TPLCSSFloat absolute APercent;
begin
  if (Length(AParams) <> 2) or not (CheckRange(t) and (Length(AParams) = 2) and (AParams[0] > 0)
    and AnsiMatchText(IntToStr(trunc(AParams[1])), ['1', '2', '3', '4'])) then exit(0);

  case trunc(AParams[1]) of
    1: o := 'start'; // jump-start/start
    2: o := 'end'; // jump-end/end
    3: o := 'jump-both';
    4: o := 'jump-none';
  end;

  n := floor64(AParams[0]);
  k := floor64(n * t + offset(o));
  j := step_count(n, o);

  if (t >= 0) and (k < 0) then k := 0;
  if (t <= 1) and (k > j) then k := j;

  Result := k / j;
end;

initialization
  InternalTimer := TPLCSSAnimationTimer.Create(nil);
  InternalTimer.Enabled := true;

finalization
  InternalTimer.Enabled := false;
  InternalTimer.Free;

end.

