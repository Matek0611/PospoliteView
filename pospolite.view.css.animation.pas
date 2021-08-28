unit Pospolite.View.CSS.Animation;

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
  Classes, SysUtils, math, dateutils, Pospolite.View.Basics,
  Pospolite.View.Drawing.Basics;

type

  // https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function
  TPLCSSTimingFunction = function(ATime: TPLFloat; const AParams: array of const): TPLPointF of object;

  { TPLCSSTimingFunctions }

  TPLCSSTimingFunctions = packed class sealed
  public
    class function CubicBezier(ATime: TPLFloat; const AParams: array of const): TPLPointF;
    class function Steps(ATime: TPLFloat; const AParams: array of const): TPLPointF;

    class function Linear(ATime: TPLFloat; const {%H-}AParams: array of const): TPLPointF;
    class function Ease(ATime: TPLFloat; const {%H-}AParams: array of const): TPLPointF;
    class function EaseIn(ATime: TPLFloat; const {%H-}AParams: array of const): TPLPointF;
    class function EaseOut(ATime: TPLFloat; const {%H-}AParams: array of const): TPLPointF;
    class function EaseInOut(ATime: TPLFloat; const {%H-}AParams: array of const): TPLPointF;

    class function GetFunction(const AName: TPLString): TPLCSSTimingFunction;
  end;

  TPLCSSTransitionEvent = procedure(const APropertyName, APseudoElement: TPLString;
    const AElapsedTime: TPLFloat) of object;

  { TPLCSSTransition }

  TPLCSSTransition = packed class
  private
    FHTMLObject: TPLHTMLObject;
    FOnCancel: TPLCSSTransitionEvent;
    FOnEnd: TPLCSSTransitionEvent;
    FOnRun: TPLCSSTransitionEvent;
    FOnStart: TPLCSSTransitionEvent;
    FRunning: TPLBool;
    FStarted: TDateTime;
    FTProp, FTPseudo: TPLString;

    procedure SetHTMLObject(AValue: TPLHTMLObject);
    procedure CleanUp;
  public
    constructor Create(AObject: TPLHTMLObject);

    procedure Start;
    procedure Cancel;
    procedure &End;

    property HTMLObject: TPLHTMLObject read FHTMLObject write SetHTMLObject;
    property Running: TPLBool read FRunning;
    property OnStart: TPLCSSTransitionEvent read FOnStart write FOnStart;
    property OnEnd: TPLCSSTransitionEvent read FOnEnd write FOnEnd;
    property OnRun: TPLCSSTransitionEvent read FOnRun write FOnRun;
    property OnCancel: TPLCSSTransitionEvent read FOnCancel write FOnCancel;
  end;

implementation

{ TPLCSSTimingFunctions }

class function TPLCSSTimingFunctions.CubicBezier(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
var
  t: TPLFloat absolute ATime;
begin
  if Length(AParams) <> 4 then exit(TPLPointF.Create(0, 0));

  Result := 3 * t * power(1 - t, 2) * TPLPointF.Create(AParams[0].VExtended^, AParams[1].VExtended^) +
    3 * (1 - t) * power(t, 2) * TPLPointF.Create(AParams[2].VExtended^, AParams[3].VExtended^) +
    power(t, 3) * TPLPointF.Create(1, 1);
end;

class function TPLCSSTimingFunctions.Steps(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
var
  t: TPLFloat absolute ATime;
  n, j: TPLInt;
  k: TPLFloat;
  o: TPLString;
begin
  if Length(AParams) <> 2 then exit(TPLPointF.Create(0, 0));

  n := floor64(AParams[0].VInt64^);
  o := TPLString(AParams[1].VUnicodeString^).Trim.ToLower;
  k := floor64(n * t + ifthen(o in TPLStringFuncs.NewArray(['jump-end', 'end', 'jump-none']), 0, 1));
  case o of
    'jump-both': j := n + 1;
    'jump-none': j := n - 1;
    else j := n;
  end;

  Result := TPLPointF.Create(t, k / j);
end;

class function TPLCSSTimingFunctions.Linear(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
begin
  Result := TPLPointF.Create(ATime, ATime); // CubicBezier(ATime, [0.0, 0.0, 1.0, 1.0])
end;

class function TPLCSSTimingFunctions.Ease(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
begin
  Result := CubicBezier(ATime, [0.25, 0.1, 0.25, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseIn(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
begin
  Result := CubicBezier(ATime, [0.42, 0.0, 1.0, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseOut(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
begin
  Result := CubicBezier(ATime, [0.0, 0.0, 0.58, 1.0]);
end;

class function TPLCSSTimingFunctions.EaseInOut(ATime: TPLFloat;
  const AParams: array of const): TPLPointF;
begin
  Result := CubicBezier(ATime, [0.42, 0.0, 0.58, 1.0]);
end;

class function TPLCSSTimingFunctions.GetFunction(const AName: TPLString
  ): TPLCSSTimingFunction;
begin
  case AName.Trim.ToLower of
    'cubic-bezier': Result := @CubicBezier;
    'steps', 'step-start', 'step-end': Result := @Steps;
    'ease': Result := @Ease;
    'ease-in': Result := @EaseIn;
    'ease-out': Result := @EaseOut;
    'ease-in-out': Result := @EaseInOut;
    else Result := @Linear;
  end;
end;

{ TPLCSSTransition }

procedure TPLCSSTransition.SetHTMLObject(AValue: TPLHTMLObject);
begin
  if FHTMLObject = AValue then exit;

  CleanUp;
  FHTMLObject := AValue;
end;

procedure TPLCSSTransition.CleanUp;
begin
  FHTMLObject := nil;

end;

constructor TPLCSSTransition.Create(AObject: TPLHTMLObject);
begin
  inherited Create;

  HTMLObject := AObject;
end;

procedure TPLCSSTransition.Start;
begin
  if FRunning then exit;
  FRunning := true;

  FStarted := Now;
end;

procedure TPLCSSTransition.Cancel;
begin
  if not FRunning then exit;
  FRunning := false;

  if Assigned(FOnCancel) then FOnCancel(FTProp, FTPseudo, SecondOf(Now - FStarted));
end;

procedure TPLCSSTransition.&End;
begin
  if not FRunning then exit;
  FRunning := false;


end;

end.

