unit Pospolite.View.CSS.Animation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Pospolite.View.Basics, Pospolite.View.Drawing.Basics;

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

end.

