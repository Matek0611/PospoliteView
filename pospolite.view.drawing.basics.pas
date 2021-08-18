unit Pospolite.View.Drawing.Basics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, FPimage, strutils, LazUTF8, Pospolite.View.Basics,
  Pospolite.View.CSS.Declaration, math;

type

  // https://developer.mozilla.org/en-US/docs/Web/CSS/color_value

  TPLColorType = (ctHex, ctRGBWithoutAlpha, ctRGBWithAlpha, ctHSLWithoutAlpha,
    ctHSLWithAlpha);

  { TPLColor }

  TPLColor = packed record
  strict private
    FR, FG, FB, FA: Byte;

    function CSSExtract(AColor: TPLString): TPLCSSPropertyValuePartFunction;
    function GuessType(AF: TPLCSSPropertyValuePartFunction): TPLColorType;
    function GuessIfSpaceSeparated(AF: TPLCSSPropertyValuePartFunction): TPLBool; inline;

    procedure SetColor(AValue: TPLString);
    procedure SetColorParsed(AFunction: TPLCSSPropertyValuePartFunction;
      AFree: TPLBool = true);
  public
    constructor Create(AValue: TPLString);
    constructor Create(ARed, AGreen, ABlue: Byte; AAlpha: Byte = 255);

    class operator :=(AValue: TPLString) r: TPLColor; inline;
    class operator :=(AValue: TColor) r: TPLColor;
    class operator :=(AValue: TFPColor) r: TPLColor;
    class operator :=(AValue: TPLCSSPropertyValuePartFunction) r: TPLColor;
    class operator :=(AValue: TPLColor) r: TColor; inline;
    class operator :=(AValue: TPLColor) r: TFPColor;
    class operator =(AColor1, AColor2: TPLColor) r: TPLBool; inline;

    function ToString(AType: TPLColorType = ctHex; ASpaceSeparated: TPLBool = false): TPLString;
    function Mix(AColor: TPLColor; APercent: TPLFloat): TPLColor;
    function ChangeLightness(APercent: TPLFloat = 0): TPLColor; // %: + lighten, - darken

    class function Black: TPLColor; static; inline;
    class function White: TPLColor; static; inline;
    class function Transparent: TPLColor; static; inline;

    property Red: Byte read FR write FR;
    property Green: Byte read FG write FG;
    property Blue: Byte read FB write FB;
    property Alpha: Byte read FA write FA;
  end;

  PPLColor = ^TPLColor;
  PPLPixel = PPLColor;

  { TPLPoint }

  generic TPLPoint<T> = packed record
  strict private
    FX, FY: T;
  public
    constructor Create(AX, AY: T);
    function Empty: TPLPoint; inline;
    function IsEmpty: TPLBool; inline;
    function Rotate(APivot: TPLPoint; AAngleDeg: TPLFloat): TPLPoint;
    function RotateClockwise(APivot: TPLPoint; AAngleDeg: TPLFloat): TPLPoint;

    class operator :=(p: TPoint) r: TPLPoint; inline;
    class operator :=(p: TPLPoint) r: TPoint; inline;
    class operator not(p: TPLPoint) r: TPLPoint; inline;
    class operator =(a, b: TPLPoint) r: TPLBool; inline;
    class operator -(a, b: TPLPoint) r: TPLPoint; inline;
    class operator +(a, b: TPLPoint) r: TPLPoint; inline;
    class operator >< (a, b: TPLPoint) r: TPLFloat; inline; // distance

    property X: T read FX write FX;
    property Y: T read FY write FY;
  end;

  TPLPointI = specialize TPLPoint<TPLInt>;
  TPLPointF = specialize TPLPoint<TPLFloat>;
  TPLPointS = specialize TPLPoint<TPLShortInt>;

  operator := (p: TPLPointF) r: TPLPointI; inline;
  operator := (p: TPLPointI) r: TPLPointF; inline;
  operator := (p: TPLPointS) r: TPLPointI; inline;
  operator := (p: TPLPointI) r: TPLPointS; inline;
  operator * (p: TPLPointF; q: TPLFloat) r: TPLPointF; inline;
  operator * (q: TPLFloat; p: TPLPointF) r: TPLPointF; inline;

type

  { TPLRect }

  generic TPLRect<T> = packed record
  private type
    TPLPointT = specialize TPLPoint<T>;
  strict private
    FLeft, FTop, FWidth, FHeight: T;
    function GetBottom: T;
    function GetBottomLeft: TPLPointT;
    function GetBottomRight: TPLPointT;
    function GetMiddle: TPLPointT;
    function GetRight: T;
    function GetTopLeft: TPLPointT;
    function GetTopRight: TPLPointT;
    procedure SetBottom(AValue: T);
    procedure SetRight(AValue: T);
  public
    constructor Create(ALeft, ATop, AWidth, AHeight: T);
    function IsEmpty: TPLBool; inline;
    function Inflate(AX, AY: T): TPLRect; overload;
    function Inflate(ALeft, ATop, AWidth, AHeight: T): TPLRect; overload;

    class operator =(a, b: TPLRect) r: TPLBool; inline;
    class operator in(a, b: TPLRect) r: TPLBool; inline;
    class operator in(a: TPLPointT; b: TPLRect) r: TPLBool; inline;
    class operator **(a: TPLRect; b: TPLPointT) r: TPLRect; // center
    class operator :=(a: TPLRect) r: TRect; inline;
    class operator :=(a: TRect) r: TPLRect; inline;

    property Left: T read FLeft write FLeft;
    property Top: T read FTop write FTop;
    property Right: T read GetRight write SetRight;
    property Bottom: T read GetBottom write SetBottom;
    property Width: T read FWidth write FWidth;
    property Height: T read FHeight write FHeight;
    property TopLeft: TPLPointT read GetTopLeft;
    property TopRight: TPLPointT read GetTopRight;
    property BottomLeft: TPLPointT read GetBottomLeft;
    property BottomRight: TPLPointT read GetBottomRight;
    property Middle: TPLPointT read GetMiddle;
  end;

  TPLRectI = specialize TPLRect<TPLInt>;
  TPLRectF = specialize TPLRect<TPLFloat>;

  operator := (p: TPLRectF) r: TPLRectI; inline;
  operator := (p: TPLRectI) r: TPLRectF; inline;

implementation

procedure RGB2HSL(ARed, AGreen, ABlue: byte; out AHue, ASaturation, ALightness: TPLFloat);
var
  r, g, b, cmin, cmax, delta: TPLFloat;
begin
  r := ARed/255;
  g := AGreen/255;
  b := ABlue/255;
  cmin := Min(Min(r, g), b);
  cmax := Max(Max(r, g), b);
  delta := cmax-cmin;

  AHue := 0;
  ASaturation := 0;
  ALightness := 0;

  if delta = 0 then AHue := 0 else
  if cmax = r then AHue := fmod((g-b)/delta, 6) else
  if cmax = g then AHue := (b-r)/delta+2 else
    AHue := (r-g)/delta+4;

  AHue := RoundTo(AHue*60, -1);
  if AHue < 0 then AHue := AHue+360;

  ALightness := (cmax+cmin)/2;

  if delta = 0 then ASaturation := 0 else ASaturation := delta/(1-abs(2*ALightness-1));

  ASaturation := RoundTo(ASaturation*100, -1);
  ALightness := RoundTo(ALightness*100, -1);
end;

procedure HSL2RGB(AHue, ASaturation, ALightness: TPLFloat; out ARed, AGreen, ABlue: byte);
var
  h, s, l, x, c, m, r, g, b: TPLFloat;
begin
  h := AHue;
  s := ASaturation/100;
  l := ALightness/100;

  c := (1-abs(2*l-1))*s;
  x := c*(1-abs(fmod(h/60, 2) -1));
  m := l-c/2;

  ARed := 0;
  AGreen := 0;
  ABlue := 0;
  r := 0;
  g := 0;
  b := 0;

  if ((0 <= h) or (h = 360)) and (h < 60) then begin
    r := c;
    g := x;
    b := 0;
  end else if (60 <= h) and (h < 120) then begin
    r := x;
    g := c;
    b := 0;
  end else if (120 <= h) and (h < 180) then begin
    r := 0;
    g := c;
    b := x;
  end else if (180 <= h) and (h < 240) then begin
    r := 0;
    g := x;
    b := c;
  end else if (240 <= h) and (h < 300) then begin
    r := x;
    g := 0;
    b := c;
  end else if (300 <= h) and (h < 360) then begin
    r := c;
    g := 0;
    b := x;
  end;

  ARed := round((r+m)*255);
  AGreen := round((g+m)*255);
  ABlue := round((b+m)*255);
end;

{ TPLColor }

function TPLColor.CSSExtract(AColor: TPLString
  ): TPLCSSPropertyValuePartFunction;
var
  list: TPLCSSPropertyValueParts;
  spart: string = '';
  i: integer;
begin
  list := TPLCSSPropertyValueParts.Create(false);
  try
    TPLCSSPropertyParser.ParsePropertyValue(AColor, list);

    if (list.Count = 1) and (list[0] is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      for i := low(STANDARD_HTML_COLORS) to High(STANDARD_HTML_COLORS) do begin
        if STANDARD_HTML_COLORS[i][0].ToLower = list[0].AsString.ToLower then begin
          spart := STANDARD_HTML_COLORS[i][1];
          break;
        end;
      end;

      if spart.IsEmpty then Result := TPLCSSPropertyValuePartFunction.Create('rgb(0 0 0)') else begin
        list.FreeObjects := true;
        list.Clear;
        list.FreeObjects := false;

        TPLCSSPropertyParser.ParsePropertyValue(spart, list);
        Result := list[0] as TPLCSSPropertyValuePartFunction;
        list.Clear;
      end;
    end else if (list.Count = 1) and (list[0] is TPLCSSPropertyValuePartFunction) and
      AnsiMatchStr(TPLCSSPropertyValuePartFunction(list[0]).Name.ToLower,
      ['#', 'rgb', 'rgba', 'hsl', 'hsla']) and
      (TPLCSSPropertyValuePartFunction(list[0]).Arguments.Count > 0) then begin
      Result := list[0] as TPLCSSPropertyValuePartFunction;
      list.Remove(list[0]);
    end else Result := TPLCSSPropertyValuePartFunction.Create('rgb(0 0 0)');
  finally
    list.FreeObjects := true;
    list.Free;
  end;
end;

function TPLColor.GuessType(AF: TPLCSSPropertyValuePartFunction): TPLColorType;
begin
  case AF.Name.ToLower of
    '#': Result := ctHex;
    'rgb', 'rgba': if AF.Arguments.Count > 3 then Result := ctRGBWithAlpha else Result := ctRGBWithoutAlpha;
    'hsl', 'hsla': if AF.Arguments.Count > 3 then Result := ctHSLWithAlpha else Result := ctHSLWithoutAlpha;
    else Result := ctHex;
  end;
end;

function TPLColor.GuessIfSpaceSeparated(AF: TPLCSSPropertyValuePartFunction
  ): TPLBool;
begin
  Result := (AF.Arguments.Count = 5) and (AF.Arguments[3] is TPLCSSPropertyValuePartStringOrIdentifier);
end;

procedure TPLColor.SetColor(AValue: TPLString);
begin
  SetColorParsed(CSSExtract(AValue));
end;

procedure TPLColor.SetColorParsed(AFunction: TPLCSSPropertyValuePartFunction;
  AFree: TPLBool);
var
  func: TPLCSSPropertyValuePartFunction absolute AFunction;
  issep: TPLBool;
  typ: TPLColorType;
  pom: TPLString;

  procedure SetDefault;
  begin
    FR := 0;
    FG := 0;
    FB := 0;
    FA := 255;
  end;

  function GetRGBArgVal(AArg: TPLCSSPropertyValuePart): Byte;
  var
    w: TPLFloat;
  begin
    Result := 0;

    if AArg is TPLCSSPropertyValuePartNumber then begin
      w := TPLCSSPropertyValuePartNumber(AArg).Value;
      if w < 0 then w := 0 else if w > 255 then w := 255;

      Result := round(w);
    end else if (AArg is TPLCSSPropertyValuePartDimension) and (TPLCSSPropertyValuePartDimension(AArg).&Unit = '%') then begin
      w := (TPLCSSPropertyValuePartDimension(AArg).Value / 100) * 255;
      if w < 0 then w := 0 else if w > 255 then w := 255;

      Result := round(w);
    end;
  end;

  function GetAlphaVal(AArg: TPLCSSPropertyValuePart): Byte;
  var
    w: TPLFloat;
  begin
    Result := 0;

    if AArg is TPLCSSPropertyValuePartNumber then begin
      w := TPLCSSPropertyValuePartNumber(AArg).Value * 255;
      if w < 0 then w := 0 else if w > 255 then w := 255;

      Result := round(w);
    end else if (AArg is TPLCSSPropertyValuePartDimension) and (TPLCSSPropertyValuePartDimension(AArg).&Unit = '%') then begin
      w := (TPLCSSPropertyValuePartDimension(AArg).Value / 100) * 255;
      if w < 0 then w := 0 else if w > 255 then w := 255;

      Result := round(w);
    end;
  end;

var
  h, s, l: TPLFloat;
begin
  //func := CSSExtract(AValue);
  try
    issep := GuessIfSpaceSeparated(func);
    typ := GuessType(func);

    case typ of
      ctHex: begin
        pom := func.Arguments[0].AsString;
        case pom.Length of
          3, 4: begin
            FR := (pom[1] * 2).FromHex;
            FG := (pom[2] * 2).FromHex;
            FB := (pom[3] * 2).FromHex;
            if pom.Length = 4 then FA := (pom[4] * 2).FromHex else FA := 255;
          end;
          6, 8: begin
            FR := pom.SubStr(1, 2).FromHex;
            FG := pom.SubStr(3, 2).FromHex;
            FB := pom.SubStr(5, 2).FromHex;
            if pom.Length = 8 then FA := pom.SubStr(7, 2).FromHex else FA := 255;
          end

          else SetDefault;
        end;
      end;
      ctRGBWithoutAlpha, ctRGBWithAlpha: begin
        if (func.Arguments.Count < 3) or (func.Arguments.Count > 5) then SetDefault else begin
          FR := GetRGBArgVal(func.Arguments[0]);
          FG := GetRGBArgVal(func.Arguments[1]);
          FB := GetRGBArgVal(func.Arguments[2]);

          if typ = ctRGBWithAlpha then FA := GetAlphaVal(func.Arguments[ifthen(issep, 4, 3)]) else FA := 255;
        end;
      end;
      ctHSLWithoutAlpha, ctHSLWithAlpha: begin
        if (func.Arguments.Count < 3) or (func.Arguments.Count > 5) then SetDefault else begin
          if func.Arguments[0] is TPLCSSPropertyValuePartNumber then h := AngleDeg(TPLCSSPropertyValuePartNumber(func.Arguments[0]).Value) else
            if func.Arguments[0] is TPLCSSPropertyValuePartDimension then h := AngleDeg(TPLCSSPropertyValuePartDimension(func.Arguments[0]).Value, TPLCSSPropertyValuePartDimension(func.Arguments[0]).&Unit) else
              h := 0;

          if (func.Arguments[1] is TPLCSSPropertyValuePartDimension) and (TPLCSSPropertyValuePartDimension(func.Arguments[1]).&Unit = '%') then begin
            s := TPLCSSPropertyValuePartDimension(func.Arguments[1]).Value;
            if s > 100 then s := 100;
            if s < 0 then s := 0;
          end else s := 0;

          if (func.Arguments[2] is TPLCSSPropertyValuePartDimension) and (TPLCSSPropertyValuePartDimension(func.Arguments[2]).&Unit = '%') then begin
            l := TPLCSSPropertyValuePartDimension(func.Arguments[2]).Value;
            if l > 100 then l := 100;
            if l < 0 then l := 0;
          end else l := 0;

          HSL2RGB(h, s, l, FR, FG, FB);

          if typ = ctHSLWithAlpha then FA := GetAlphaVal(func.Arguments[ifthen(issep, 4, 3)]) else FA := 255;
        end;
      end;
    end;
  finally
    if AFree then func.Free;
  end;
end;

constructor TPLColor.Create(AValue: TPLString);
begin
  SetColor(AValue);
end;

constructor TPLColor.Create(ARed, AGreen, ABlue: Byte; AAlpha: Byte);
begin
  FR := ARed;
  FG := AGreen;
  FB := ABlue;
  FA := AAlpha;
end;

class operator TPLColor.:=(AValue: TPLString)r: TPLColor;
begin
  r := TPLColor.Create(AValue);
end;

class operator TPLColor.:=(AValue: TColor) r: TPLColor;
begin
  r := Default(TPLColor);
  r.FR := Graphics.Red(AValue);
  r.FG := Graphics.Green(AValue);
  r.FB := Graphics.Blue(AValue);
  r.FA := 255;
end;

class operator TPLColor.:=(AValue: TFPColor) r: TPLColor;
var
  c: TColor;
begin
  r := Default(TPLColor);
  c := FPColorToTColor(AValue);

  r.Red := Graphics.Red(c);
  r.Green := Graphics.Green(c);
  r.Blue := Graphics.Blue(c);
  r.Alpha := round(AValue.alpha / $FFFF * 255);
end;

class operator TPLColor.:=(AValue: TPLCSSPropertyValuePartFunction) r: TPLColor;
begin
  r := Default(TPLColor);
  r.SetColorParsed(AValue, false);
end;

class operator TPLColor.:=(AValue: TPLColor) r: TColor;
begin
  r := RGBToColor(AValue.FR, AValue.FG, AValue.FB);
end;

class operator TPLColor.:=(AValue: TPLColor) r: TFPColor;
begin
  r := TColorToFPColor(RGBToColor(AValue.FR, AValue.FG, AValue.FB));
  r.Alpha := round((AValue.Alpha / 255) * $FFFF);
end;

class operator TPLColor.=(AColor1, AColor2: TPLColor) r: TPLBool;
begin
  r := (AColor1.Red = AColor2.Red) and (AColor1.Green = AColor2.Green) and
    (AColor1.Blue = AColor2.Blue) and (AColor1.Alpha = AColor2.Alpha);
end;

function TPLColor.ToString(AType: TPLColorType; ASpaceSeparated: TPLBool
  ): TPLString;
var
  h, s, l: TPLFloat;
begin
  case AType of
    ctHex: Result := '#' + FR.ToHexString(2) + FG.ToHexString(2) + FB.ToHexString(2) + IfThen(FA < 255, FA.ToHexString(2), '');
    ctRGBWithoutAlpha:
      if ASpaceSeparated then Result := 'rgb(' + FR.ToString + ' ' + FG.ToString + ' ' + FB.ToString + ')' else
        Result := 'rgb(' + FR.ToString + ', ' + FG.ToString + ', ' + FB.ToString + ')';
    ctRGBWithAlpha:
      if ASpaceSeparated then Result := 'rgb(' + FR.ToString + ' ' + FG.ToString + ' ' + FB.ToString + ' / ' + RoundTo(FA / 255, -2).ToString(PLFormatSettingsDef) + ')' else
        Result := 'rgb(' + FR.ToString + ', ' + FG.ToString + ', ' + FB.ToString + ', ' + RoundTo(FA / 255, -2).ToString(PLFormatSettingsDef) + ')';
    ctHSLWithAlpha, ctHSLWithoutAlpha: begin
      RGB2HSL(FR, FG, FB, h, s, l);

      Result := 'hsl(';

      if ASpaceSeparated then Result += h.ToString(PLFormatSettingsDef) + 'deg ' + s.ToString(PLFormatSettingsDef) + '% ' + l.ToString(PLFormatSettingsDef) + '%' else
        Result += h.ToString(PLFormatSettingsDef) + 'deg, ' + s.ToString(PLFormatSettingsDef) + '%, ' + l.ToString(PLFormatSettingsDef) + '%';

      if AType = ctHSLWithAlpha then begin
        if ASpaceSeparated then Result += ' / ' else Result += ', ';
        Result += RoundTo(FA / 255, -2).ToString(PLFormatSettingsDef);
      end;

      Result += ')';
    end;
  end;
end;

function TPLColor.Mix(AColor: TPLColor; APercent: TPLFloat): TPLColor;
var
  r, g, b: integer;
  a: TPLFloat;
begin
  r := trunc((AColor.FR - FR) * APercent);
  g := trunc((AColor.FG - FG) * APercent);
  b := trunc((AColor.FB - FB) * APercent);
  a := (AColor.FA - FA) * APercent;

  Result := '#' + hexStr(FR + r, 2) + hexStr(FG + g, 2) + hexStr(FB + b, 2) + hexStr(round(255 * (FA + a)), 2);
end;

function TPLColor.ChangeLightness(APercent: TPLFloat): TPLColor;
var
  h, s, l: TPLFloat;
begin
  if APercent = 0 then exit(self);

  Result := self;

  RGB2HSL(FR, FG, FB, h, s, l);
  l := l + APercent * l;
  HSL2RGB(h, s, l, Result.FR, Result.FG, Result.FB);
end;

class function TPLColor.Black: TPLColor;
begin
  Result := TPLColor.Create(0, 0, 0);
end;

class function TPLColor.White: TPLColor;
begin
  Result := TPLColor.Create(255, 255, 255);
end;

class function TPLColor.Transparent: TPLColor;
begin
  Result := TPLColor.Create(0, 0, 0, 0);
end;

{ TPLPoint }

constructor TPLPoint.Create(AX, AY: T);
begin
  FX := AX;
  FY := AY;
end;

function TPLPoint.Empty: TPLPoint;
begin
  Result := Create(0, 0);
end;

function TPLPoint.IsEmpty: TPLBool;
begin
  Result := (FX = 0) and (FY = 0);
end;

function TPLPoint.Rotate(APivot: TPLPoint; AAngleDeg: TPLFloat): TPLPoint;
var
  s, c: TPLFloat;
begin
  AAngleDeg := degtorad(AAngleDeg);
  s := sin(AAngleDeg);
  c := cos(AAngleDeg);

  Result := Self - APivot;
  Result := TPLPoint.Create(Variant(Result.X * c - Result.Y * s), Variant(Result.X * s + Result.Y * c));
  Result := Result + APivot;
end;

function TPLPoint.RotateClockwise(APivot: TPLPoint; AAngleDeg: TPLFloat): TPLPoint;
var
  s, c: TPLFloat;
begin
  AAngleDeg := degtorad(AAngleDeg);
  s := sin(AAngleDeg);
  c := cos(AAngleDeg);

  Result := Self - APivot;
  Result := TPLPoint.Create(Variant(Result.X * c + Result.Y * s), Variant(Result.X * s - Result.Y * c));
  Result := Result + APivot;
end;

class operator TPLPoint.><(a, b: TPLPoint) r: TPLFloat;
begin
  r := sqrt(power(a.X - b.X, 2) + power(a.Y - b.Y, 2));
end;

class operator TPLPoint.-(a, b: TPLPoint) r: TPLPoint;
begin
  r := TPLPoint.Create(a.X - b.X, a.Y - b.Y);
end;

class operator TPLPoint.+(a, b: TPLPoint) r: TPLPoint;
begin
  r := TPLPoint.Create(a.X + b.X, a.Y + b.Y);
end;

class operator TPLPoint.:=(p: TPoint) r: TPLPoint;
begin
  r := TPLPoint.Create(p.x, p.y);
end;

class operator TPLPoint.:=(p: TPLPoint) r: TPoint;
begin
  r := TPoint.Create(round(TPLFloat(p.FX)), round(TPLFloat(p.FY)));
end;

class operator TPLPoint.not(p: TPLPoint) r: TPLPoint;
begin
  r := TPLPoint.Create(-p.FX, -p.FY);
end;

class operator TPLPoint.=(a, b: TPLPoint) r: TPLBool;
begin
  r := (a.FX = b.FX) and (a.FY = b.FY);
end;

{ TPLRect }

function TPLRect.GetTopLeft: TPLPointT;
begin
  Result := TPLPointT.Create(Left, Top);
end;

function TPLRect.GetBottomLeft: TPLPointT;
begin
  Result := TPLPointT.Create(Left, Top + Height);
end;

function TPLRect.GetBottom: T;
begin
  Result := Top + Height;
end;

function TPLRect.GetBottomRight: TPLPointT;
begin
  Result := TPLPointT.Create(Left + Width, Top + Height);
end;

function TPLRect.GetMiddle: TPLPointT;
begin
  Result := TPLPointT.Create(Left + T(Width / 2), Top + T(Height / 2));
end;

function TPLRect.GetRight: T;
begin
  Result := Left + Width;
end;

function TPLRect.GetTopRight: TPLPointT;
begin
  Result := TPLPointT.Create(Left + Width, Top);
end;

procedure TPLRect.SetBottom(AValue: T);
begin
  Height := AValue - Top;
end;

procedure TPLRect.SetRight(AValue: T);
begin
  Width := AValue - Left;
end;

constructor TPLRect.Create(ALeft, ATop, AWidth, AHeight: T);
begin
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

function TPLRect.IsEmpty: TPLBool;
begin
  Result := (Width < 1) or (Height < 1);
end;

function TPLRect.Inflate(AX, AY: T): TPLRect;
begin
  Result := TPLRect.Create(FLeft + AX, FTop + AY, FWidth - AX * 2, FHeight - AY * 2);
end;

function TPLRect.Inflate(ALeft, ATop, AWidth, AHeight: T): TPLRect;
begin
  Result := TPLRect.Create(FLeft + ALeft, FTop + ATop, FWidth - AWidth, FHeight - AHeight);
end;

class operator TPLRect.**(a: TPLRect; b: TPLPointT) r: TPLRect;
begin
  r := a;
  r.FLeft := b.X - T(r.Width / 2);
  r.FTop := b.Y - T(r.Height / 2);
end;

class operator TPLRect.:=(a: TPLRect) r: TRect;
begin
  r := TRect.Create(round(TPLFloat(a.Left)), round(TPLFloat(a.Top)), round(TPLFloat(a.Right)), round(TPLFloat(a.Bottom)));
end;

class operator TPLRect.:=(a: TRect) r: TPLRect;
begin
  r := TPLRect.Create(a.Left, a.Top, a.Width, a.Height);
end;

class operator TPLRect.=(a, b: TPLRect) r: TPLBool;
begin
  r := (a.Left = b.Left) and (a.Top = b.Top) and (a.Width = b.Width) and (a.Height = b.Height);
end;

class operator TPLRect.in(a, b: TPLRect) r: TPLBool;
begin
  r := (a.GetTopLeft in b) and (a.GetTopRight in b) and (a.GetBottomLeft in b)
    and (a.GetBottomRight in b);
end;

class operator TPLRect.in(a: TPLPointT; b: TPLRect) r: TPLBool;
begin
  Result := not b.IsEmpty and (b.Left <= a.X) and (b.Left + b.Width > a.X) and
    (b.Top <= a.Y) and (b.Top + b.Height > a.Y);
end;

operator :=(p: TPLPointF) r: TPLPointI;
begin
  r := TPLPointI.Create(round(p.X), round(p.Y));
end;

operator :=(p: TPLPointI) r: TPLPointF;
begin
  r := TPLPointF.Create(p.X, p.Y);
end;

operator :=(p: TPLPointS) r: TPLPointI;
begin
  r := TPLPointI.Create(p.X, p.Y);
end;

operator :=(p: TPLPointI) r: TPLPointS;
begin
  r := TPLPointS.Create(p.X, p.Y);
end;

operator * (p: TPLPointF; q: TPLFloat) r: TPLPointF;
begin
  r := TPLPointF.Create(p.X * q, p.Y * q);
end;

operator * (q: TPLFloat; p: TPLPointF) r: TPLPointF;
begin
  r := TPLPointF.Create(p.X * q, p.Y * q);
end;

operator :=(p: TPLRectF) r: TPLRectI;
begin
  r := TPLRectI.Create(round(p.Left), round(p.Top), round(p.Width), round(p.Height));
end;

operator :=(p: TPLRectI) r: TPLRectF;
begin
  r := TPLRectF.Create(p.Left, p.Top, p.Width, p.Height);
end;

end.

