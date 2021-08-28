unit PospoLiteHTML.CSS.Values;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, math, strutils, BGRABitmap, BGRAPath,
  BGRABitmapTypes, BGRACanvas2D, PospoLiteHTML.CSS.Basics, PospoLiteHTML.Cache,
  FPCanvas;

type

  { TPLCSSColor }

  TPLCSSColor = record
  strict private
    FAsHex: string;
    FAsHSL: string;
    FAsHSLA: string;
    FAsRGB: string;
    FAsRGBA: string;
    FRed, FGreen, FBlue: byte;
    FHue, FSaturation, FLightness: TPLCSSFloat;
    FAlpha: TPLCSSFloat;
    FNotifySimpleHex: boolean;
    function GetAsName: string;
    function GetAsPascalColor: TColor;
    function GetFullValue: TPLCSSInt;
    procedure Update;
    function SimpleToNormalFormat(AValue: string): string;
  public
    constructor Create(AValue: string);
    constructor Create(AValue: TPLCSSInt);
    constructor Create(AValue: TPLCSSColor);

    // set color
    function SetColor(AValue: string): boolean;
    // set default color (black)
    procedure SetDefaultColor;
    // get another color format (rgb/hsl(RR GG BB / AA) e.g. rgb(100 20 0) or rgb(100 20 0 / 1))
    function AsSimpleFormat(AFormat: TPLCSSColorSimpleFormat = csfRGB;
      AIsAlphaIncluded: boolean = true): string;
    // add color (+RR, +GG, +BB, +AA), e.g. rgba(12, 10, 10, .2) + rgba(1, 0, 3, .1) = rgba(13, 10, 13, .3)
    procedure AddColor(AColor: TPLCSSColor);
    // return mixed color
    function MixWithColor(AColor: TPLCSSColor; APercent: TPLCSSFloat): TPLCSSColor;

    class operator = (a, b: TPLCSSColor) r: boolean;
    class operator > (a, b: TPLCSSColor) r: boolean;

    // #RGB or #RRGGBB or #RRGGBBAA e.g. #FFAABB
    property AsHex: string read FAsHex;
    // rgb(RR, GG, BB) e.g. rgb(100, 20, 0)
    property AsRGB: string read FAsRGB;
    // hsl(RR, GG%, BB%) e.g. hsl(0, 100%, 50%)
    property AsHSL: string read FAsHSL;
    // rgba(RR, GG, BB, AA) e.g. rgb(100, 20, 0, .5) or rgb(100, 20, 0, 0.5)
    property AsRGBA: string read FAsRGBA;
    // hsla(RR, GG%, BB%, AA) e.g. hsl(0, 100%, 50%, 0.66)
    property AsHSLA: string read FAsHSLA;
    // HTML color to Pascal color (without alpha)
    property AsPascalColor: TColor read GetAsPascalColor;
    // if this color is a standard color, get its name (if not, result is an empty string)
    property AsName: string read GetAsName;
    // color alpha
    property Alpha: TPLCSSFloat read FAlpha;
    // full int value with alpha
    property FullValue: TPLCSSInt read GetFullValue;
  end;

  function PLHTMLColor(AValue: string): TPLCSSColor;
  function BGRAColorFromCSS(AColor: TPLCSSColor): TBGRAPixel;
  operator := (a: string) r: TPLCSSColor;
  operator := (a: TPLCSSInt) r: TPLCSSColor;

type

  { TPLCSSTimeUnit }

  PPLCSSTimeUnit = ^TPLCSSTimeUnit;
  TPLCSSTimeUnit = record
  strict private
    FSeconds: boolean;
    FValue: TPLCSSFloat;
  public
    constructor Create(AValue: string);

    function AsMilliseconds: TPLCSSInt;
    class operator := (a: TPLCSSTimeUnit) r: TPLCSSInt;
    class operator := (a: string) r: TPLCSSTimeUnit;
    class operator = (a, b: TPLCSSTimeUnit) r: boolean;

    property Value: TPLCSSFloat read FValue write FValue;
    property Seconds: boolean read FSeconds write FSeconds;
  end;

type

  { TPLCSSLength }

  PPLCSSLength = ^TPLCSSLength;
  TPLCSSLength = record
  strict private
    FUnit: TPLCSSUnit;
    FValue: TPLCSSFloat;
  public
    constructor Create(AValue: TPLCSSFloat; AUnit: TPLCSSUnit);
    constructor Create(AValue: string);

    class operator = (a, b: TPLCSSLength) r: boolean;

    property LValue: TPLCSSFloat read FValue write FValue;
    property LUnit: TPLCSSUnit read FUnit write FUnit;
  end;

  function PLCSSLength(AValue: string): TPLCSSLength;
  // CSS Length to real size in pixels
  function CSSLengthToScreenF(AValue: TPLCSSLength; ASelf: IPLCSSCustomControl = nil; AProperty: string = ''): TPLCSSFloat;
  function CSSLengthToScreen(AValue: TPLCSSLength; ASelf: IPLCSSCustomControl = nil; AProperty: string = ''): TPLCSSInt;
  operator := (a: TPLCSSLength) r: TPLCSSFloat;
  operator := (a: string) r: TPLCSSLength;

type

  { TPLCSSMargins }

  PPLCSSMargins = ^TPLCSSMargins;
  TPLCSSMargins = record
  strict private
    FBottom: TPLCSSLength;
    FLeft: TPLCSSLength;
    FRight: TPLCSSLength;
    FTop: TPLCSSLength;
  public
    constructor Create(AValue: string);

    class operator = (a, b: TPLCSSMargins) r: boolean;

    property Top: TPLCSSLength read FTop write FTop;
    property Left: TPLCSSLength read FLeft write FLeft;
    property Bottom: TPLCSSLength read FBottom write FBottom;
    property Right: TPLCSSLength read FRight write FRight;
  end;

  TPLCSSPaddings = TPLCSSMargins;

  { TPLCSSBorderRadius }

  PPLCSSBorderRadius = ^TPLCSSBorderRadius;
  TPLCSSBorderRadius = record
  strict private
    FBottomLeft: TPLCSSLength;
    FBottomRight: TPLCSSLength;
    FTopLeft: TPLCSSLength;
    FTopRight: TPLCSSLength;
  public
    //constructor Create(ASelector: TPLCSSSelector);

    property TopLeft: TPLCSSLength read FTopLeft write FTopLeft;
    property TopRight: TPLCSSLength read FTopRight write FTopRight;
    property BottomLeft: TPLCSSLength read FBottomLeft write FBottomLeft;
    property BottomRight: TPLCSSLength read FBottomRight write FBottomRight;
  end;

  { TPLCSSBorderPart }

  PPLCSSBorderPart = ^TPLCSSBorderPart;
  TPLCSSBorderPart = record
  strict private
    FColor: TPLCSSColor;
    FStyle: string;
    FWidth: TPLCSSLength;
    procedure SetStyle(AValue: string);
  public
    //constructor Create(ASelector: TPLCSSSelector; AType: string = 'left');

    property Color: TPLCSSColor read FColor write FColor;
    property Style: string read FStyle write SetStyle;
    property Width: TPLCSSLength read FWidth write FWidth;
  end;

  { TPLCSSBorder }

  PPLCSSBorder = ^TPLCSSBorder;
  TPLCSSBorder = record
  private
    FBottom: TPLCSSBorderPart;
    FLeft: TPLCSSBorderPart;
    FRadius: TPLCSSBorderRadius;
    FRight: TPLCSSBorderPart;
    FTop: TPLCSSBorderPart;
  public
    //constructor Create(AValue: TPLCSSSelector);

    procedure Draw(ABitmap: TBGRABitmap; ARect: TRect; ASelf: IPLCSSCustomControl);
    function GetBorders(ARect: TRect; ASelf: IPLCSSCustomControl = nil; AType: string = 'left'): TBGRAPath;

    property Left: TPLCSSBorderPart read FLeft write FLeft;
    property Top: TPLCSSBorderPart read FTop write FTop;
    property Right: TPLCSSBorderPart read FRight write FRight;
    property Bottom: TPLCSSBorderPart read FBottom write FBottom;
    property Radius: TPLCSSBorderRadius read FRadius write FRadius;
  end;

  { TPLCSSBackground }

  PPLCSSBackground = ^TPLCSSBackground;
  TPLCSSBackground = record
  strict private
    FAttachment: string;
    FBlendMode: string;
    FClip: string;
    FColor: TPLCSSColor;
    FImage: string;
    FLocation: string;
    FOrigin: string;
    FPosition: string;
    FRepeating: string;
    FSize: string;
    FImageCache: TBGRABitmap;
    FBorder: TPLCSSBorder;
    FOpacity: TPLCSSFloat;
    procedure SetAttachment(AValue: string);
    procedure SetBlendMode(AValue: string);
    procedure SetClip(AValue: string);
    procedure SetColor(AValue: TPLCSSColor);
    procedure SetOrigin(AValue: string);
    procedure SetRepeating(AValue: string);
  public
    //constructor Create(AValue: TPLCSSSelector);
    // bg-color bg-image position/bg-size bg-repeat bg-origin bg-clip bg-attachment initial[|inherit]
    //procedure SetBackground(AValue: TPLCSSSelector);

    procedure Draw(ABitmap: TBGRABitmap; ARect: TRect; ASelf: IPLCSSCustomControl);

    // scroll|fixed|local|initial[|inherit]
    property Attachment: string read FAttachment write SetAttachment;
    // normal|multiply|screen|overlay|darken|lighten|color-dodge|saturation|color|luminosity[|initial]
    property BlendMode: string read FBlendMode write SetBlendMode;
    // border-box|padding-box|content-box|initial[|inherit]
    property Clip: string read FClip write SetClip;
    // color|transparent|initial[|inherit]
    property Color: TPLCSSColor read FColor write SetColor;
    // url|none|initial[|inherit]
    property Image: string read FImage write FImage;
    // padding-box|border-box|content-box|initial[|inherit]
    property Origin: string read FOrigin write SetOrigin;
    // left/right/center top/center/bottom|x% y%|xpos ypos|initial[|inherit]
    property Position: string read FPosition write FPosition;
    // repeat|repeat-x|repeat-y|no-repeat|initial[|inherit]
    property Repeating: string read FRepeating write SetRepeating;
    // auto|length|cover|contain|initial[|inherit]
    property Size: string read FSize write FSize;

    property Location: string read FLocation write FLocation;
  end;

type

  { TPLCSSTextDecoration }

  TPLCSSTextDecoration = record
  private
    FColor: TPLCSSColor;
    FLine: string;
    FStyle: string;
  public
    property Color: TPLCSSColor read FColor write FColor;
    property Line: string read FLine write FLine;
    property Style: string read FStyle write FStyle;
  end;

  { TPLCSSText }

  PPLCSSText = ^TPLCSSText;
  TPLCSSText = record
  private
    FAlign: string;
    FAlignLast: string;
    FDecoration: TPLCSSTextDecoration;
    FIndent: TPLCSSFloat;
    FOverflow: string;
    FTransform: string;
  public
    property Align: string read FAlign write FAlign;
    property AlignLast: string read FAlignLast write FAlignLast;
    property Decoration: TPLCSSTextDecoration read FDecoration write FDecoration;
    property Indent: TPLCSSFloat read FIndent write FIndent;
    property Overflow: string read FOverflow write FOverflow;
    property Transform: string read FTransform write FTransform;
  end;

implementation

uses Types;

{ TPLCSSColor }

function TPLCSSColor.GetAsPascalColor: TColor;
begin
  Result := RGBToColor(FRed, FGreen, FBlue);
end;

function TPLCSSColor.GetFullValue: TPLCSSInt;
begin
  Result := Hex2Dec(hexStr(FRed, 2) + hexStr(FGreen, 2) + hexStr(FBlue, 2) + hexStr(round(255 * FAlpha), 2));
end;

procedure TPLCSSColor.Update;
begin
  if FNotifySimpleHex then begin
    FNotifySimpleHex := false;
    FAsHex := '#'+IntToHex(FRed, 2)[1]+IntToHex(FGreen, 2)[1]+IntToHex(FBlue, 2)[1];
  end else FAsHex := '#'+IntToHex(FRed, 2)+IntToHex(FGreen, 2)+IntToHex(FBlue, 2)+ifthen(FAlpha < 1, IntToHex(round(FAlpha*255), 2));

  FAsRGB := 'rgb('+IntToStr(FRed)+', '+IntToStr(FGreen)+', '+IntToStr(FBlue);
  FAsRGBA := FAsRGB.Replace('rgb', 'rgba')+', '+FloatToStr(FAlpha, PLCSSFormatSettingsDef)+')';
  FAsRGB := FAsRGB+')';

  FAsHSL := 'hsl('+IntToStr(round(FHue))+', '+IntToStr(round(FSaturation))+'%, '+IntToStr(round(FLightness))+'%';
  FAsHSLA := FAsHSL.Replace('hsl', 'hsla')+', '+FloatToStr(FAlpha, PLCSSFormatSettingsDef)+')';
  FAsHSL := FAsHSL+')';
end;

function TPLCSSColor.SimpleToNormalFormat(AValue: string): string;

  function FToByte(x: TPLCSSFloat): TPLCSSFloat;
  begin
    if x < 0 then x := 0 else
    if x > 255 then x := 255;
    Result := x;
  end;

  function FToPercent(x: TPLCSSFloat): TPLCSSFloat;
  begin
    if x < 0 then x := 0 else
    if x > 100 then x := 100;
    Result := x;
  end;

var
  r: TRegExpr;
  a, b, c, d: TPLCSSFloat;
  s: string;
begin
  Result := '';
  AValue := AValue.Trim;
  d := -1;
  r := TRegExpr.Create('(([0-9]*[.])?[0-9]+)');
  try
    r.ModifierR:=false;
    r.ModifierG:=true;

    if r.Exec(AValue) then begin
      TryStrToFloat(r.Match[1], a, PLCSSFormatSettingsDef);
      r.ExecNext;
      TryStrToFloat(r.Match[1], b, PLCSSFormatSettingsDef);
      r.ExecNext;
      TryStrToFloat(r.Match[1], c, PLCSSFormatSettingsDef);
      r.ExecNext;

      if r.SubExprMatchCount > 0 then begin
        TryStrToFloat('0'+r.Match[1], d, PLCSSFormatSettingsDef);
        if d < 0 then d := 0 else
        if d > 1 then d := 1;
      end;

      if Copy(AValue, 1, 3) = 'rgb' then begin
        a := FToByte(a);
        b := FToByte(b);
        c := FToByte(c);
        if d > -1 then s := 'rgba' else s := 'rgb';
        Result := s+'('+IntToStr(round(a))+', '+IntToStr(round(b))+', '+IntToStr(round(c));
        if s = 'rgba' then Result := Result+', '+FloatToStr(d, PLCSSFormatSettingsDef);
        Result := Result+')';
      end else if Copy(AValue, 1, 3) = 'hsl' then begin
        if a < 0 then a := 0 else if a > 360 then a := 360;
        b := FToPercent(b);
        c := FToPercent(c);
        if d > -1 then s := 'hsla' else s := 'hsl';
        Result := s+'('+IntToStr(round(a))+', '+IntToStr(round(b))+'%, '+IntToStr(round(c))+'%';
        if s = 'hsl' then Result := Result+', '+FloatToStr(d, PLCSSFormatSettingsDef);
        Result := Result+')';
      end else begin
        r.Free;
        exit('');
      end;
    end;
  finally
    r.Free;
  end;
end;

function TPLCSSColor.GetAsName: string;
var
  i: integer;
begin
  Result := '';
  for i := Low(STANDARD_HTML_COLORS) to High(STANDARD_HTML_COLORS) do
    if STANDARD_HTML_COLORS[i, 1] = FAsHex then begin
      Result := STANDARD_HTML_COLORS[i, 0];
      break;
    end;
end;

constructor TPLCSSColor.Create(AValue: string);
begin
  SetColor(AValue);
end;

constructor TPLCSSColor.Create(AValue: TPLCSSInt);
begin
  Self := AValue;
end;

constructor TPLCSSColor.Create(AValue: TPLCSSColor);
begin
  Self := AValue.AsHex;
end;

function TPLCSSColor.SetColor(AValue: string): boolean;
var
  s, x: string;
  i: integer;
  sl: TStringList;
begin
  Result := true;
  FNotifySimpleHex := false;
  AValue := AValue.Trim;
  if Length(AValue) < 1 then begin
    Result := false;
    SetDefaultColor;
  end else if AValue[1] = '#' then begin
    AValue := UpperCase(AValue);
    AValue := Copy(AValue, 2, Length(AValue));
    for i:=1 to Length(AValue) do
      if not (AValue[i] in ['0'..'9', 'A'..'F']) then begin
        Result := false;
        SetDefaultColor;
        exit;
      end;
    case Length(AValue) of
      3: begin
        FAlpha := 1;
        FRed := Hex2Dec(AValue[1]+AValue[1]);
        FGreen := Hex2Dec(AValue[2]+AValue[2]);
        FBlue := Hex2Dec(AValue[3]+AValue[3]);
        RGB2HSL(FRed, FGreen, FBlue, FHue, FSaturation, FLightness);
        FNotifySimpleHex := true;
        Update;
      end;
      6: begin
        FAlpha := 1;
        FRed := Hex2Dec(AValue[1]+AValue[2]);
        FGreen := Hex2Dec(AValue[3]+AValue[4]);
        FBlue := Hex2Dec(AValue[5]+AValue[6]);
        RGB2HSL(FRed, FGreen, FBlue, FHue, FSaturation, FLightness);
        Update;
      end;
      8: begin
        FAlpha := Hex2Dec(AValue[7]+AValue[8])/255;
        FRed := Hex2Dec(AValue[1]+AValue[2]);
        FGreen := Hex2Dec(AValue[3]+AValue[4]);
        FBlue := Hex2Dec(AValue[5]+AValue[6]);
        RGB2HSL(FRed, FGreen, FBlue, FHue, FSaturation, FLightness);
        Update;
      end
      else begin
        Result := false;
        SetDefaultColor;
        exit;
      end;
    end;
  end else if LowerCase(Copy(AValue, 1, 3)) = 'rgb' then begin
    if Pos(',', AValue) < 1 then begin
      s := SimpleToNormalFormat(AValue);
      if s <> '' then exit(SetColor(s)) else begin SetDefaultColor; exit(false); end;
    end;
    AValue := Copy(AValue, Pos('(', AValue)+1, Pos(')', AValue)-Pos('(', AValue)-1);
    sl := TStringList.Create;
    try
      sl.Text := AValue.Replace(',', LineEnding);
      if sl.Count in [3, 4] then begin
        if not TryStrToByte(sl[0].Trim, FRed) then FRed := 0;
        if not TryStrToByte(sl[1].Trim, FGreen) then FGreen := 0;
        if not TryStrToByte(sl[2].Trim, FBlue) then FBlue := 0;
        if (sl.Count = 4) then begin
          if not TryStrToFloat('0'+sl[3].Trim, FAlpha, PLCSSFormatSettingsDef) then FAlpha := 0;
        end else FAlpha := 1;
        if FAlpha < 0 then FAlpha := 0 else
        if FAlpha > 1 then FAlpha := 1;
      end else begin
        sl.Free;
        Result := false;
        SetDefaultColor;
        exit;
      end;
    finally
      sl.Free;
    end;
    RGB2HSL(FRed, FGreen, FBlue, FHue, FSaturation, FLightness);
    Update;
  end else if LowerCase(Copy(AValue, 1, 3)) = 'hsl' then begin
    if Pos(',', AValue) < 1 then begin
      s := SimpleToNormalFormat(AValue);
      if s <> '' then exit(SetColor(s)) else begin SetDefaultColor; exit(false); end;
    end;
    AValue := Copy(AValue, Pos('(', AValue)+1, Pos(')', AValue)-Pos('(', AValue)-1);
    sl := TStringList.Create;
    try
      sl.Text := AValue.Replace(',', LineEnding);
      if sl.Count in [3, 4] then begin
        if not TryStrToFloat(sl[0].Trim, FHue, PLCSSFormatSettingsDef) then FHue := 0;
        if (FHue < 0) or (FHue > 360) then FHue := 0;
        if not TryStrToFloat(sl[1].Trim.Replace('%', ''), FSaturation, PLCSSFormatSettingsDef) then FSaturation := 0;
        if FSaturation < 0 then FSaturation := 0 else
        if FSaturation > 100 then FSaturation := 100;
        if not TryStrToFloat(sl[2].Trim.Replace('%', ''), FLightness, PLCSSFormatSettingsDef) then FLightness := 0;
        if FLightness < 0 then FLightness := 0 else
        if FLightness > 100 then FLightness := 100;
        if (sl.Count = 4) then begin
          if not TryStrToFloat('0'+sl[3].Trim, FAlpha, PLCSSFormatSettingsDef) then FAlpha := 0;
        end else FAlpha := 1;
        if FAlpha < 0 then FAlpha := 0 else
        if FAlpha > 1 then FAlpha := 1;
      end else begin
        sl.Free;
        Result := false;
        SetDefaultColor;
        exit;
      end;
    finally
      sl.Free;
    end;
    HSL2RGB(FHue, FSaturation, FLightness, FRed, FGreen, FBlue);
    Update;
  end else begin
    s := LowerCase(AValue);
    x := '';
    for i := Low(STANDARD_HTML_COLORS) to High(STANDARD_HTML_COLORS) do
      if LowerCase(STANDARD_HTML_COLORS[i, 0]) = s then begin
        x := STANDARD_HTML_COLORS[i, 1];
        break;
      end;
    if x <> '' then SetColor(x) else begin
      Result := false;
      SetDefaultColor;
    end;
  end;
end;

procedure TPLCSSColor.SetDefaultColor;
begin
  SetColor('#000');
end;

function TPLCSSColor.AsSimpleFormat(AFormat: TPLCSSColorSimpleFormat;
  AIsAlphaIncluded: boolean): string;
begin
  Result := '';
  case AFormat of
    csfRGB: begin
      Result := 'rgb('+IntToStr(FRed)+' '+IntToStr(FGreen)+' '+IntToStr(FBlue);
      if AIsAlphaIncluded then Result := Result+' / '+FloatToStr(FAlpha, PLCSSFormatSettingsDef);
      Result := Result+')';
    end;
    csfHSL: begin
      Result := 'hsl('+IntToStr(round(FHue))+' '+IntToStr(round(FSaturation))+'% '+IntToStr(round(FLightness))+'%';
      if AIsAlphaIncluded then Result := Result+' / '+FloatToStr(FAlpha, PLCSSFormatSettingsDef);
      Result := Result+')';
    end;
  end;
end;

procedure TPLCSSColor.AddColor(AColor: TPLCSSColor);
begin
  Self := '#' + hexStr(max(FRed + AColor.FRed, 255), 2) + hexStr(max(FGreen + AColor.FGreen, 255), 2) +
    hexStr(max(FBlue + AColor.FBlue, 255), 2) + hexStr(round(255 * max(FAlpha + AColor.FAlpha, 1)), 2);
end;

function TPLCSSColor.MixWithColor(AColor: TPLCSSColor; APercent: TPLCSSFloat
  ): TPLCSSColor;
var
  r, g, b: integer;
  a: TPLCSSFloat;
begin
  r := trunc((AColor.FRed - FRed) * APercent);
  g := trunc((AColor.FGreen - FGreen) * APercent);
  b := trunc((AColor.FBlue - FBlue) * APercent);
  a := (AColor.FAlpha - FAlpha) * APercent;

  Result := '#' + hexStr(FRed + r, 2) + hexStr(FGreen + g, 2) + hexStr(FBlue + b, 2) + hexStr(round(255 * (FAlpha + a)), 2);
end;

class operator TPLCSSColor.=(a, b: TPLCSSColor)r: boolean;
begin
  r := a.AsRGBA = b.AsRGBA;
end;

class operator TPLCSSColor.>(a, b: TPLCSSColor)r: boolean;
begin
  r := a.GetFullValue > b.GetFullValue;
end;

function PLHTMLColor(AValue: string): TPLCSSColor;
begin
  Result := TPLCSSColor.Create(AValue);
end;

function BGRAColorFromCSS(AColor: TPLCSSColor): TBGRAPixel;
begin
  Result := ColorToBGRA(AColor.AsPascalColor, round(AColor.Alpha * 255));
end;

operator := (a: string) r: TPLCSSColor;
begin
  r.SetColor(a);
end;

operator := (a: TPLCSSInt) r: TPLCSSColor;
var
  s: string;
begin
  s := '#' + hexStr(a and $FF000000, 2) + hexStr(a and $00FF0000, 2) + hexStr(a and $0000FF00, 2)
    + hexStr(a and $000000FF, 2);
  r := s;
end;

{ TPLCSSTimeUnit }

constructor TPLCSSTimeUnit.Create(AValue: string);
var
  a: TPLCSSFloat = 0;
begin
  AValue := LowerCase(AValue.Trim);

  if AValue.EndsWith('ms') then begin
    FSeconds := false;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('s') then begin
    FSeconds := true;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-1), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else begin
    FSeconds := true;
    FValue := 0;
  end;
end;

function TPLCSSTimeUnit.AsMilliseconds: TPLCSSInt;
var
  w: TPLCSSFloat;
begin
  w := FValue;
  if FSeconds then w *= 1000;

  Result := round(w);
end;

class operator TPLCSSTimeUnit.:=(a: TPLCSSTimeUnit) r: TPLCSSInt;
begin
  r := a.AsMilliseconds;
end;

class operator TPLCSSTimeUnit.:=(a: string) r: TPLCSSTimeUnit;
begin
  r := TPLCSSTimeUnit.Create(a);
end;

class operator TPLCSSTimeUnit.=(a, b: TPLCSSTimeUnit) r: boolean;
begin
  r := a.AsMilliseconds = b.AsMilliseconds;
end;

{ TPLCSSLength }

constructor TPLCSSLength.Create(AValue: TPLCSSFloat; AUnit: TPLCSSUnit);
begin
  FValue := AValue;
  FUnit := AUnit;
end;

constructor TPLCSSLength.Create(AValue: string);
var
  a: TPLCSSFloat = 0;
begin
  AValue := LowerCase(AValue.Trim);

  if AValue = 'auto' then begin
    FUnit := cuPercent;
    FValue := 100;
  end else
  if AValue.EndsWith('cm') then begin
    FUnit := cuCm;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('mm') then begin
    FUnit := cuMm;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('in') then begin
    FUnit := cuIn;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('px') then begin
    FUnit := cuPx;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('pt') then begin
    FUnit := cuPt;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('pc') then begin
    FUnit := cuPc;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('rem') then begin
    FUnit := cuRem;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-3), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('em') then begin
    FUnit := cuEm;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('ex') then begin
    FUnit := cuEx;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('ch') then begin
    FUnit := cuCh;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('vw') then begin
    FUnit := cuVw;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('vh') then begin
    FUnit := cuVh;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-2), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('vmin') then begin
    FUnit := cuVmin;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-4), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('vmax') then begin
    FUnit := cuCm;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-4), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else
  if AValue.EndsWith('%') then begin
    FUnit := cuPercent;
    TryStrToFloat(Copy(AValue, 1, Length(AValue)-1), a, PLCSSFormatSettingsDef);
    FValue := a;
  end else begin
    FUnit := cuNone;
    TryStrToFloat(AValue, a, PLCSSFormatSettingsDef);
    FValue := a;
  end;
end;

class operator TPLCSSLength.=(a, b: TPLCSSLength)r: boolean;
begin
  r := (a.FUnit = b.FUnit) and (a.FValue = b.FValue);
end;

function PLCSSLength(AValue: string): TPLCSSLength;
begin
  Result := TPLCSSLength.Create(AValue);
end;

function CSSLengthToScreenF(AValue: TPLCSSLength; ASelf: IPLCSSCustomControl;
  AProperty: string): TPLCSSFloat;
var
  w: string;
  c: IPLCSSCustomControl;
  vw, vh: TPLCSSFloat;
begin
  Result := 0;

  // absolute lengths

  case AValue.LUnit of
    cuNone: Result := AValue.LValue;
    cuPx: Result := ScaleValueF(AValue.LValue, ASelf);
    cuCm: Result := ScaleValueF(AValue.LValue*37.7952755906, ASelf);
    cuMm: Result := ScaleValueF(AValue.LValue*3.77952755906, ASelf);
    cuIn: Result := ScaleValueF(AValue.LValue*96, ASelf);
    cuPt: Result := ScaleValueF(AValue.LValue*1.33333333333, ASelf);
    cuPc: Result := ScaleValueF(AValue.LValue*16, ASelf);
  end;

  if (ASelf = nil) or (Result <> 0) or (AProperty = '') then exit;

  // relative lengths

  //case AValue.LUnit of
  //  cuEm: begin
  //    if Assigned(ASelf.GetParent) then c := ASelf.GetParent else c := ASelf;
  //    w := c.Styles['::self::'].Properties[AProperty];
  //    if IsInitValue(w) then begin
  //      if c = ASelf then Result := CSSLengthToScreenF(TPLCSSLength.Create(15*AValue.LValue, TPLCSSUnit.cuPx), nil, '') else
  //        Result := CSSLengthToScreenF(TPLCSSLength.Create(AValue.LValue, AValue.LUnit), c, AProperty);
  //    end else
  //      Result := CSSLengthToScreenF(TPLCSSLength.Create(PLCSSLength(w).LValue*AValue.LValue, PLCSSLength(w).LUnit), nil, '');
  //  end;
  //  cuRem: begin
  //    if Assigned(ASelf.Browser) then c := ASelf.Browser else c := ASelf;
  //    w := c.Styles['::self::'].Properties[AProperty];
  //    if IsInitValue(w) then Result := CSSLengthToScreenF(TPLCSSLength.Create(15*AValue.LValue, TPLCSSUnit.cuPx), nil, '') else
  //      Result := CSSLengthToScreenF(TPLCSSLength.Create(PLCSSLength(w).LValue*AValue.LValue, PLCSSLength(w).LUnit), nil, '');
  //  end;
  //  cuPercent: begin
  //    if Assigned(ASelf.GetParent) then c := ASelf.GetParent else
  //      if Assigned(ASelf.Browser) then c := ASelf.Browser else c := ASelf;
  //    w := c.Styles['::self::'].Properties[AProperty];
  //    if IsInitValue(w) then Result := CSSLengthToScreenF(TPLCSSLength.Create('1px'), nil, '') else
  //      Result := CSSLengthToScreenF(TPLCSSLength.Create(PLCSSLength(w).LValue*(AValue.LValue/100), PLCSSLength(w).LUnit), nil, '');
  //  end;
  //  cuEx: begin
  //    w := ASelf.Styles['::self::'].Properties[AProperty];
  //    if IsInitValue(w) then begin
  //      c := ASelf.GetParent;
  //      if Assigned(c) then Result := CSSLengthToScreenF(TPLCSSLength.Create(AValue.LValue, AValue.LUnit), c, AProperty) else
  //        Result := CSSLengthToScreenF(TPLCSSLength.Create(15, TPLCSSUnit.cuPx), nil, '');
  //    end else Result := CSSLengthToScreenF(TPLCSSLength.Create(PLCSSLength(w).LValue*AValue.LValue, PLCSSLength(w).LUnit), nil, '');
  //  end;
  //  cuCh: Result := ASelf.Canvas.TextWidth('0');
  //  cuVw, cuVh, cuVmin, cuVmax: begin
  //    if Assigned(ASelf.Browser) then vw := ASelf.Browser.Width else if Assigned(ASelf.Parent) then vw := ASelf.Parent.Width else vw := ASelf.Width;
  //    if Assigned(ASelf.Browser) then vh := ASelf.Browser.Height else if Assigned(ASelf.Parent) then vh := ASelf.Parent.Height else vh := ASelf.Height;
  //    vw := vw / 100;
  //    vh := vh / 100;
  //    case AValue.LUnit of
  //      cuVw: Result := vw;
  //      cuVh: Result := vh;
  //      cuVmin: Result := Min(vw, vh);
  //      cuVmax: Result := Max(vw, vh);
  //    end;
  //    Result := Result*AValue.LValue;
  //  end;
  //end;
end;

function CSSLengthToScreen(AValue: TPLCSSLength; ASelf: IPLCSSCustomControl;
  AProperty: string): TPLCSSInt;
begin
  Result := round(CSSLengthToScreenF(AValue, ASelf, AProperty));
end;

operator := (a: TPLCSSLength) r: TPLCSSFloat;
begin
  r := CSSLengthToScreenF(a);
end;

operator := (a: string) r: TPLCSSLength;
begin
  r := TPLCSSLength.Create(a);
end;

{ TPLCSSMargins }

constructor TPLCSSMargins.Create(AValue: string);
begin
  // 100% = auto
  FLeft := '0';
  FTop := '0';
  FRight := '100%';
  FBottom := '0';

  AValue := AValue.Trim;

  if IsInitValue(AValue) then exit;


end;

class operator TPLCSSMargins.=(a, b: TPLCSSMargins)r: boolean;
begin
  r := (a.FLeft = b.FLeft) and (a.FRight = b.FRight) and (a.FTop = b.FTop) and (a.FBottom = b.FBottom);
end;

{ TPLCSSBorderRadius }

//constructor TPLCSSBorderRadius.Create(ASelector: TPLCSSSelector);
//var
//  w: string;
//  arr: array of string;
//begin
//  w := ASelector.Properties['border-radius'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if IsInitValue(w) then w := '0px';
//
//  arr := w.Split(' ');
//  case Length(arr) of
//    1: begin
//      FTopLeft := TPLCSSLength.Create(w);
//      FBottomLeft := TPLCSSLength.Create(w);
//      FTopRight := TPLCSSLength.Create(w);
//      FBottomRight := TPLCSSLength.Create(w);
//    end;
//    2: begin
//      FTopLeft := TPLCSSLength.Create(arr[0]);
//      FBottomRight := TPLCSSLength.Create(arr[0]);
//      FTopRight := TPLCSSLength.Create(arr[1]);
//      FBottomLeft := TPLCSSLength.Create(arr[1]);
//    end;
//    3: begin
//      FTopLeft := TPLCSSLength.Create(arr[0]);
//      FTopRight := TPLCSSLength.Create(arr[1]);
//      FBottomLeft := TPLCSSLength.Create(arr[1]);
//      FBottomRight := TPLCSSLength.Create(arr[2]);
//    end;
//    4: begin
//      FTopLeft := TPLCSSLength.Create(arr[0]);
//      FTopRight := TPLCSSLength.Create(arr[1]);
//      FBottomLeft := TPLCSSLength.Create(arr[3]);
//      FBottomRight := TPLCSSLength.Create(arr[2]);
//    end;
//  end;
//
//  w := ASelector.Properties['border-top-left-radius'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then
//    FTopLeft := TPLCSSLength.Create(w);
//
//  w := ASelector.Properties['border-top-right-radius'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then
//    FTopRight := TPLCSSLength.Create(w);
//
//  w := ASelector.Properties['border-bottom-left-radius'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then
//    FBottomLeft := TPLCSSLength.Create(w);
//
//  w := ASelector.Properties['border-bottom-right-radius'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then
//    FBottomRight := TPLCSSLength.Create(w);
//end;

function BorderW(aval: string): string;
begin
  Result := aval.Trim.ToLower;
  case Result of
    'thin': Result := '1px';
    'medium': Result := '3px';
    'thick': Result := '5px';
  end;
end;

{ TPLCSSBorderPart }

procedure TPLCSSBorderPart.SetStyle(AValue: string);
begin
  if FStyle = AValue then exit;
  FStyle := AValue;
end;

//constructor TPLCSSBorderPart.Create(ASelector: TPLCSSSelector; AType: string);
//var
//  w: string;
//begin
//  w := ASelector.Properties['border-'+AType.Trim.ToLower+'-color'].ToLower.Trim;
//  ProcessImportant(w);
//  FColor := w;
//
//  w := ASelector.Properties['border-'+AType.Trim.ToLower+'-style'].ToLower.Trim;
//  ProcessImportant(w);
//  FStyle := w;
//
//  w := ASelector.Properties['border-'+AType.Trim.ToLower+'-width'].ToLower.Trim;
//  ProcessImportant(w);
//  FWidth := TPLCSSLength.Create(BorderW(w));
//end;

{ TPLCSSBorder }

//constructor TPLCSSBorder.Create(AValue: TPLCSSSelector);
//var
//  w: string;
//  arr: array of string;
//begin
//  FLeft := TPLCSSBorderPart.Create(AValue, 'left');
//  FTop := TPLCSSBorderPart.Create(AValue, 'top');
//  FRight := TPLCSSBorderPart.Create(AValue, 'right');
//  FBottom := TPLCSSBorderPart.Create(AValue, 'bottom');
//  FRadius := TPLCSSBorderRadius.Create(AValue);
//
//  w := AValue.Properties['border-color'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then begin
//    // now support for one color only
//    FLeft.Color := w;
//    FTop.Color := w;
//    FRight.Color := w;
//    FBottom.Color := w;
//  end else begin
//    FLeft.Color := 'transparent';
//    FTop.Color := FLeft.Color;
//    FRight.Color := FLeft.Color;
//    FBottom.Color := FLeft.Color;
//  end;
//
//  w := AValue.Properties['border-width'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then begin
//    CorrectColorFunctions(w);
//    arr := w.Split(' ');
//
//    case Length(arr) of
//      1: begin
//        FTop.Width := TPLCSSLength.Create(BorderW(w));
//        FLeft.Width := FTop.Width;
//        FBottom.Width := FTop.Width;
//        FRight.Width := FTop.Width;
//      end;
//      2: begin
//        FTop.Width := TPLCSSLength.Create(BorderW(arr[0]));
//        FBottom.Width := FTop.Width;
//        FLeft.Width := TPLCSSLength.Create(BorderW(arr[1]));
//        FRight.Width := FTop.Width;
//      end;
//      3: begin
//        FTop.Width := TPLCSSLength.Create(BorderW(arr[0]));
//        FBottom.Width := TPLCSSLength.Create(BorderW(arr[2]));
//        FLeft.Width := TPLCSSLength.Create(BorderW(arr[1]));
//        FRight.Width := FLeft.Width;
//      end;
//      4: begin
//        FTop.Width := TPLCSSLength.Create(BorderW(arr[0]));
//        FBottom.Width := TPLCSSLength.Create(BorderW(arr[2]));
//        FLeft.Width := TPLCSSLength.Create(BorderW(arr[3]));
//        FRight.Width := TPLCSSLength.Create(BorderW(arr[1]));
//      end;
//    end;
//  end else begin
//    FTop.Width := TPLCSSLength.Create('1px');
//    FLeft.Width := FTop.Width;
//    FBottom.Width := FTop.Width;
//    FRight.Width := FTop.Width;
//  end;
//
//  w := AValue.Properties['border-style'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then begin
//    arr := w.Split(' ');
//
//    case Length(arr) of
//      1: begin
//        FTop.Style := w;
//        FLeft.Style := FTop.Style;
//        FBottom.Style := FTop.Style;
//        FRight.Style := FTop.Style;
//      end;
//      2: begin
//        FTop.Style := arr[0];
//        FBottom.Style := FTop.Style;
//        FLeft.Style := arr[1];
//        FRight.Style := FTop.Style;
//      end;
//      3: begin
//        FTop.Style := arr[0];
//        FBottom.Style := arr[2];
//        FLeft.Style := arr[1];
//        FRight.Style := FLeft.Style;
//      end;
//      4: begin
//        FTop.Style := arr[0];
//        FBottom.Style := arr[2];
//        FLeft.Style := arr[3];
//        FRight.Style := arr[1];
//      end;
//    end;
//  end else begin
//    FTop.Style := 'solid';
//    FLeft.Style := FTop.Style;
//    FBottom.Style := FTop.Style;
//    FRight.Style := FTop.Style;
//  end;
//
//  w := AValue.Properties['border'].ToLower.Trim;
//  ProcessImportant(w);
//
//  if not IsInitValue(w) then begin
//    CorrectColorFunctions(w);
//    arr := w.Split(' ');
//
//    case Length(arr) of
//      1: begin
//        FTop.Style := w;
//        FLeft.Style := FTop.Style;
//        FBottom.Style := FTop.Style;
//        FRight.Style := FTop.Style;
//
//        FLeft.Color := 'initial';
//        FTop.Color := FLeft.Color;
//        FRight.Color := FLeft.Color;
//        FBottom.Color := FLeft.Color;
//
//        FTop.Width := TPLCSSLength.Create('1px');
//        FLeft.Width := FTop.Width;
//        FBottom.Width := FTop.Width;
//        FRight.Width := FTop.Width;
//      end;
//      2: begin
//        if TPLCSSColor.SetColor(arr[1]) then begin
//          FTop.Style := arr[0];
//          FLeft.Style := FTop.Style;
//          FBottom.Style := FTop.Style;
//          FRight.Style := FTop.Style;
//
//          FLeft.Color := arr[1];
//          FTop.Color := FLeft.Color;
//          FRight.Color := FLeft.Color;
//          FBottom.Color := FLeft.Color;
//
//          FTop.Width := TPLCSSLength.Create('1px');
//          FLeft.Width := FTop.Width;
//          FBottom.Width := FTop.Width;
//          FRight.Width := FTop.Width;
//        end else begin
//          FTop.Width := TPLCSSLength.Create(BorderW(arr[0]));
//          FLeft.Width := FTop.Width;
//          FBottom.Width := FTop.Width;
//          FRight.Width := FTop.Width;
//
//          FTop.Style := arr[1];
//          FLeft.Style := FTop.Style;
//          FBottom.Style := FTop.Style;
//          FRight.Style := FTop.Style;
//
//          FLeft.Color := 'initial';
//          FTop.Color := FLeft.Color;
//          FRight.Color := FLeft.Color;
//          FBottom.Color := FLeft.Color;
//        end;
//      end;
//      3: begin
//        FTop.Width := TPLCSSLength.Create(BorderW(arr[0]));
//        FLeft.Width := FTop.Width;
//        FBottom.Width := FTop.Width;
//        FRight.Width := FTop.Width;
//
//        FTop.Style := arr[1];
//        FLeft.Style := FTop.Style;
//        FBottom.Style := FTop.Style;
//        FRight.Style := FTop.Style;
//
//        FLeft.Color := arr[2];
//        FTop.Color := FLeft.Color;
//        FRight.Color := FLeft.Color;
//        FBottom.Color := FLeft.Color;
//      end;
//    end;
//  end;
//end;

procedure TPLCSSBorder.Draw(ABitmap: TBGRABitmap; ARect: TRect;
  ASelf: IPLCSSCustomControl);

  procedure DrawBorder(AName, AType: string; APart: TPLCSSBorderPart);
  var
    pth: TBGRAPath;
    r: TRect;
    bw : TPLCSSFloat;
    ps: TFPPenStyle;
    col, col2: TBGRAPixel;
  begin
    if APart.Color = 'transparent' then exit; // optimalization

    ps := ABitmap.PenStyle;
    r := ARect;
    bw := CSSLengthToScreenF(APart.Width, ASelf, 'border-'+AType.ToLower.Trim+'-width');
    col := BGRAColorFromCSS(APart.Color);
    pth := GetBorders(r, ASelf, AType);

    case AName.Trim.ToLower of
      'dotted': begin
        ABitmap.PenStyle := psDot;
        ABitmap.DrawPath(pth, col, bw, BGRAPixelTransparent);
      end;
      'dashed': begin
        ABitmap.PenStyle := psDash;
        ABitmap.DrawPath(pth, col, bw, BGRAPixelTransparent);
      end;
      'solid': begin
        ABitmap.PenStyle := psSolid;
        ABitmap.DrawPath(pth, col, bw, BGRAPixelTransparent);
      end;
      'double': begin
        ABitmap.PenStyle := psSolid;
        ABitmap.DrawPath(pth, col, bw / 3, BGRAPixelTransparent);
        InflateRect(r, -round(1.5 * bw / 3), -round(1.5 * bw / 3));
        pth.Free;
        pth := GetBorders(r, ASelf, AType);
        ABitmap.DrawPath(pth, col, bw / 3, BGRAPixelTransparent);
      end;
      'inset': begin
        ABitmap.PenStyle := psSolid;
        if AnsiMatchStr(AType.ToLower.Trim, ['left', 'top']) then
          col.Lightness := Max(0, col.Lightness - 15728);  // 65535 * 60/255
        ABitmap.DrawPath(pth, col, bw, BGRAPixelTransparent);
      end;
      'outset': begin
        ABitmap.PenStyle := psSolid;
        if AnsiMatchStr(AType.ToLower.Trim, ['right', 'bottom']) then
          col.Lightness := Max(0, col.Lightness - 15728);
        ABitmap.DrawPath(pth, col, bw, BGRAPixelTransparent);
      end;
      'groove': begin
        ABitmap.PenStyle := psSolid;
        if AnsiMatchStr(AType.ToLower.Trim, ['left', 'top']) then begin
          col2 := col;
          col.Lightness := Max(0, col.Lightness - 15728);
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
          InflateRect(r, -round(bw / 2), -round(bw / 2));
          pth.Free;
          pth := GetBorders(r, ASelf, AType);
          ABitmap.DrawPath(pth, col2, bw / 2, BGRAPixelTransparent);
        end else begin
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
          InflateRect(r, -round(bw / 2), -round(bw / 2));
          pth.Free;
          pth := GetBorders(r, ASelf, AType);
          col.Lightness := Max(0, col.Lightness - 15728);
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
        end;
      end;
      'ridge': begin
        ABitmap.PenStyle := psSolid;
        if AnsiMatchStr(AType.ToLower.Trim, ['right', 'bottom']) then begin
          col2 := col;
          col.Lightness := Max(0, col.Lightness - 15728);
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
          InflateRect(r, -round(bw / 2), -round(bw / 2));
          pth.Free;
          pth := GetBorders(r, ASelf, AType);
          ABitmap.DrawPath(pth, col2, bw / 2, BGRAPixelTransparent);
        end else begin
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
          InflateRect(r, -round(bw / 2), -round(bw / 2));
          pth.Free;
          pth := GetBorders(r, ASelf, AType);
          col.Lightness := Max(0, col.Lightness - 15728);
          ABitmap.DrawPath(pth, col, bw / 2, BGRAPixelTransparent);
        end;
      end;
    end;

    ABitmap.PenStyle := ps;

    pth.Free;
  end;

begin
  if (FLeft.Width.LValue > 0) then DrawBorder(FLeft.Style, 'left', FLeft);
  if (FTop.Width.LValue > 0) then DrawBorder(FTop.Style, 'top', FTop);
  if (FRight.Width.LValue > 0) then DrawBorder(FRight.Style, 'right', FRight);
  if (FBottom.Width.LValue > 0) then DrawBorder(FBottom.Style, 'bottom', FBottom);
end;

function TPLCSSBorder.GetBorders(ARect: TRect; ASelf: IPLCSSCustomControl;
  AType: string): TBGRAPath;
begin
  Result := TBGRAPath.Create;

  case AType.ToLower.Trim of
    'left': begin
      Result.moveTo(ARect.Left + min(CSSLengthToScreen(FRadius.BottomLeft, ASelf, 'border-bottom-left-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Bottom);
      Result.arcTo(ARect.Left, ARect.Bottom, ARect.Left, ARect.Bottom - min(CSSLengthToScreen(FRadius.BottomLeft, ASelf, 'border-bottom-left-radius'), min(ARect.Width, ARect.Height) / 2), min(CSSLengthToScreen(FRadius.BottomLeft, ASelf, 'border-bottom-left-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.lineTo(ARect.Left, ARect.Top + min(CSSLengthToScreen(FRadius.TopLeft, ASelf, 'border-top-left-radius'), min(ARect.Width, ARect.Height) / 2));
    end;
    'top': begin
      Result.moveTo(ARect.Left, ARect.Top + min(CSSLengthToScreen(FRadius.TopLeft, ASelf, 'border-top-left-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.arcTo(ARect.Left, ARect.Top, ARect.Left + min(CSSLengthToScreen(FRadius.TopLeft, ASelf, 'border-top-left-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Top, min(CSSLengthToScreen(FRadius.TopLeft, ASelf, 'border-top-left-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.lineTo(ARect.Right - min(CSSLengthToScreen(FRadius.TopRight, ASelf, 'border-top-right-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Top);
    end;
    'right': begin
      Result.moveTo(ARect.Right - min(CSSLengthToScreen(FRadius.TopRight, ASelf, 'border-top-right-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Top);
      Result.arcTo(ARect.Right, ARect.Top, ARect.Right, ARect.Top + min(CSSLengthToScreen(FRadius.TopRight, ASelf, 'border-top-right-radius'), min(ARect.Width, ARect.Height) / 2), min(CSSLengthToScreen(FRadius.TopRight, ASelf, 'border-top-right-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.lineTo(ARect.Right, ARect.Bottom - min(CSSLengthToScreen(FRadius.BottomRight, ASelf, 'border-bottom-right-radius'), min(ARect.Width, ARect.Height) / 2));
    end;
    'bottom': begin
      Result.moveTo(ARect.Right, ARect.Bottom - min(CSSLengthToScreen(FRadius.BottomRight, ASelf, 'border-bottom-right-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.arcTo(ARect.Right, ARect.Bottom, ARect.Right - min(CSSLengthToScreen(FRadius.BottomRight, ASelf, 'border-bottom-right-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Bottom, min(CSSLengthToScreen(FRadius.BottomRight, ASelf, 'border-bottom-right-radius'), min(ARect.Width, ARect.Height) / 2));
      Result.lineTo(ARect.Left + min(CSSLengthToScreen(FRadius.BottomLeft, ASelf, 'border-bottom-left-radius'), min(ARect.Width, ARect.Height) / 2), ARect.Bottom);
    end;
  end;
end;

{ TPLCSSBackground }

procedure TPLCSSBackground.SetAttachment(AValue: string);
begin
  AValue := LowerCase(AValue.Trim);
  if (FAttachment = AValue) or not AnsiMatchStr(AValue, ['scroll','fixed','local','initial']) then exit;
  FAttachment := AValue;
end;

procedure TPLCSSBackground.SetBlendMode(AValue: string);
begin
  AValue := LowerCase(AValue.Trim);
  if (FBlendMode = AValue) or not AnsiMatchStr(AValue, ['initial','normal','multiply','screen','overlay','darken','lighten','color-dodge','saturation','color','luminosity']) then exit;
  FBlendMode := AValue;
end;

procedure TPLCSSBackground.SetClip(AValue: string);
begin
  AValue := LowerCase(AValue.Trim);
  if (FClip = AValue) or not AnsiMatchStr(AValue, ['border-box','padding-box','content-box','initial']) then exit;
  FClip := AValue;
end;

procedure TPLCSSBackground.SetColor(AValue: TPLCSSColor);
begin
  FColor := AValue;
end;

procedure TPLCSSBackground.SetOrigin(AValue: string);
begin
  AValue := LowerCase(AValue.Trim);
  if (FOrigin = AValue) or not AnsiMatchStr(AValue, ['padding-box','border-box','content-box','initial']) then exit;
  FOrigin := AValue;
end;

procedure TPLCSSBackground.SetRepeating(AValue: string);
begin
  AValue := LowerCase(AValue.Trim);
  if (FRepeating = AValue) or not AnsiMatchStr(AValue, ['repeat','repeat-x','repeat-y','no-repeat','initial']) then exit;
  FRepeating := AValue;
end;

//constructor TPLCSSBackground.Create(AValue: TPLCSSSelector);
//begin
//  SetBackground(AValue);
//end;

//procedure TPLCSSBackground.SetBackground(AValue: TPLCSSSelector);
//var
//  w: string;
//  s: TBGRABitmap;
//begin
//  w := AValue.Properties['background'].ToLower.Trim;
//  ProcessImportant(w);
//  CorrectColorFunctions(w);
//
//
//  w := AValue.Properties['background-image'].Trim;
//  ProcessImportant(w);
//  FImage := '';
//  FImageCache := nil;
//  if LowerCase(Copy(w, 1, 3)) = 'url' then begin
//    FImage := Copy(w, 5, Length(w)-5);
//    if FImage.StartsWith(#39) or FImage.StartsWith('"') then FImage := Copy(FImage, 2, Length(FImage));
//    if FImage.EndsWith(#39) or FImage.EndsWith('"') then FImage := Copy(FImage, 1, Length(FImage)-1);
//
//    CheckAndCorrectURL(FImage, FLocation);
//
//    if PLCSSGlobalBgCache.Exists(FImage, s) then begin
//      FImageCache := s;
//    end else if FileExists(FImage) then begin
//      s := TBGRABitmap.Create(FImage);
//      PLCSSGlobalBgCache.Add(FImage, s);
//      FImageCache := s;
//    end else if LoadOnlineImage(FImage, s) then begin
//      FImageCache := s;
//      PLCSSGlobalBgCache.Add(FImage, s);
//    end else begin
//      FImageCache := GetBlankImage;
//      PLCSSGlobalBgCache.Add(FImage, FImageCache);
//    end;
//  end;
//
//  w := AValue.Properties['background-color'].ToLower.Trim;
//  ProcessImportant(w);
//  FColor := 'transparent';
//  if not IsInitValue(w) then FColor := w;
//
//  w := AValue.Properties['background-repeat'].ToLower.Trim;
//  ProcessImportant(w);
//  SetRepeating(w);
//
//  w := AValue.Properties['background-clip'].ToLower.Trim;
//  ProcessImportant(w);
//  SetClip(w);
//
//  w := AValue.Properties['background-attachment'].ToLower.Trim;
//  ProcessImportant(w);
//  SetAttachment(w);
//
//  w := AValue.Properties['background-origin'].ToLower.Trim;
//  ProcessImportant(w);
//  SetOrigin(w);
//
//  w := AValue.Properties['background-position'].ToLower.Trim;
//  ProcessImportant(w);
//  FPosition := w;
//
//  w := AValue.Properties['background-size'].ToLower.Trim;
//  ProcessImportant(w);
//  FSize := w;
//
//  FOpacity := 1;
//  w := AValue.Properties['opacity'].ToLower.Trim;
//  ProcessImportant(w);
//  if IsInitValue(w) then w := '1';
//  TryStrToFloat('0'+w, FOpacity);
//  if FOpacity < 0 then FOpacity := 0
//  else if FOpacity > 1 then FOpacity := 1;
//
//  FBorder := TPLCSSBorder.Create(AValue);
//end;
//
procedure TPLCSSBackground.Draw(ABitmap: TBGRABitmap; ARect: TRect;
  ASelf: IPLCSSCustomControl);
var
  tmp, bmp: TBGRABitmap;
  w: string;
  pthl, ptht, pthr, pthb, pth: TBGRAPath;
begin
  bmp := TBGRABitmap.Create(ARect.Width, ARect.Height);
  try
    if FColor <> 'transparent' then bmp.Fill(BGRAColorFromCSS(FColor));

    if Assigned(FImageCache) then begin
      w := FRepeating;
      if FRepeating = 'initial' then w := 'repeat';
      bmp.Canvas2D.save;
      bmp.Canvas2D.clearPath;
      bmp.Canvas2D.rect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas2D.fillStyle(bmp.Canvas2D.createPattern(FImageCache, w));
      bmp.Canvas2D.fill;
      bmp.Canvas2D.restore;
    end;

    tmp := TBGRABitmap.Create(bmp.Width, bmp.Height, BGRABlack);
    try
      pthl := FBorder.GetBorders(tmp.ClipRect, ASelf, 'left');
      ptht := FBorder.GetBorders(tmp.ClipRect, ASelf, 'top');
      pthr := FBorder.GetBorders(tmp.ClipRect, ASelf, 'right');
      pthb := FBorder.GetBorders(tmp.ClipRect, ASelf, 'bottom');

      pth := TBGRAPath.Create;
      pth.SetPoints(ConcatPointsF([pthl.ToPoints(), ptht.ToPoints(), pthr.ToPoints(), pthb.ToPoints()]));
      pth.closePath;

      tmp.Canvas2D.addPath(pth);
      tmp.Canvas2D.fillStyle(BGRAWhite);
      tmp.Canvas2D.fill;

      if FClip = 'padding-box' then begin
        tmp.Canvas2D.strokeStyle(BGRAWhite);
        tmp.Canvas2D.clearPath;
        tmp.Canvas2D.addPath(pthl);
        tmp.Canvas2D.lineWidth := CSSLengthToScreenF(FBorder.Left.Width, ASelf, 'border-left-width');
        tmp.Canvas2D.stroke;
        tmp.Canvas2D.clearPath;
        tmp.Canvas2D.addPath(ptht);
        tmp.Canvas2D.lineWidth := CSSLengthToScreenF(FBorder.Top.Width, ASelf, 'border-top-width');
        tmp.Canvas2D.stroke;
        tmp.Canvas2D.clearPath;
        tmp.Canvas2D.addPath(pthr);
        tmp.Canvas2D.lineWidth := CSSLengthToScreenF(FBorder.Right.Width, ASelf, 'border-right-width');
        tmp.Canvas2D.stroke;
        tmp.Canvas2D.clearPath;
        tmp.Canvas2D.addPath(pthb);
        tmp.Canvas2D.lineWidth := CSSLengthToScreenF(FBorder.Bottom.Width, ASelf, 'border-bottom-width');
        tmp.Canvas2D.stroke;
      end;

      bmp.ApplyMask(tmp);
    finally
      tmp.Free;
      pth.Free;
      pthl.Free;
      ptht.Free;
      pthr.Free;
      pthb.Free;
    end;

    ABitmap.PutImage(ARect.Left, ARect.Top, bmp, dmDrawWithTransparency, round(FOpacity * 255));

    FBorder.Draw(ABitmap, ARect, ASelf);
  finally
    bmp.Free;
  end;
end;

end.

