unit PospoLiteHTML.CSS.Basics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, math, Forms, LazUTF8, DOM;

const
  // CSS Base Units (cm is base)
  PLCSSUnitCm = 1.0;
  PLCSSUnitMm = PLCSSUnitCm*10;
  PLCSSUnitIn = 2.54*PLCSSUnitCm;
  PLCSSUnitPx = 96/PLCSSUnitIn;
  PLCSSUnitPt = 72/PLCSSUnitIn;
  PLCSSUnitPc = 12*PLCSSUnitPt;

  // CSS Default Format Settings
  PLCSSFormatSettingsDef: TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

  // List of standard HTML colors
  STANDARD_HTML_COLORS: array[0..149] of array[0..1] of string = (
      // special
      ('transparent', '#00000000'), ('initial', '#000'),
      // normal (alphabetically ordered)
      ('AliceBlue', '#F0F8FF'), ('AntiqueWhite', '#FAEBD7'), ('Aqua', '#00FFFF'),
      ('Aquamarine', '#7FFFD4'), ('Azure', '#F0FFFF'), ('Beige', '#F5F5DC'),
      ('Bisque', '#FFE4C4'), ('Black', '#000000'), ('BlanchedAlmond', '#FFEBCD'),
      ('Blue', '#0000FF'), ('BlueViolet', '#8A2BE2'), ('Brown', '#A52A2A'),
      ('BurlyWood', '#DEB887'), ('CadetBlue', '#5F9EA0'), ('Chartreuse', '#7FFF00'),
      ('Chocolate', '#D2691E'), ('Coral', '#FF7F50'), ('CornflowerBlue', '#6495ED'),
      ('Cornsilk', '#FFF8DC'), ('Crimson', '#DC143C'), ('Cyan', '#00FFFF'),
      ('DarkBlue', '#00008B'), ('DarkCyan', '#008B8B'), ('DarkGoldenRod', '#B8860B'),
      ('DarkGray', '#A9A9A9'), ('DarkGrey', '#A9A9A9'), ('DarkGreen', '#006400'),
      ('DarkKhaki', '#BDB76B'), ('DarkMagenta', '#8B008B'), ('DarkOliveGreen', '#556B2F'),
      ('DarkOrange', '#FF8C00'), ('DarkOrchid', '#9932CC'), ('DarkRed', '#8B0000'),
      ('DarkSalmon', '#E9967A'), ('DarkSeaGreen', '#8FBC8F'), ('DarkSlateBlue', '#483D8B'),
      ('DarkSlateGray', '#2F4F4F'), ('DarkSlateGrey', '#2F4F4F'), ('DarkTurquoise', '#00CED1'),
      ('DarkViolet', '#9400D3'), ('DeepPink', '#FF1493'), ('DeepSkyBlue', '#00BFFF'),
      ('DimGray', '#696969'), ('DimGrey', '#696969'), ('DodgerBlue', '#1E90FF'),
      ('FireBrick', '#B22222'), ('FloralWhite', '#FFFAF0'), ('ForestGreen', '#228B22'),
      ('Fuchsia', '#FF00FF'), ('Gainsboro', '#DCDCDC'), ('GhostWhite', '#F8F8FF'),
      ('Gold', '#FFD700'), ('GoldenRod', '#DAA520'), ('Gray', '#808080'),
      ('Grey', '#808080'), ('Green', '#008000'), ('GreenYellow', '#ADFF2F'),
      ('HoneyDew', '#F0FFF0'), ('HotPink', '#FF69B4'), ('IndianRed', '#CD5C5C'),
      ('Indigo', '#4B0082'), ('Ivory', '#FFFFF0'), ('Khaki', '#F0E68C'),
      ('Lavender', '#E6E6FA'), ('LavenderBlush', '#FFF0F5'), ('LawnGreen', '#7CFC00'),
      ('LemonChiffon', '#FFFACD'), ('LightBlue', '#ADD8E6'), ('LightCoral', '#F08080'),
      ('LightCyan', '#E0FFFF'), ('LightGoldenRodYellow', '#FAFAD2'), ('LightGray', '#D3D3D3'),
      ('LightGrey', '#D3D3D3'), ('LightGreen', '#90EE90'), ('LightPink', '#FFB6C1'),
      ('LightSalmon', '#FFA07A'), ('LightSeaGreen', '#20B2AA'), ('LightSkyBlue', '#87CEFA'),
      ('LightSlateGray', '#778899'), ('LightSlateGrey', '#778899'), ('LightSteelBlue', '#B0C4DE'),
      ('LightYellow', '#FFFFE0'), ('Lime', '#00FF00'), ('LimeGreen', '#32CD32'),
      ('Linen', '#FAF0E6'), ('Magenta', '#FF00FF'), ('Maroon', '#800000'),
      ('MediumAquaMarine', '#66CDAA'), ('MediumBlue', '#0000CD'), ('MediumOrchid', '#BA55D3'),
      ('MediumPurple', '#9370DB'), ('MediumSeaGreen', '#3CB371'), ('MediumSlateBlue', '#7B68EE'),
      ('MediumSpringGreen', '#00FA9A'), ('MediumTurquoise', '#48D1CC'), ('MediumVioletRed', '#C71585'),
      ('MidnightBlue', '#191970'), ('MintCream', '#F5FFFA'), ('MistyRose', '#FFE4E1'),
      ('Moccasin', '#FFE4B5'), ('NavajoWhite', '#FFDEAD'), ('Navy', '#000080'),
      ('OldLace', '#FDF5E6'), ('Olive', '#808000'), ('OliveDrab', '#6B8E23'),
      ('Orange', '#FFA500'), ('OrangeRed', '#FF4500'), ('Orchid', '#DA70D6'),
      ('PaleGoldenRod', '#EEE8AA'), ('PaleGreen', '#98FB98'), ('PaleTurquoise', '#AFEEEE'),
      ('PaleVioletRed', '#DB7093'), ('PapayaWhip', '#FFEFD5'), ('PeachPuff', '#FFDAB9'),
      ('Peru', '#CD853F'), ('Pink', '#FFC0CB'), ('Plum', '#DDA0DD'),
      ('PowderBlue', '#B0E0E6'), ('Purple', '#800080'), ('RebeccaPurple', '#663399'),
      ('Red', '#FF0000'), ('RosyBrown', '#BC8F8F'), ('RoyalBlue', '#4169E1'),
      ('SaddleBrown', '#8B4513'), ('Salmon', '#FA8072'), ('SandyBrown', '#F4A460'),
      ('SeaGreen', '#2E8B57'), ('SeaShell', '#FFF5EE'), ('Sienna', '#A0522D'),
      ('Silver', '#C0C0C0'), ('SkyBlue', '#87CEEB'), ('SlateBlue', '#6A5ACD'),
      ('SlateGray', '#708090'), ('SlateGrey', '#708090'), ('Snow', '#FFFAFA'),
      ('SpringGreen', '#00FF7F'), ('SteelBlue', '#4682B4'), ('Tan', '#D2B48C'),
      ('Teal', '#008080'), ('Thistle', '#D8BFD8'), ('Tomato', '#FF6347'),
      ('Turquoise', '#40E0D0'), ('Violet', '#EE82EE'), ('Wheat', '#F5DEB3'),
      ('White', '#FFFFFF'), ('WhiteSmoke', '#F5F5F5'), ('Yellow', '#FFFF00'),
      ('YellowGreen', '#9ACD32')
    );

  // List of CSS properties (alphabetically ordered)
  ALL_CSS_PROPERTIES_NAMES: array[0..(86+1)-1] of string = (
      //align 3
      'align-content', 'align-items', 'align-self',
      //all 1
      'all',
      //animation 9
      'animation', 'animation-delay', 'animation-direction', 'animation-duration',
      'animation-fill-mode', 'animation-iteration-count', 'animation-name',
      'animation-play-state', 'animation-timing-function',
      //backface 1
      'backface-visibility',
      //background 10
      'background', 'background-attachment', 'background-blend-mode', 'background-clip',
      'background-color', 'background-image', 'background-origin', 'background-position',
      'background-repeat', 'background-size',
      //border 33
      'border', 'border-bottom', 'border-bottom-color', 'border-bottom-left-radius',
      'border-bottom-right-radius', 'border-bottom-style', 'border-bottom-width',
      'border-collapse', 'border-color', 'border-image', 'border-image-outset',
      'border-image-repeat', 'border-image-slice', 'border-image-source',
      'border-image-width', 'border-left', 'border-left-color', 'border-left-style',
      'border-left-width', 'border-radius', 'border-right', 'border-right-color',
      'border-right-style', 'border-right-width', 'border-spacing', 'border-style',
      'border-top', 'border-top-color', 'border-top-left-radius', 'border-top-right-radius',
      'border-top-style', 'border-top-width', 'border-width',
      //bottom 1
      'bottom',
      //box 3
      'box-decoration-break', 'box-shadow', 'box-sizing',
      //break 3
      'break-after', 'break-before', 'break-inside',
      //caption 1
      'caption-side',
      //caret 1
      'caret-color',
      //clear 1
      'clear',
      //clip 1
      'clip',
      //color 1
      'color',
      //column 10
      'column-count', 'column-fill', 'column-gap', 'column-rule', 'column-rule-color',
      'column-rule-style', 'column-rule-width', 'column-span', 'column-width',
      'columns',
      //content 1
      'content',
      //counter 2
      'counter-increment', 'counter-reset',
      //cursor 1
      'cursor',
      //direction 1
      'direction',
      //display 1
      'display',
      //empty-cells 1 --86
      'empty-cells',
      //filter 1
      'filter'//,
      //flex 7
      //'flex', 'flex-basis', 'flex-direction', 'flex-flow', 'flex-grow', 'flex-shrink',
      //'flex-wrap',
      ////float 1
      //'float',
      //// font 18  --
      //'font', 'font-family', 'font-feature-settings', 'font-kerning', 'font-language-override',
      //'font-size', 'font-size-adjust', 'font-stretch', 'font-style', 'font-synthesis',
      //'font-variant', 'font-variant-alternates', 'font-variant-caps', 'font-variant-east-asian',
      //'font-variant-ligatures', 'font-variant-numeric', 'font-variant-position',
      //'font-weight',
      //
      //// hanging 1
      //'hanging-punctuation',
      //// height 1
      //'height',
      //// hypens 1
      //'hyphens',
      //
      //// src 1
      //'src',
      //
      //// width 1
      //'width',
      //
      //// zindex 1
      //'z-index'
    ) deprecated;

type

  { Basic types }

  // Normal char
  TPLCSSChar = char;
  // Complex string
  TPLCSSString = string;
  // High precision floating number
  TPLCSSFloat = Extended;
  // The biggest signed integer
  TPLCSSInt = Int64;

  // Color simple format type
  TPLCSSColorSimpleFormat = (csfRGB, csfHSL);

  // Length unit type
  TPLCSSUnit = (cuNone, cuCm, cuMm, cuIn, cuPx, cuPt, cuPc, cuEm, cuEx, cuCh,
    cuRem, cuVw, cuVh, cuVmin, cuVmax, cuPercent);

  { Real values }

  TPLRealSize = record
    Width, Height: SizeInt;
  end;

  TPLRealPos = record
    Left, Top: SizeInt;
  end;

  TPLRealRect = record
    Position: TPLRealPos;
    Size: TPLRealSize;
  end;

  { IPLHTMLZoomed }

  IPLHTMLZoomed = interface
  ['{F44B52D9-0DEA-41CF-AE68-2CF23E3624C6}']
    function GetZoom: TPLCSSFloat;
    procedure SetZoom(AValue: TPLCSSFloat);

    property Zoom: TPLCSSFloat read GetZoom write SetZoom;
  end;

  { IPLCSSCustomControl }

  IPLCSSCustomControl = interface(IPLHTMLZoomed)
  ['{1E435637-E6F1-48C9-B4A4-864EE95813B7}']
    function GetRealRect: TPLRealRect;
    function GetViewport: IPLCSSCustomControl;
    function GetWindow: TCustomForm;
  end;

  { IPLHTMLBaseObject }

  IPLHTMLBaseObject = interface(IPLCSSCustomControl)
  ['{AF3E68BA-81FD-4906-B575-C1C08956AEAA}']
    function GetNode: TDOMNode;
  end;

  { TPLPair }

  generic TPLPair<A, B> = packed record
  public
    first: A;
    second: B;

    constructor Create(AFirst: A; ASecond: B);
  end;

  generic TPLPairs<T> = array of specialize TPLPair<T, T>;

  TPLStringPair = specialize TPLPair<string, string>;
  TPLStringPairs = specialize TPLPairs<string>;

  { TPLObjectList }

  generic TPLObjectList<T: class> = class(TObject)
  private type
    TListOfT = array of T;
  private
    FArray: TListOfT;
    FFreeObjects: boolean;
    FSize: SizeInt;
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);
  public
    constructor Create(AFreeObjects: boolean = true);
    destructor Destroy; override;

    procedure Add(AItem: T);
    procedure Remove(AItem: T);
    function Find(AItem: T): SizeInt;
    function Count: SizeInt;
    function Empty: boolean;
    procedure Clear;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TPLList }

  generic TPLList<T> = class(TObject)
  private type
    TListOfT = array of T;
  private
    FArray: TListOfT;
    FSize: SizeInt;
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AItem: T);
    procedure Remove(AItem: T);
    function Find(AItem: T): SizeInt;
    function Count: SizeInt;
    function Empty: boolean;
    procedure Clear;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { TPLCSSTypeUrl }

  TPLCSSTypeUrl = packed record
  strict private
    FLink: TPLCSSString;
  public
    constructor Create(AURL: TPLCSSString);

    property Link: TPLCSSString read FLink;
  end;

  { TPLCSSStringHelper }

  TPLCSSStringHelper = type helper(TStringHelper) for TPLCSSString
  public
    function SubStr(AFrom, ACount: SizeInt): TPLCSSString; overload;
    function SubStr(AFrom: SizeInt): TPLCSSString; overload;
    function Length: SizeInt;
    function Find(AWhat: TPLCSSString; AFrom: SizeInt = 1): SizeInt;
    function Exists(AWhat: TPLCSSString; ASearchFrom: SizeInt = 1): boolean;
    function FromC: TPLCSSString;
    function IsWhiteSpaceAt(APos: SizeInt = 1): boolean;
    function At(AIndex: SizeInt): TPLCSSString;
    function Codepoint(AIndex: SizeInt): Cardinal;
    function AsCSSCode(AIndex: SizeInt): TPLCSSString;
    function AsHTMLCode(AIndex: SizeInt): TPLCSSString;
    function IsEmoji(AIndex: SizeInt): boolean;
    function AsHex: TPLCSSString;
  end;

  { TPLCSSCharHelper }

  TPLCSSCharHelper = type helper for TPLCSSChar
  public
    function IsWhiteSpace: boolean;
    function FromHex: integer;
    function FromC: TPLCSSString;
  end;

  TPLCSSIntRange = packed record
    min, max: TPLCSSInt;
  end;

  function TryStrToByte(const s: TPLCSSString; out i: byte): boolean;

  function fmod(a, b: TPLCSSFloat): TPLCSSFloat;
  function MinMax(AValue, AFrom, ATo: TPLCSSInt): TPLCSSInt;

  function Range(const AMin, AMax: TPLCSSInt): TPLCSSIntRange;
  function InRanges(const AValue: TPLCSSInt; const ARanges: array of TPLCSSIntRange): boolean;

  procedure RGB2HSL(ARed, AGreen, ABlue: byte; out AHue, ASaturation, ALightness: TPLCSSFloat);
  procedure HSL2RGB(AHue, ASaturation, ALightness: TPLCSSFloat; out ARed, AGreen, ABlue: byte);

  // Scale value to screen DPI
  function ScaleValue(AValue: TPLCSSFloat; AZoomed: IPLHTMLZoomed = nil): TPLCSSInt;
  function ScaleValueF(AValue: TPLCSSFloat; AZoomed: IPLHTMLZoomed = nil): TPLCSSFloat;
  // Centimiters to pixels (DPI-aware)
  function CmToPxValue(AValue: TPLCSSFloat): TPLCSSInt;

  function ProcessImportant(var AValue: TPLCSSString): boolean;

  function IsInitValue(AValue: TPLCSSString): boolean;

  // useful operators
  operator := (a: TPLCSSFloat) b: TPLCSSString;
  operator := (a: TPLCSSString) b: TPLCSSFloat;
  operator := (a: TPLCSSInt) b: TPLCSSString;
  operator := (a: TPLCSSString) b: TPLCSSInt;
  operator := (a: TPLCSSFloat) b: boolean;

implementation

operator := (a: TPLCSSFloat) b: TPLCSSString;
begin
  b := FloatToStr(a, PLCSSFormatSettingsDef);
end;

operator := (a: TPLCSSString) b: TPLCSSFloat;
begin
  b := 0;
  TryStrToFloat(a, b, PLCSSFormatSettingsDef);
end;

operator := (a: TPLCSSInt) b: TPLCSSString;
begin
  b := IntToStr(a);
end;

operator := (a: TPLCSSString) b: TPLCSSInt;
begin
  b := 0;
  TryStrToInt64(a, b);
end;

operator := (a: TPLCSSFloat) b: boolean;
begin
  b := a <> 0;
end;

function TryStrToByte(const s: TPLCSSString; out i: byte): boolean;
var
  x: integer;
begin
  Result := TryStrToInt(s, x);
  if Result then i := byte(x);
end;

function fmod(a, b: TPLCSSFloat): TPLCSSFloat;
begin
  Result := abs(a);
  b := abs(b);

  while Result >= b do Result := Result-b;

  if a < 0 then exit(-Result);
end;

function MinMax(AValue, AFrom, ATo: TPLCSSInt): TPLCSSInt;
begin
  Result := Max(Min(AValue, ATo), AFrom);
end;

function Range(const AMin, AMax: TPLCSSInt): TPLCSSIntRange;
begin
  Result.min := AMin;
  Result.max := AMax;
end;

function InRanges(const AValue: TPLCSSInt;
  const ARanges: array of TPLCSSIntRange): boolean;
var
  i: SizeInt;
begin
  Result := false;

  for i := Low(ARanges) to High(ARanges) do begin
    if (ARanges[i].min <= AValue) and (AValue <= ARanges[i].max) then exit(true);
  end;
end;

procedure RGB2HSL(ARed, AGreen, ABlue: byte; out AHue, ASaturation, ALightness: TPLCSSFloat);
var
  r, g, b, cmin, cmax, delta: TPLCSSFloat;
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

procedure HSL2RGB(AHue, ASaturation, ALightness: TPLCSSFloat; out ARed, AGreen, ABlue: byte);
var
  h, s, l, x, c, m, r, g, b: TPLCSSFloat;
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

  if (0 <= h) and (h < 60) then begin
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

function ScaleValue(AValue: TPLCSSFloat; AZoomed: IPLHTMLZoomed): TPLCSSInt;
begin
  Result := round(ScaleValueF(AValue, AZoomed));
end;

function ScaleValueF(AValue: TPLCSSFloat; AZoomed: IPLHTMLZoomed): TPLCSSFloat;
var
  p: TPLCSSFloat;
begin
  if Assigned(AZoomed) then p := AZoomed.Zoom else p := 1;
  Result := Screen.PixelsPerInch/96*AValue*p;
end;

function CmToPxValue(AValue: TPLCSSFloat): TPLCSSInt;
begin
  Result := ScaleValue(AValue*PLCSSUnitPx);
end;

function ProcessImportant(var AValue: TPLCSSString): boolean;
begin
  Result := AValue.Trim.ToLower.EndsWith('!important');
  if Result then AValue := Copy(AValue, 1, Length(AValue) - Length('!important'));
end;

function IsInitValue(AValue: TPLCSSString): boolean;
begin
  AValue := AValue.Trim.ToLower;
  Result := (AValue = 'initial') or (AValue = '') or (AValue = 'inherit');
end;

{ TPLCSSStringHelper }

function TPLCSSStringHelper.SubStr(AFrom, ACount: SizeInt): TPLCSSString;
begin
  Result := UTF8Copy(Self, AFrom, ACount);
end;

function TPLCSSStringHelper.SubStr(AFrom: SizeInt): TPLCSSString;
begin
  Result := UTF8Copy(Self, AFrom, Length);
end;

function TPLCSSStringHelper.Length: SizeInt;
begin
  Result := UTF8Length(Self);
end;

function TPLCSSStringHelper.Find(AWhat: TPLCSSString; AFrom: SizeInt): SizeInt;
begin
  Result := UTF8Pos(AWhat, Self, AFrom);
end;

function TPLCSSStringHelper.Exists(AWhat: TPLCSSString; ASearchFrom: SizeInt
  ): boolean;
begin
  Result := Find(AWhat, ASearchFrom) > 0;
end;

function TPLCSSStringHelper.FromC: TPLCSSString;
begin
  Result := Utf8EscapeControlChars(Utf8EscapeControlChars(Self, emC), emHexC);
end;

function TPLCSSStringHelper.IsWhiteSpaceAt(APos: SizeInt): boolean;
begin
  if (APos < 0) or (APos > Length) then
    Result := false
  else
    Result := Self[APos] in [' ', #9, #10, #13, #12];
end;

function TPLCSSStringHelper.At(AIndex: SizeInt): TPLCSSString;
begin
  Result := SubStr(AIndex, 1);
end;

function TPLCSSStringHelper.Codepoint(AIndex: SizeInt): Cardinal;
var
  dummy: integer;
begin
  Result := UTF8CodepointToUnicode(PChar(At(AIndex)), dummy);
end;

function TPLCSSStringHelper.AsCSSCode(AIndex: SizeInt): TPLCSSString;
begin
  Result := '\' + IntToStr(Codepoint(AIndex)).AsHex;
end;

function TPLCSSStringHelper.AsHTMLCode(AIndex: SizeInt): TPLCSSString;
begin
  Result := '&#x' + IntToStr(Codepoint(AIndex)).AsHex + ';';
end;

// https://en.wikipedia.org/wiki/Emoji
function TPLCSSStringHelper.IsEmoji(AIndex: SizeInt): boolean;
var
  cp: TPLCSSInt;
begin
  cp := Codepoint(AIndex);
  Result := InRanges(cp, [
    Range($2700, $27BF), // Dingbats
    Range($1F600, $1F64F), // Emoticons
    Range($2600, $26FF), // Miscellaneous Symbols
    Range($1F300, $1F5FF), // Miscellaneous Symbols and Pictographs
    Range($1F900, $1F9FF), // Supplemental Symbols and Pictographs
    Range($1FA70, $1FAFF), // Symbols and Pictographs Extended-A
    Range($1F680, $1F6FF) // Transport and Map Symbols
  ]);
end;

function TPLCSSStringHelper.AsHex: TPLCSSString;
var
  a: SizeInt = 0;
begin
  TryStrToInt64(Self, a);
  Result := '%X'.Format([a]);
end;

{ TPLCSSCharHelper }

function TPLCSSCharHelper.IsWhiteSpace: boolean;
begin
  Result := TPLCSSString(Self).IsWhiteSpaceAt();
end;

function TPLCSSCharHelper.FromHex: integer;
begin
  Result := -1;
  TryStrToInt('$' + Self, Result);
end;

function TPLCSSCharHelper.FromC: TPLCSSString;
begin
  Result := TPLCSSString('\' + Self).FromC;
end;

{ TPLObjectList }

function TPLObjectList.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= FSize) then
    Result := nil
  else
    Result := FArray[AIndex];
end;

procedure TPLObjectList.SetItem(AIndex: SizeInt; AValue: T);
begin
  if (AIndex < 0) or (AIndex >= FSize) then exit;

  FArray[AIndex] := AValue;
end;

function TPLObjectList.Find(AItem: T): SizeInt;
var
  i: SizeInt;
begin
  Result := -1;
  for i := 0 to FSize-1 do
    if FArray[i] = AItem then exit(i);
end;

constructor TPLObjectList.Create(AFreeObjects: boolean);
begin
  inherited Create;

  FSize := 0;
  FFreeObjects := AFreeObjects;
  SetLength(FArray, 0);
end;

destructor TPLObjectList.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TPLObjectList.Add(AItem: T);
begin
  if FSize >= MaxListSize then exit;

  SetLength(FArray, FSize + 1);
  FArray[FSize] := AItem;
  FSize += 1;
end;

procedure TPLObjectList.Remove(AItem: T);
var
  id, i: SizeInt;
begin
  id := Find(AItem);

  if id < 0 then exit;
  if FFreeObjects then FArray[id].Free;

  for i := id to FSize-2 do
    FArray[i] := FArray[i + 1];

  FSize := FSize - 1;
  SetLength(FArray, FSize);
end;

function TPLObjectList.Count: SizeInt;
begin
  Result := FSize;
end;

function TPLObjectList.Empty: boolean;
begin
  Result := FSize = 0;
end;

procedure TPLObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then begin
    for i := 0 to FSize-1 do FArray[i].Free;
  end;

  FSize := 0;
  SetLength(FArray, FSize);
end;

{ TPLPair }

constructor TPLPair.Create(AFirst: A; ASecond: B);
begin
  first := AFirst;
  second := ASecond;
end;

{ TPLCSSTypeUrl }

constructor TPLCSSTypeUrl.Create(AURL: TPLCSSString);
begin
  AURL := AURL.Trim;
  FLink := '';

  if not AURL.ToLower.StartsWith('url(') or not AURL.EndsWith(')') then exit;

  FLink := AURL.SubStr(5, AURL.Length - 5).DeQuotedString('"').DeQuotedString('''');
end;

{ TPLList }

function TPLList.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= FSize) then
    Result := Default(T)
  else
    Result := FArray[AIndex];
end;

procedure TPLList.SetItem(AIndex: SizeInt; AValue: T);
begin
  if (AIndex < 0) or (AIndex >= FSize) then exit;

  FArray[AIndex] := AValue;
end;

constructor TPLList.Create;
begin
  inherited Create;

  FSize := 0;
  SetLength(FArray, 0);
end;

destructor TPLList.Destroy;
begin
  Clear;

  inherited Destroy;
end;

procedure TPLList.Add(AItem: T);
begin
  if FSize >= MaxListSize then exit;

  SetLength(FArray, FSize + 1);
  FArray[FSize] := AItem;
  FSize += 1;
end;

procedure TPLList.Remove(AItem: T);
var
  id, i: SizeInt;
begin
  id := Find(AItem);

  if id < 0 then exit;

  for i := id to FSize-2 do
    FArray[i] := FArray[i + 1];

  FSize := FSize - 1;
  SetLength(FArray, FSize);
end;

function TPLList.Find(AItem: T): SizeInt;
var
  i: SizeInt;
begin
  Result := -1;
  for i := 0 to FSize-1 do
    if FArray[i] = AItem then exit(i);
end;

function TPLList.Count: SizeInt;
begin
  Result := FSize;
end;

function TPLList.Empty: boolean;
begin
  Result := FSize = 0;
end;

procedure TPLList.Clear;
begin
  FSize := 0;
  SetLength(FArray, FSize);
end;

end.

