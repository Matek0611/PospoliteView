unit Pospolite.View.Basics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, strutils, math, LazUTF8;

type

  // - Basic types - //

  // Normal char
  TPLChar = char;
  // Complex string
  TPLString = string;
  // High precision floating number
  TPLFloat = Double;
  // The biggest signed integer
  TPLInt = Int64;
  // Unsigned int
  TPLUInt = QWord;
  // Short Int
  TPLShortInt = ShortInt;
  // Sign value
  TPLSign = -1..1;
  // Bool
  TPLBool = Boolean;

  // - Generics - //

  { TPLNumberRange }

  generic TPLNumberRange<T> = packed record
  private type
    TPLNumberRangeT = specialize TPLNumberRange<T>;
  public
    min, max: T;

    constructor Create(const AMin, AMax: T);
    class function InRanges(const AValue: T; const ARanges: array of TPLNumberRangeT): TPLBool; static;
    function Correct(const ANumber: Variant): T;
  end;

  TPLIntRange = specialize TPLNumberRange<TPLInt>;
  TPLFloatRange = specialize TPLNumberRange<TPLFloat>;
  TPLByteRange = specialize TPLNumberRange<Byte>;

  generic TPLObjectListFindCompare<T> = function(a, b: T): TPLBool of object;
  generic TPLObjectListSortCompare<T> = function(a, b: T): TPLSign of object;

  { TPLObjectList }

  generic TPLObjectList<T: class> = class(TObject)
  private type
    TListOfT = array of T;
  private
    FArray: TListOfT;
    FFreeObjects: TPLBool;
    FSize: SizeInt;
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);
    function DefaultCompare(a, b: T): TPLBool; inline;
    procedure MergeArray(var AArray: TListOfT; l, r, x, y: TPLInt;
      AComparator: specialize TPLObjectListSortCompare<T>);
    procedure SortArray(AArray: TListOfT; ALeft, ARight: TPLInt;
      AComparator: specialize TPLObjectListSortCompare<T>);
  public
    constructor Create(AFreeObjects: TPLBool = true);
    destructor Destroy; override;

    procedure Add(AItem: T); virtual;
    procedure Remove(AItem: T); virtual;
    function Find(AItem: T; AComparator: specialize TPLObjectListFindCompare<T> = nil): SizeInt; virtual;
    procedure Sort(AComparator: specialize TPLObjectListSortCompare<T>); inline;
    function Count: SizeInt;
    function Empty: TPLBool;
    procedure Clear;
    function Last: T;
    function First: T;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
    property FreeObjects: TPLBool read FFreeObjects write FFreeObjects;
  end;

  { TPLList }

  generic TPLList<T> = class(TObject)
  private type
    TListOfT = array of T;
  private
    FArray: TListOfT;
    FSize: SizeInt;
    function GetData: Pointer;
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AItem: T); virtual;
    procedure Remove(AItem: T); virtual;
    function Find(AItem: T): SizeInt; virtual;
    function Count: SizeInt;
    function Empty: TPLBool;
    procedure Clear;
    function Last: T;
    function First: T;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
    property Data: Pointer read GetData;
  end;

  { TPLInterfaceList }

  TPLInterfaceList = specialize TPLList<IInterface>;

  { IPLCloneable }

  generic IPLCloneable<T> = interface
    ['{9BA2FE16-9C98-462E-BE86-5C28A6D18228}']

    function Clone: T;
  end;

  // - Helpers - //

  { TPLStringHelper }

  TPLStringHelper = type helper(TStringHelper) for TPLString
  public
    function SubStr(AFrom, ACount: SizeInt): TPLString; overload;
    function SubStr(AFrom: SizeInt): TPLString; overload;
    function Length: SizeInt;
    function Find(AWhat: TPLString; AFrom: SizeInt = 1): SizeInt;
    function Exists(AWhat: TPLString; ASearchFrom: SizeInt = 1): TPLBool;
    function FromC: TPLString;
    function IsWhiteSpaceAt(APos: SizeInt = 1): TPLBool;
    function At(AIndex: SizeInt): TPLString;
    function Codepoint(AIndex: SizeInt): Cardinal;
    function AsCSSCode(AIndex: SizeInt): TPLString;
    function AsHTMLCode(AIndex: SizeInt): TPLString;
    function IsEmoji(AIndex: SizeInt): TPLBool;
    function AsHex: TPLString;
    function FromHex: integer;
  end;

  { TPLCharHelper }

  TPLCharHelper = type helper for TPLChar
  public
    function IsWhiteSpace: TPLBool;
    function FromHex: integer;
    function FromC: TPLString;
  end;

  // - Consts - //

const

  // Default Format Settings
  PLFormatSettingsDef: TFormatSettings = (
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
  STANDARD_HTML_COLORS: array[0..149] of array[0..1] of TPLString = (
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

  // - Functions and operators - //

  function Range(const AMin, AMax: TPLInt): TPLIntRange;
  function InRanges(const AValue: TPLInt; const ARanges: array of TPLIntRange): TPLBool;
  function fmod(a, b: TPLFloat): TPLFloat;
  function AngleDeg(AAngle: TPLFloat; const AUnit: TPLString = 'deg'): TPLFloat;

  operator := (a: TPLFloat) b: TPLString;
  operator := (a: TPLString) b: TPLFloat;
  operator := (a: TPLInt) b: TPLString;
  operator := (a: TPLString) b: TPLInt;
  operator := (a: TPLFloat) b: TPLBool;
  operator * (a: TPLString; b: TPLInt) r: TPLString;
  operator mod (a, b: TPLFloat) r: TPLFloat;

implementation

// - Functions and operators - //
{$I pospolite.view.basics.funcs.inc}

// - Generics - //
{$I pospolite.view.basics.generics.inc}

// - Helpers - //
{$I pospolite.view.basics.helpers.inc}

end.

