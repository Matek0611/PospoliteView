unit Pospolite.View.Basics;

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
{$modeswitch TypeHelpers}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, strutils, math, LazUTF8, Controls, LCLType, LCLProc,
  LCLIntf;

type

  // - Basic types - //

  // Normal char
  TPLChar = char;
  // Charset
  TPLCharSet = set of TPLChar;
  // Complex string
  TPLString = string;
  // High precision floating number
  TPLFloat = Double;
  // The biggest signed integer
  TPLInt = Int64;
  // Unsigned int
  TPLUInt = QWord;
  // Short int
  TPLShortInt = ShortInt;
  // Sign value
  TPLSign = -1..1;
  // Bool
  TPLBool = Boolean;

  // - Generics and normal types - //

  TPLNestedProc = procedure is nested;
  generic TPLNestedProcP1<T> = procedure(AParam: T) is nested;
  generic TPLNestedProcPn<T> = procedure(AParams: array of T) is nested;
  generic TPLNestedFunc<R> = function: R is nested;
  generic TPLNestedFuncP1<R, T> = function(AParam: T): R is nested;
  generic TPLNestedFuncPn<R, T> = function(AParams: array of T): R is nested;

  TPLArrayOfConst = array of TVarRec;

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

  { IPLCloneable }

  generic IPLCloneable<T> = interface
    ['{9BA2FE16-9C98-462E-BE86-5C28A6D18228}']

    function Clone: T;
  end;

  generic IPLSimpleList<T> = interface(specialize IEnumerable<T>)
    ['{85CB3AC4-43DA-4F67-814F-EC954688A12A}']

    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);

    function Count: SizeInt;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
  end;

  { IPLListBase }

  generic IPLListBase<T> = interface(specialize IPLSimpleList<T>)
    ['{8523A611-1885-49FC-9DD3-6B104AC4ED59}']
    procedure Add(AItem: T);
    procedure Remove(AItem: T);
    function Empty: TPLBool;
    procedure Clear;
    function Last: T;
    function First: T;
    function Poll: T;
    function Pop: T;
  end;

  { TPLListEnumerator }

  generic TPLListEnumerator<L, E> = class(TInterfacedObject, specialize IEnumerator<E>)
  private
    FList: L;
    FIndex: SizeInt;
  protected
    function GetCurrent: E;
  public
    constructor Create(const AList: L);
    function MoveNext: TPLBool;
    procedure Reset;

    property Current: E read GetCurrent;
  end;

  generic TPLObjectListFindCompare<T> = function(a, b: T): TPLBool of object;
  generic TPLObjectListSortCompare<T> = function(a, b: T): TPLSign of object;

  { IPLObjectList }

  generic IPLObjectList<T: class> = interface(specialize IPLListBase<T>)
    ['{5BA70B04-A7B1-45B6-852D-3EE2B0575121}']
    function Find(AItem: T; AComparator: specialize TPLObjectListFindCompare<T> = nil): SizeInt;
    procedure Sort(AComparator: specialize TPLObjectListSortCompare<T>);
    function Duplicate: specialize IPLObjectList<T>;
    procedure SetObjectsFreeing(const AValue: TPLBool);

    property FreeObjects: TPLBool;
  end;

  { TPLObjectList }

  generic TPLObjectList<T: class> = class(TInterfacedObject, specialize IPLObjectList<T>)
  private type
    TListOfT = array of T;
  protected
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
    type IObjectListEnumerator = specialize IEnumerator<T>;
    type TObjectListEnumerator = class(specialize TPLListEnumerator<TPLObjectList, T>, IObjectListEnumerator);

    function GetEnumerator: IObjectListEnumerator; reintroduce;
  public
    constructor Create(AFreeObjects: TPLBool = true);
    destructor Destroy; override;

    procedure Add(AItem: T); virtual;
    procedure CustomInsert(AIndex: SizeInt; AItem, ATempItem: T);
    procedure Insert(AIndex: SizeInt; AItem: T);
    procedure Replace(AIndex1, AIndex2: SizeInt);
    procedure Remove(AItem: T); virtual;
    function Find(AItem: T; AComparator: specialize TPLObjectListFindCompare<T> = nil): SizeInt; virtual;
    procedure Sort(AComparator: specialize TPLObjectListSortCompare<T>); inline;
    function Count: SizeInt;
    function Empty: TPLBool;
    procedure Clear; virtual;
    function Last: T;
    function First: T;
    function Poll: T;
    function Pop: T;
    function Duplicate: specialize IPLObjectList<T>;
    procedure SetObjectsFreeing(const AValue: TPLBool);

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
    property FreeObjects: TPLBool read FFreeObjects write FFreeObjects;
  end;

  { IPLList }

  generic IPLList<T> = interface(specialize IPLListBase<T>)
    ['{6BA50AFC-7FCD-4D09-9465-73C942F78AA3}']
    function GetData: Pointer;

    function Find(AItem: T): SizeInt;
    function Duplicate: specialize IPLList<T>;

    property Data: Pointer read GetData;
  end;

  { TPLList }

  generic TPLList<T> = class(TInterfacedObject, specialize IPLList<T>)
  protected type
    TListOfT = array of T;
  protected
    FArray: TListOfT;
    FSize: SizeInt;
    function GetData: Pointer;
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; AValue: T);
  public
    type IListEnumerator = specialize IEnumerator<T>;
    type TListEnumerator = class(specialize TPLListEnumerator<TPLList, T>, IListEnumerator);

    function GetEnumerator: IListEnumerator; reintroduce;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AItem: T); virtual;
    procedure Remove(AItem: T); virtual;
    function Find(AItem: T): SizeInt; virtual;
    function Count: SizeInt;
    function Empty: TPLBool;
    procedure Clear; virtual;
    function Last: T;
    function First: T;
    function Poll: T;
    function Pop: T;
    function Duplicate: specialize IPLList<T>; virtual;

    property Item[AIndex: SizeInt]: T read GetItem write SetItem; default;
    property Data: Pointer read GetData;
  end;

  { IPLInterfaceList }

  generic IPLInterfaceList<I> = interface(specialize IPLList<I>)
    ['{34A068B1-7410-420A-9B7E-90968F924C0D}']
    function {%H-}Duplicate: specialize IPLInterfaceList<I>;
  end;

  { TPLInterfaceList }

  generic TPLInterfaceList<I> = class(specialize TPLList<I>, specialize IPLInterfaceList<I>)
  public
    procedure Add(AItem: I); override;
    procedure Remove(AItem: I); override;
    procedure Clear; override;
    function Duplicate: specialize IPLInterfaceList<I>; overload; reintroduce;
  end;

  { TPLFuncs }

  generic TPLFuncs<T> = packed class sealed
  private type
    ListHelper = specialize TPLList<T>;
  public
    class procedure Swap(var A, B: T);
    class function NewArray(ATab: array of T): specialize TArray<T>;
    class function NewList(ATab: array of T): specialize TPLList<T>;
    class function ToString(ATab: specialize TArray<T>): TPLString; reintroduce;
    class procedure FillArray(var ATab: array of T; const AValue: T);
    class function Extract(const AList: specialize TPLList<T>): specialize TArray<T>; inline;
  end;

  TPLStringFuncs = specialize TPLFuncs<TPLString>;
  TPLIntFuncs = specialize TPLFuncs<TPLInt>;
  TPLFloatFuncs = specialize TPLFuncs<TPLFloat>;
  TPLPointerFuncs = specialize TPLFuncs<Pointer>;
  TPLVariantFuncs = specialize TPLFuncs<Variant>;

  generic TPLFuncsOfClassComparator<T: class> = function(const AObject: T;
    const ACriteria: Variant): TPLSign of object;

  { TPLFuncsOfClass }

  generic TPLFuncsOfClass<T: class> = packed class sealed
  private type
    ListHelper = specialize TPLObjectList<T>;
    ComparatorHelper = specialize TPLFuncsOfClassComparator<T>;
  private
    class function DefaultComparator(const AObject: T; const ACriteria: Variant): TPLSign;
    class function InternalBinarySearch(AList: ListHelper; const ACriteria: Variant;
      c: ComparatorHelper; ALeft, ARight: SizeInt): SizeInt;
    class function InternalSearch(AList: ListHelper; const ACriteria: Variant;
      c: ComparatorHelper): SizeInt;
  public
    class procedure Swap(var A, B: T);
    class function NewList(ATab: array of T; const AFreeObjects: TPLBool = true): specialize TPLObjectList<T>;
    class function NewArray(ATab: array of T): specialize TArray<T>;
    class function Extract(const AList: specialize TPLObjectList<T>): specialize TArray<T>; inline;
    // Fast (exponential) search works if only list is sorted!
    class function FastSearch(AList: specialize TPLObjectList<T>; const ACriteria: Variant;
      ACustomComparator: specialize TPLFuncsOfClassComparator<T> = nil): SizeInt;
  end;

  TPLStringArray = specialize TArray<TPLString>;
  TPLIntArray = specialize TArray<TPLInt>;
  TPLFloatArray = specialize TArray<TPLFloat>;
  TPLPointerArray = specialize TArray<Pointer>;
  TPLConstArray = specialize TArray<TVarRec>;

  { TPLParameter }

  generic TPLParameter<K, V> = packed record
  public
    Key: K;
    Value: V;

    constructor Create(AKey: K; AValue: V);
    class operator =(a, b: TPLParameter) r: TPLBool; inline;
  end;

  // - HTML/CSS and JS Objects' Basics - //

  { IPLJSBasicObject }

  IPLJSBasicObject = interface
    ['{215753F5-D90B-4F41-A39C-7CED096453AC}']
    function AsString: TPLString;
  end;

  TPLCSSElementState = (esNormal, esActive, esFocus, esFocusWithin, esTarget,
    esHover, esVisited, esFocusVisible);

  TPLHTMLObjectAttribute = specialize TPLParameter<TPLString, TPLString>;

  { IPLHTMLObjectAttributes }

  IPLHTMLObjectAttributes = interface(specialize IPLList<TPLHTMLObjectAttribute>)
    ['{CA105FEA-CCFF-4192-B17F-45161C3BDF35}']
    function GetCharset: TPLHTMLObjectAttribute;
    function GetClass: TPLHTMLObjectAttribute;
    function GetHref: TPLHTMLObjectAttribute;
    function GetId: TPLHTMLObjectAttribute;
    function GetName: TPLHTMLObjectAttribute;
    function GetRel: TPLHTMLObjectAttribute;
    function GetSrc: TPLHTMLObjectAttribute;
    function GetStyle: TPLHTMLObjectAttribute;
    function GetType: TPLHTMLObjectAttribute;

    function Get(AName: TPLString): TPLHTMLObjectAttribute;
    function Has(AName: TPLString): TPLBool;
    function ToString: TPLString;

    // Do not change the order of these properties!

    property &Class: TPLHTMLObjectAttribute read GetClass;
    property Name: TPLHTMLObjectAttribute read GetName;
    property Id: TPLHTMLObjectAttribute read GetId;
    property Style: TPLHTMLObjectAttribute read GetStyle;
    property Href: TPLHTMLObjectAttribute read GetHref;
    property Src: TPLHTMLObjectAttribute read GetSrc;
    property &Type: TPLHTMLObjectAttribute read GetType;
    property Charset: TPLHTMLObjectAttribute read GetCharset;
    property Rel: TPLHTMLObjectAttribute read GetRel;
  end;

  { TPLHTMLObjectAttributes }

  TPLHTMLObjectAttributes = class(specialize TPLList<TPLHTMLObjectAttribute>, IPLHTMLObjectAttributes)
  private
    FPtrs: array[0..8] of TPLHTMLObjectAttribute;

    function GetCharset: TPLHTMLObjectAttribute;
    function GetClass: TPLHTMLObjectAttribute;
    function GetHref: TPLHTMLObjectAttribute;
    function GetId: TPLHTMLObjectAttribute;
    function GetName: TPLHTMLObjectAttribute;
    function GetRel: TPLHTMLObjectAttribute;
    function GetSrc: TPLHTMLObjectAttribute;
    function GetStyle: TPLHTMLObjectAttribute;
    function GetType: TPLHTMLObjectAttribute;

    procedure UpdateConsts;
  public
    procedure Add(AItem: TPLHTMLObjectAttribute); override;
    procedure Remove(AItem: TPLHTMLObjectAttribute); override;
    procedure Clear; override;

    function Get(AName: TPLString): TPLHTMLObjectAttribute;
    function Has(AName: TPLString): TPLBool;
    function ToString: TPLString; reintroduce;

    property &Class: TPLHTMLObjectAttribute read GetClass;
    property Name: TPLHTMLObjectAttribute read GetName;
    property Id: TPLHTMLObjectAttribute read GetId;
    property Style: TPLHTMLObjectAttribute read GetStyle;
    property Href: TPLHTMLObjectAttribute read GetHref;
    property Src: TPLHTMLObjectAttribute read GetSrc;
    property &Type: TPLHTMLObjectAttribute read GetType;
    property Charset: TPLHTMLObjectAttribute read GetCharset;
    property Rel: TPLHTMLObjectAttribute read GetRel;
  end;

  // https://developer.mozilla.org/en-US/docs/Web/API/Node
  TPLHTMLObjectNodeType = (ontElementNode = 1, ontTextNode = 3, ontCDataSectionNode,
    ontProcessingInstructionNode = 7, ontCommentNode, ontDocumentNode, ontDocumentTypeNode,
    ontDocumentFragmentNode);

  IPLHTMLObject = interface;
  TPLHTMLObject = class;
  IPLHTMLObjects = specialize IPLObjectList<TPLHTMLObject>;
  TPLHTMLObjects = class(specialize TPLObjectList<TPLHTMLObject>, IPLHTMLObjects);

  TPLNestedHTMLObjectProc = specialize TPLNestedProcP1<TPLHTMLObject>;

  { IPLHTMLObject }

  IPLHTMLObject = interface(specialize IPLCloneable<IPLHTMLObject>)
    ['{37CA6394-6FDE-4E4A-A44D-EBCEC2EBED34}']
    function GetAttributes: TPLHTMLObjectAttributes;
    function GetChild(const AName: TPLString): TPLHTMLObject;
    function GetChildren: TPLHTMLObjects;
    function GetJSObject: IPLJSBasicObject;
    function GetName: TPLString;
    function GetNodeType: TPLHTMLObjectNodeType;
    function GetParent: TPLHTMLObject;
    function GetPosition: SizeInt;
    function GetState: TPLCSSElementState;
    function GetText: TPLString;
    function GetZoom: TPLFloat;
    procedure SetName(AValue: TPLString);
    procedure SetNodeType(AValue: TPLHTMLObjectNodeType);
    procedure SetParent(AValue: TPLHTMLObject);
    procedure SetPosition(AValue: SizeInt);
    procedure SetState(AValue: TPLCSSElementState);
    procedure SetText(AValue: TPLString);
    procedure SetZoom(AValue: TPLFloat);

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; deprecated;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; deprecated;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; deprecated;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; deprecated;
    function CSS_Get(APropName: TPLString): Pointer; deprecated; // get as pointer to css property value part class
    procedure CSS_Set(APropName: TPLString; const APropValue); deprecated; // set value to css property

    function GetCSSProperty(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true): Pointer;
    function GetCSSPropertyValue(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true; AIndex: SizeInt = 0): Pointer;
    procedure SetCSSPropertyValue(const AName: TPLString; const AValue: Pointer;
      AState: TPLCSSElementState; AIndex: SizeInt = 0);

    procedure UpdateScrollbars;
    procedure Draw(ADrawer: Pointer);
    function ToHTML: TPLString;
    function ToObject: TPLHTMLObject;
    function PositionInParent: SizeInt;
    procedure RefreshStyles(const AParentStyles);
    procedure UpdateLayoutForAll;
    procedure UpdateOwnLayout;
    procedure ApplyInlineStyles;

    property Zoom: TPLFloat read GetZoom write SetZoom;

    property State: TPLCSSElementState read GetState write SetState;

    property JSObject: IPLJSBasicObject read GetJSObject;

    property Attributes: TPLHTMLObjectAttributes read GetAttributes;
    property Parent: TPLHTMLObject read GetParent write SetParent;
    property Children: TPLHTMLObjects read GetChildren;
    property Child[const AName: TPLString]: TPLHTMLObject read GetChild;
    property Name: TPLString read GetName write SetName;
    property Text: TPLString read GetText write SetText;
    property Position: SizeInt read GetPosition write SetPosition;
    property NodeType: TPLHTMLObjectNodeType read GetNodeType write SetNodeType;
  end;

  { TPLHTMLObject }

  TPLHTMLObject = class(TInterfacedObject, IPLHTMLObject)
  protected
    FParent: TPLHTMLObject;
    FJSObject: IPLJSBasicObject;
    FState: TPLCSSElementState;
    FZoom: TPLFloat;
    FStates: array[TPLCSSElementState] of Pointer;
    FAttributes: TPLHTMLObjectAttributes;
    FChildren: TPLHTMLObjects;
    FName: TPLString;
    FText: TPLString;
    FPosition: SizeInt;
    FNodeType: TPLHTMLObjectNodeType;
  private
    FZIndex: TPLInt;

    function GetAttributes: TPLHTMLObjectAttributes;
    function GetChild(const AName: TPLString): TPLHTMLObject;
    function GetChildren: TPLHTMLObjects;
    function GetJSObject: IPLJSBasicObject;
    function GetName: TPLString;
    function GetNodeType: TPLHTMLObjectNodeType;
    function GetParent: TPLHTMLObject;
    function GetPosition: SizeInt;
    function GetState: TPLCSSElementState;
    function GetText: TPLString;
    function GetZoom: TPLFloat;
    procedure SetName(AValue: TPLString);
    procedure SetNodeType(AValue: TPLHTMLObjectNodeType);
    procedure SetParent(AValue: TPLHTMLObject);
    procedure SetPosition(AValue: SizeInt);
    procedure SetState(AValue: TPLCSSElementState);
    procedure SetText(AValue: TPLString);
    procedure SetZoom(AValue: TPLFloat);
  protected
    procedure DoDraw(ADrawer: Pointer); virtual;
    function DoToHTMLChildren: TPLString;
    procedure InitStates; virtual;
    procedure DoneStates; virtual;
  public
    constructor Create(AParent: TPLHTMLObject); virtual;
    destructor Destroy; override;

    function Clone: IPLHTMLObject; virtual;

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; virtual; deprecated;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; virtual; deprecated;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; virtual; deprecated;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; virtual; deprecated;
    function CSS_Get(APropName: TPLString): Pointer; virtual; deprecated;
    procedure CSS_Set(APropName: TPLString; const APropValue); virtual; deprecated;

    function GetCSSProperty(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true): Pointer; virtual;
    function GetCSSPropertyValue(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true; AIndex: SizeInt = 0): Pointer; virtual;
    procedure SetCSSPropertyValue(const AName: TPLString; const AValue: Pointer;
      AState: TPLCSSElementState; AIndex: SizeInt = 0); virtual;

    procedure UpdateScrollbars; virtual;
    procedure Draw(ADrawer: Pointer);
    function ToHTML: TPLString; virtual;
    function ToObject: TPLHTMLObject;
    function PositionInParent: SizeInt;
    procedure RefreshStyles(const AParentStyles); virtual;
    procedure UpdateLayoutForAll;
    procedure UpdateOwnLayout; virtual;
    procedure ApplyInlineStyles; virtual;

    function GetWidth: TPLFloat; virtual;
    function GetHeight: TPLFloat; virtual;
    function GetTop: TPLFloat; virtual;
    function GetLeft: TPLFloat; virtual;
    function GetElementTarget: Pointer; virtual;
    function GetIDFromParent: SizeInt;

    function IsVisible: TPLBool; virtual;
    function Display: TPLString; virtual;
    function IsLink: TPLBool; virtual;
    function PositionType: TPLString; virtual;
    function CoordsInObject(const AX, AY: TPLFloat): TPLBool; virtual;
    function CoordsInObjectOnly(const AX, AY: TPLFloat): TPLBool;

    property Zoom: TPLFloat read GetZoom write SetZoom;
    property State: TPLCSSElementState read GetState write SetState;
    property JSObject: IPLJSBasicObject read GetJSObject;
    property Attributes: TPLHTMLObjectAttributes read GetAttributes;
    property Parent: TPLHTMLObject read GetParent write SetParent;
    property Children: TPLHTMLObjects read GetChildren;
    property Child[const AName: TPLString]: TPLHTMLObject read GetChild;
    property Name: TPLString read GetName write SetName;
    property Text: TPLString read GetText write SetText;
    property Position: SizeInt read GetPosition write SetPosition;
    property NodeType: TPLHTMLObjectNodeType read GetNodeType write SetNodeType;
    property ZIndex: TPLInt read FZIndex write FZIndex;
  end;

  { IPLBasicDocument }

  IPLBasicDocument = interface
    ['{9060BA6C-72E2-4E7B-B0C2-1BC588DC802B}']
    function GetContent: TPLString;
    function GetMimeType: TPLString;
    function GetTitle: TPLString;
    procedure SetTitle(AValue: TPLString);

    procedure LoadFromLocalFile(const AFileName: TPLString);
    procedure LoadFromURL(const AFileName: TPLString);
    procedure LoadFromString(const AText: TPLString); // HTML only
    procedure SaveToLocalFile(const AFileName: TPLString);
    procedure Reload; // reload page which is from the local file or URL
    function IsLoaded: TPLBool;

    property Title: TPLString read GetTitle write SetTitle;
    property Content: TPLString read GetContent;
    property MimeType: TPLString read GetMimeType;
  end;

  { IPLHTMLDocument }

  IPLHTMLDocument = interface(IPLBasicDocument)
    ['{760627BC-3E74-4CE4-AD3B-256BDDA722B0}']
    function GetRoot: TPLHTMLObject;

    function querySelector(const AQuery: TPLString; AObject: TPLHTMLObject = nil): TPLHTMLObject;
    function querySelectorAll(const AQuery: TPLString; AObject: TPLHTMLObject = nil): TPLHTMLObjects;

    property Root: TPLHTMLObject read GetRoot;
  end;

  { TPLCustomControl }

  TPLCustomControl = class(TCustomControl)
  public
    procedure Redraw; virtual; abstract;
  end;

  // - Helpers - //

  { TPLStringHelper }

  TPLStringHelper = type helper(TStringHelper) for TPLString
  public const
    WhitespacesSet = [' ', #9, #10, #12, #13];
    WhitespacesArrayString: array[0..4] of TPLString = (' ', #9, #10, #12, #13);
    WhitespacesArrayChar: array[0..4] of TPLChar = (' ', #9, #10, #12, #13);
    Whitespaces = WhitespacesSet;
  public
    function SubStr(AFrom, ACount: SizeInt): TPLString; overload;
    function SubStr(AFrom: SizeInt): TPLString; overload;
    function Length: SizeInt;
    function Find(AWhat: TPLString; AFrom: SizeInt = 1): SizeInt;
    function Exists(AWhat: TPLString; ASearchFrom: SizeInt = 1): TPLBool;
    function Exists(AWhat: array of TPLString; ASearchFrom: SizeInt = 1): TPLBool;
    function FromC: TPLString;
    function IsWhiteSpaceAt(APos: SizeInt = 1): TPLBool;
    function At(AIndex: SizeInt): TPLString;
    function Codepoint(AIndex: SizeInt): Cardinal;
    function AsCSSCode(AIndex: SizeInt): TPLString;
    function AsHTMLCode(AIndex: SizeInt): TPLString;
    function IsEmoji(AIndex: SizeInt): TPLBool;
    function AsHex: TPLString;
    function FromHex: integer;
    function WithoutWhitespaces: TPLString;
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
  function ScaleLengthToScreen(AValueInPx: TPLFloat; AHTMLObject: TPLHTMLObject = nil): TPLFloat;
  function AbsoluteLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject = nil): TPLFloat; // px, cm, mm, Q, in, pc, pt => px (scaled)
  function RelativeLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject = nil): TPLFloat; // => px (scaled)
  function AutoLengthToPx(AValue: TPLFloat; const AUnit: TPLString; AHTMLObject: TPLHTMLObject = nil): TPLFloat; // auto detect absolute or relative value
  function ObjectToVariant(AObject: TObject): Variant;

  operator := (a: TPLFloat) b: TPLString;
  operator := (a: TPLString) b: TPLFloat;
  operator := (a: TPLInt) b: TPLString;
  operator := (a: TPLString) b: TPLInt;
  operator := (a: TPLFloat) b: TPLBool;
  operator := (a: TPLCSSElementState) b: TPLString;
  operator := (a: TPLString) b: TPLCSSElementState;
  operator * (a: TPLString; b: TPLInt) r: TPLString;
  operator mod (a, b: TPLFloat) r: TPLFloat;
  operator in (a: TPLString; tab: specialize TArray<TPLString>): TPLBool;
  operator in (a: TPLFloat; tab: specialize TArray<TPLFloat>): TPLBool;
  operator + (a, b: specialize TArray<TPLString>): specialize TArray<TPLString>;

implementation

uses Forms, variants;

// - Functions and operators - //
{$I pospolite.view.basics.funcs.inc}

// - Generics - //
{$I pospolite.view.basics.generics.inc}

// - Helpers - //
{$I pospolite.view.basics.helpers.inc}

end.

