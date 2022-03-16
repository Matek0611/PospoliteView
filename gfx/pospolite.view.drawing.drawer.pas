unit Pospolite.View.Drawing.Drawer;

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
  Classes, SysUtils, Graphics, math, Pospolite.View.Basics,
  Pospolite.View.Drawing.Basics;

type

  IPLDrawingMatrix = interface;
  IPLDrawingBrush = interface;
  IPLDrawingPen = interface;
  IPLDrawingPath = interface;

  IPLDrawingRenderer = interface;

  { IPLDrawingMatrix }

  IPLDrawingMatrix = interface(specialize IPLCloneable<IPLDrawingMatrix>)
    ['{194299D0-683F-46F2-9D87-DE2F0F7DEAE1}']

    procedure Multiply(AMatrix: IPLDrawingMatrix);
    procedure Scale(AX, AY: TPLFloat);
    procedure Translate(AX, AY: TPLFloat);
    function Transform(AP: TPLPointF): TPLPointF;
    procedure Identify;
    procedure Rotate(ARad: TPLFloat);
  end;

  operator * (a, b: IPLDrawingMatrix) r: IPLDrawingMatrix;

type

  { IPLDrawingBrush }

  IPLDrawingBrush = interface
    ['{B4608CAC-2938-40C8-9653-CF3C3753AD58}']

    function GetAlpha: Byte;
    function GetMatrix: IPLDrawingMatrix;
    procedure SetAlpha(AValue: Byte);
    procedure SetMatrix(AValue: IPLDrawingMatrix);

    property Alpha: Byte read GetAlpha write SetAlpha;
    property Matrix: IPLDrawingMatrix read GetMatrix write SetMatrix;
  end;

  { IPLDrawingBrushSolid }

  IPLDrawingBrushSolid = interface(IPLDrawingBrush)
    ['{2B10E8A4-059C-40C4-8E40-FE7DB5E9A127}']

    function GetColor: TPLColor;
    procedure SetColor(AValue: TPLColor);

    property Color: TPLColor read GetColor write SetColor;
  end;

  { IPLDrawingBrushBitmap }

  IPLDrawingBrushBitmap = interface(IPLDrawingBrush)
    ['{D03694C0-EA34-4B94-BE3A-5FC2D376FF1D}']
  end;

  TPLDrawingBrushGradientWrap = (dbgwClamp, dbgwWrap, dbgwMirror);

  { IPLDrawingBrushGradient }

  IPLDrawingBrushGradient = interface(IPLDrawingBrush)
    ['{967EFB69-F638-4554-A75A-715DE97FF520}']

    function GetWrap: TPLDrawingBrushGradientWrap;
    procedure SetWrap(AValue: TPLDrawingBrushGradientWrap);

    procedure AddStop(AColor: TPLColor; AOffset: TPLFloat);

    property Wrap: TPLDrawingBrushGradientWrap read GetWrap write SetWrap;
  end;

  { IPLDrawingBrushGradientLinear }

  IPLDrawingBrushGradientLinear = interface(IPLDrawingBrushGradient)
    ['{9764B12F-E95C-4DD6-A2FF-8B589C8B5CA7}']
  end;

  { IPLDrawingBrushGradientRadial }

  IPLDrawingBrushGradientRadial = interface(IPLDrawingBrushGradient)
    ['{3273762C-3985-48CF-906C-BB9DFBFCD694}']
  end;

  TPLDrawingPenStyle = (dpsSolid, dpsDash, dpsDot, dpsDashDot, dpsDashDotDot,
    dpsCustom);
  TPLDrawingPenEndCap = (dpecFlat, dpecSquare, dpecRound, dpecTriangle);
  TPLDrawingPenJoinStyle = (dpjsMitter, dpjsBevel, dpjsRound);

  { IPLDrawingPen }

  IPLDrawingPen = interface
    ['{C2562BD8-4609-4DAA-8CEC-A72DE109CC03}']

    function GetBrush: IPLDrawingBrush;
    function GetColor: TPLColor;
    function GetEndCap: TPLDrawingPenEndCap;
    function GetJoinStyle: TPLDrawingPenJoinStyle;
    function GetMiterLimit: TPLFloat;
    function GetStyle: TPLDrawingPenStyle;
    function GetStyleOffset: TPLFloat;
    function GetWidth: TPLFloat;
    procedure SetBrush(AValue: IPLDrawingBrush);
    procedure SetColor(AValue: TPLColor);
    procedure SetEndCap(AValue: TPLDrawingPenEndCap);
    procedure SetJoinStyle(AValue: TPLDrawingPenJoinStyle);
    procedure SetMiterLimit(AValue: TPLFloat);
    procedure SetStyle(AValue: TPLDrawingPenStyle);
    procedure SetStyleOffset(AValue: TPLFloat);
    procedure SetWidth(AValue: TPLFloat);

    property Color: TPLColor read GetColor write SetColor;
    property Width: TPLFloat read GetWidth write SetWidth;
    property Brush: IPLDrawingBrush read GetBrush write SetBrush;
    property Style: TPLDrawingPenStyle read GetStyle write SetStyle;
    property EndCap: TPLDrawingPenEndCap read GetEndCap write SetEndCap;
    property JoinStyle: TPLDrawingPenJoinStyle read GetJoinStyle write SetJoinStyle;
    property StyleOffset: TPLFloat read GetStyleOffset write SetStyleOffset;
    property MiterLimit: TPLFloat read GetMiterLimit write SetMiterLimit;
  end;

  TPLDrawingFontWeight = (dfw100, dfw200, dfw300, dfw400, dfw500, dfw600, dfw700,
    dfw800, dfw900, dfwNormal = dfw400, dfwBold = dfw700);
  TPLDrawingFontStyle = (dfsNormal, dfsItalic, dfsOblique);
  TPLDrawingFontStretch = (dfstUltraCondensed, dfstExtraCondensed, dfstCondensed,
    dfstSemiCondensed, dfstNormal, dfstSemiExpanded, dfstExpanded, dfstExtraExpanded,
    dfstUltraExpanded);
  TPLDrawingFontDecoration = (dfdUnderline, dfdLineThrough, dfdOverline);
  TPLDrawingFontDecorations = set of TPLDrawingFontDecoration;
  TPLDrawingFontVariantTag = (dfvtCapsSmall, dfvtCapsAllSmall, dfvtCapsPetite,
    dfvtCapsAllPetite, dfvtCapsTitling, dfvtUnicase, dfvtKerningNone, dfvtEastAsianRuby,
    dfvtEastAsianJIS78, dfvtEastAsianJIS83, dfvtEastAsianJIS90, dfvtEastAsianJIS04,
    dfvtEastAsianSimplified, dfvtEastAsianTraditional, dfvtEastAsianFullWidth,
    dfvtEastAsianProportionalWidth, dfvtLigaturesNone, dfvtLigaturesCommon,
    dfvtLigaturesNoCommon, dfvtLigaturesDiscretionary, dfvtLigaturesNoDiscretionary,
    dfvtLigaturesHistorical, dfvtLigaturesNoHistorical, dfvtLigaturesContextual,
    dfvtLigaturesNoContextual, dfvtNumericOrdinal, dfvtNumericSlashedZero,
    dfvtNumericLiningNums, dfvtNumericOldstyleNums, dfvtNumericProportionalNums,
    dfvtNumericTabularNums, dfvtNumericDiagonalFractions, dfvtNumericStackedFractions);
  TPLDrawingFontVariantTags = set of TPLDrawingFontVariantTag;

  { IPLDrawingFont }

  IPLDrawingFont = interface
    ['{6F45CBA7-F20E-4C5E-BA07-1BE5243A74D7}']

    function GetColor: TPLColor;
    function GetDecoration: TPLDrawingFontDecorations;
    function GetName: TPLString;
    function GetQuality: TFontQuality;
    function GetSize: TPLFloat;
    function GetStretch: TPLDrawingFontStretch;
    function GetStyle: TPLDrawingFontStyle;
    function GetVariantTags: TPLDrawingFontVariantTags;
    function GetWeight: TPLDrawingFontWeight;
    procedure SetColor(AValue: TPLColor);
    procedure SetDecoration(AValue: TPLDrawingFontDecorations);
    procedure SetName(AValue: TPLString);
    procedure SetQuality(AValue: TFontQuality);
    procedure SetSize(AValue: TPLFloat);
    procedure SetStretch(AValue: TPLDrawingFontStretch);
    procedure SetStyle(AValue: TPLDrawingFontStyle);
    procedure SetVariantTags(AValue: TPLDrawingFontVariantTags);
    procedure SetWeight(AValue: TPLDrawingFontWeight);

    property Name: TPLString read GetName write SetName;
    property Color: TPLColor read GetColor write SetColor;
    property Quality: TFontQuality read GetQuality write SetQuality;
    property Size: TPLFloat read GetSize write SetSize;
    property Weight: TPLDrawingFontWeight read GetWeight write SetWeight;
    property Style: TPLDrawingFontStyle read GetStyle write SetStyle;
    property Stretch: TPLDrawingFontStretch read GetStretch write SetStretch;
    property Decoration: TPLDrawingFontDecorations read GetDecoration write SetDecoration;
    property VariantTags: TPLDrawingFontVariantTags read GetVariantTags write SetVariantTags;
  end;

  { TPLDrawingFontData }

  TPLDrawingFontData = packed record
  public
    Name: TPLString;
    Color: TPLColor;
    Quality: TFontQuality;
    Size: TPLFloat;
    Weight: TPLDrawingFontWeight;
    Style: TPLDrawingFontStyle;
    Stretch: TPLDrawingFontStretch;
    Decoration: TPLDrawingFontDecorations;
    VariantTags: TPLDrawingFontVariantTags;
  end;

const
   PLDrawingFontDataDef: TPLDrawingFontData = (Name: 'Segoe UI';
     Color: (FR: 0; FG: 0; FB: 0; FA: 255); Quality: fqCleartypeNatural;
     Size: 12; Weight: dfwNormal; Style: dfsNormal; Stretch: dfstNormal;
     Decoration: []; VariantTags: []);

type

  { IPLDrawingPathData }

  IPLDrawingPathData = interface
    ['{C7C5B06B-84C8-4619-BFA0-FA47F21DCA2F}']
  end;

  { IPLDrawingPath }

  IPLDrawingPath = interface(specialize IPLCloneable<IPLDrawingPathData>)
    ['{87EEE872-D595-4380-9FED-CCB7C573569A}']

    procedure Add;
    procedure JoinData(APath: IPLDrawingPathData);
    procedure Remove;
    procedure Clip;
    procedure Unclip;
  end;

  IPLDrawingBitmap = interface;

  TPLTextDirection = (tdLeft, tdUp, tdRight, tdDown, tdCenter, tdFill, tdWrap, tdFlow);

  { TPLTextDirections }

  TPLTextDirections = record
  public
    TextPosition, ParagraphPosition: TPLTextDirection;
  public
    constructor Create(const ATextPosition, AParagraphPosition: TPLTextDirection);
  end;

  TPLWritingMode = (wmHorizontalTB = 0, wmVerticalLR = 2, wmVerticalRL = 3);
  TPLReadingDirection = (rdLTR = 0, rdRTL, rdTTB, rdBTT);

  { IPLDrawingSurface }

  IPLDrawingSurface = interface
    ['{79AD199D-585D-4070-A039-56C1E8FA732E}']

    function GetHandle: Pointer;
    function GetMatrix: IPLDrawingMatrix;
    function GetPath: IPLDrawingPath;
    procedure SetMatrix(AValue: IPLDrawingMatrix);

    procedure Save;
    procedure Restore;
    procedure Flush;
    procedure Clear(const AColor: TPLColor);
    procedure MoveTo(const AX, AY: TPLFloat);
    procedure LineTo(const AX, AY: TPLFloat);
    procedure ArcTo(const ARect: TPLRectF; const ABeginAngle, AEndAngle: TPLFloat);
    procedure CurveTo(const AX, AY: TPLFloat; const AC1, AC2: TPLPointF);
    procedure Ellipse(const ARect: TPLRectF);
    procedure Stroke(APen: IPLDrawingPen; const APreserve: TPLBool = false);
    procedure Fill(ABrush: IPLDrawingBrush; const APreserve: TPLBool = false);
    function TextSize(AFont: IPLDrawingFont; const AText: string;
      const ALineSpacing: single = NaN; const AWordWrap: boolean = true;
      const AWritingMode: TPLWritingMode = wmHorizontalTB;
      const AReading: TPLReadingDirection = rdLTR): TPLPointF;
    function TextHeight(AFont: IPLDrawingFont; const AText: string; const AWidth: TPLFloat;
      const ALineSpacing: single = NaN; const AWordWrap: boolean = true;
      const AWritingMode: TPLWritingMode = wmHorizontalTB;
      const AReading: TPLReadingDirection = rdLTR): TPLFloat;
    procedure TextOut(AFont: IPLDrawingFont; const AText: string; const ARect: TPLRectF;
      const ADirection: TPLTextDirections; const ALineSpacing: single = NaN;
      const AWordWrap: boolean = true; const AWritingMode: TPLWritingMode = wmHorizontalTB;
      const AReading: TPLReadingDirection = rdLTR; const AImmediate: TPLBool = true);

    property Handle: Pointer read GetHandle;
    property Matrix: IPLDrawingMatrix read GetMatrix write SetMatrix;
    property Path: IPLDrawingPath read GetPath;
  end;

  TPLDrawingResampleQuality = (drqLowest, drqNormal, drqBest);
  TPLDrawingImageFormat = (difPng, difJpeg, difGif, difBmp, difIco, difTiff);

  { IPLDrawingBitmap }

  IPLDrawingBitmap = interface(specialize IPLCloneable<IPLDrawingBitmap>)
    ['{7B622CB6-AF81-46C6-A85F-A406E6B7CF4E}']

    function GetClientRect: TPLRectI;
    function GetEmpty: TPLBool;
    function GetFormat: TPLDrawingImageFormat;
    function GetHeight: TPLInt;
    function GetPixels: PPLPixel;
    function GetSurface: IPLDrawingSurface;
    function GetWidth: TPLInt;

    procedure SetFormat(AValue: TPLDrawingImageFormat);
    procedure LoadFromFile(const AFileName: TPLString);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TPLString);
    procedure SaveToStream(AStream: TStream);
    procedure Clear;
    function Resample(AWidth, AHeight: TPLInt; AQuality: TPLDrawingResampleQuality = drqNormal): IPLDrawingBitmap;
    procedure SetSize(AWidth, AHeight: TPLInt);

    property Empty: TPLBool read GetEmpty;
    property Surface: IPLDrawingSurface read GetSurface;
    property ClientRect: TPLRectI read GetClientRect;
    property Format: TPLDrawingImageFormat read GetFormat write SetFormat;
    property Width: TPLInt read GetWidth;
    property Height: TPLInt read GetHeight;
    property Pixels: PPLPixel read GetPixels;
  end;

  { TPLDrawingFastBitmap }

  TPLDrawingFastBitmap = record

  end;

  { TPLDrawingInterfacedBitmap }

  TPLDrawingInterfacedBitmap = class(TInterfacedObject, IPLDrawingBitmap)
  private
    FWidth: Integer;
    FHeight: Integer;
    FFormat: TPLDrawingImageFormat;
  protected
    FBitmap: TPLDrawingFastBitmap;
    FSurface: IPLDrawingSurface;

    function HandleAvailable: Boolean; virtual;
    procedure HandleRelease; virtual;
    procedure Flush;
    function GetClientRect: TPLRectI;
    function GetEmpty: TPLBool;
    function GetFormat: TPLDrawingImageFormat;
    function GetHeight: TPLInt;
    function GetPixels: PPLPixel; virtual;
    function GetSurface: IPLDrawingSurface; virtual;
    function GetWidth: TPLInt;
  public
    destructor Destroy; override;
    procedure SetFormat(AValue: TPLDrawingImageFormat);
    procedure LoadFromFile(const AFileName: TPLString);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: TPLString);
    procedure SaveToStream(AStream: TStream);
    procedure Clear; virtual;
    function Resample(AWidth, AHeight: TPLInt; AQuality: TPLDrawingResampleQuality = drqNormal): IPLDrawingBitmap;
    procedure SetSize(AWidth, AHeight: TPLInt);
    function Clone: IPLDrawingBitmap;

    property Empty: TPLBool read GetEmpty;
    property Surface: IPLDrawingSurface read GetSurface;
    property ClientRect: TPLRectI read GetClientRect;
    property Format: TPLDrawingImageFormat read GetFormat write SetFormat;
    property Width: TPLInt read GetWidth;
    property Height: TPLInt read GetHeight;
    property Pixels: PPLPixel read GetPixels;
  end;

  // https://developer.mozilla.org/en-US/docs/Web/CSS/border-style

  TPLDrawingBorderStyle = (dbsNone, dbsHidden = dbsNone, dbsDotted, dbsDashed,
    dbsSolid, dbsDouble, dbsGroove, dbsRidge, dbsInset, dbsOutset);

  { TPLDrawingBorder }

  TPLDrawingBorder = packed record
  public
    Width: TPLFloat;
    Color: TPLColor;
    Style: TPLDrawingBorderStyle;

    constructor Create(const AWidth: TPLFloat; const AColor: TPLColor;
      const AStyle: TPLDrawingBorderStyle);
  end;

  { TPLDrawingBorders }

  TPLDrawingBorders = packed record
  public type
    TPLBorderRadiusData = array[1..4] of TPLFloat;
  public
    Left, Top, Right, Bottom: TPLDrawingBorder;
    Radius: TPLBorderRadiusData; // 1 - top-left, 2 - top-right, 3 - bottom-left, 4 - bottom-right
    Image: IPLDrawingBrush;

    constructor Create(const ALeft, ATop, ARight, ABottom: TPLDrawingBorder;
      ARadius: TPLBorderRadiusData; const AImage: IPLDrawingBrush = nil);
    function AverageBorderSize: TPLFloat;
  end;

  operator := (a: TPLString) r: TPLDrawingBorderStyle;
  function PLDrawingBordersDef: TPLDrawingBorders;
  function PLDrawingBordersOutlineDef: TPLDrawingBorders;
  function PLDrawingBordersRadiusData(b1, b2, b3, b4: TPLFloat): TPLDrawingBorders.TPLBorderRadiusData;

type

  { IPLDrawingRenderer }

  IPLDrawingRenderer = interface
    ['{0F99114C-4A69-43EB-A80C-3D5A247E2620}']

    function GetCanvas: TCanvas;
    function GetSurface: IPLDrawingSurface;

    procedure DrawObjectFully(const AObject: TPLHTMLObject; const ADebug: boolean = false);
    procedure DrawObjectBackground(const AObject: TPLHTMLObject);
    procedure DrawObjectBorder(const AObject: TPLHTMLObject);
    procedure DrawObjectOutline(const AObject: TPLHTMLObject);
    procedure DrawObjectOutlineText(const AObject: TPLHTMLObject);
    procedure DrawObjectText(const AObject: TPLHTMLObject);
    procedure DrawObjectDebug(const AObject: TPLHTMLObject);
    procedure DrawObjectShadow(const AObject: TPLHTMLObject);
    procedure DrawObjectTextShadow(const AObject: TPLHTMLObject);
    procedure DrawObjectPseudoBefore(const AObject: TPLHTMLObject);
    procedure DrawObjectPseudoAfter(const AObject: TPLHTMLObject);

    procedure DrawFrame(const ABorders: TPLDrawingBorders; const ARect: TPLRectF);

    property Canvas: TCanvas read GetCanvas;
    property Surface: IPLDrawingSurface read GetSurface;
  end;

  { TPLAbstractDrawer }

  TPLAbstractDrawer = class(TInterfacedObject, IPLDrawingRenderer)
  private
    function GetCanvas: TCanvas;
    function GetSurface: IPLDrawingSurface;
  protected
    FCanvas: TCanvas;
    FSurface: IPLDrawingSurface;
  public
    constructor Create(ACanvas: TCanvas); virtual;

    procedure DrawObjectBackground(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectBorder(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectDebug(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectFully(const AObject: TPLHTMLObject; const ADebug: boolean = false);
    procedure DrawObjectOutline(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectOutlineText(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectPseudoAfter(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectPseudoBefore(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectShadow(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectText(const AObject: TPLHTMLObject); virtual;
    procedure DrawObjectTextShadow(const AObject: TPLHTMLObject); virtual;

    procedure DrawFrame(const ABorders: TPLDrawingBorders; const ARect: TPLRectF); virtual;

    property Canvas: TCanvas read GetCanvas;
    property Surface: IPLDrawingSurface read GetSurface;
  end;

implementation

operator * (a, b: IPLDrawingMatrix) r: IPLDrawingMatrix;
begin
  r := a;
  r.Multiply(b);
end;

{ TPLTextDirections }

constructor TPLTextDirections.Create(const ATextPosition,
  AParagraphPosition: TPLTextDirection);
begin
  TextPosition := ATextPosition;
  ParagraphPosition := AParagraphPosition;
end;

operator := (a: TPLString) r: TPLDrawingBorderStyle;
begin
  case a.Trim.ToLower of
    'none': r := dbsNone;
    'dashed': r := dbsDashed;
    'dotted': r := dbsDotted;
    'double': r := dbsDouble;
    'groove': r := dbsGroove;
    'hidden': r := dbsHidden;
    'inset': r := dbsInset;
    'outset': r := dbsOutset;
    'ridge': r := dbsRidge;
    'solid': r := dbsSolid;
    else r := dbsNone;
  end;
end;

function PLDrawingBordersDef: TPLDrawingBorders;
var
  b: TPLDrawingBorder;
begin
  b := TPLDrawingBorder.Create(1, TPLColor.Black, dbsSolid);
  Result := TPLDrawingBorders.Create(b, b, b, b, PLDrawingBordersRadiusData(0, 0, 0, 0));
end;

function PLDrawingBordersOutlineDef: TPLDrawingBorders;
var
  b: TPLDrawingBorder;
begin
  b := TPLDrawingBorder.Create(3, TPLColor.Transparent, dbsNone);
  Result := TPLDrawingBorders.Create(b, b, b, b, PLDrawingBordersRadiusData(0, 0, 0, 0));
end;

function PLDrawingBordersRadiusData(b1, b2, b3, b4: TPLFloat
  ): TPLDrawingBorders.TPLBorderRadiusData;
begin
  Result[1] := b1;
  Result[2] := b2;
  Result[3] := b3;
  Result[4] := b4;
end;

{ TPLDrawingBorder }

constructor TPLDrawingBorder.Create(const AWidth: TPLFloat;
  const AColor: TPLColor; const AStyle: TPLDrawingBorderStyle);
begin
  Width := AWidth;
  Color := AColor;
  Style := AStyle;
end;

{ TPLDrawingBorders }

constructor TPLDrawingBorders.Create(const ALeft, ATop, ARight,
  ABottom: TPLDrawingBorder; ARadius: TPLBorderRadiusData;
  const AImage: IPLDrawingBrush);
begin
  Left := ALeft;
  Top := ATop;
  Right := ARight;
  Bottom := ABottom;
  Radius := ARadius;
  Image := AImage;
end;

function TPLDrawingBorders.AverageBorderSize: TPLFloat;
begin
  Result := (Left.Width + Top.Width + Right.Width + Bottom.Width) / 4;
end;

{ TPLDrawingInterfacedBitmap }

function TPLDrawingInterfacedBitmap.HandleAvailable: Boolean;
begin

end;

procedure TPLDrawingInterfacedBitmap.HandleRelease;
begin

end;

procedure TPLDrawingInterfacedBitmap.Flush;
begin

end;

function TPLDrawingInterfacedBitmap.GetClientRect: TPLRectI;
begin

end;

function TPLDrawingInterfacedBitmap.GetEmpty: TPLBool;
begin

end;

function TPLDrawingInterfacedBitmap.GetFormat: TPLDrawingImageFormat;
begin

end;

function TPLDrawingInterfacedBitmap.GetHeight: TPLInt;
begin

end;

function TPLDrawingInterfacedBitmap.GetPixels: PPLPixel;
begin

end;

function TPLDrawingInterfacedBitmap.GetSurface: IPLDrawingSurface;
begin

end;

function TPLDrawingInterfacedBitmap.GetWidth: TPLInt;
begin

end;

destructor TPLDrawingInterfacedBitmap.Destroy;
begin
  inherited Destroy;
end;

procedure TPLDrawingInterfacedBitmap.SetFormat(AValue: TPLDrawingImageFormat);
begin

end;

procedure TPLDrawingInterfacedBitmap.LoadFromFile(const AFileName: TPLString);
begin

end;

procedure TPLDrawingInterfacedBitmap.LoadFromStream(AStream: TStream);
begin

end;

procedure TPLDrawingInterfacedBitmap.SaveToFile(const AFileName: TPLString);
begin

end;

procedure TPLDrawingInterfacedBitmap.SaveToStream(AStream: TStream);
begin

end;

procedure TPLDrawingInterfacedBitmap.Clear;
begin

end;

function TPLDrawingInterfacedBitmap.Resample(AWidth, AHeight: TPLInt;
  AQuality: TPLDrawingResampleQuality): IPLDrawingBitmap;
begin

end;

procedure TPLDrawingInterfacedBitmap.SetSize(AWidth, AHeight: TPLInt);
begin

end;

function TPLDrawingInterfacedBitmap.Clone: IPLDrawingBitmap;
begin

end;

{ TPLAbstractDrawer }

function TPLAbstractDrawer.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TPLAbstractDrawer.GetSurface: IPLDrawingSurface;
begin
  Result := FSurface;
end;

constructor TPLAbstractDrawer.Create(ACanvas: TCanvas);
begin
  inherited Create;

  FCanvas := ACanvas;
end;

procedure TPLAbstractDrawer.DrawObjectBackground(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectBorder(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectDebug(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectFully(const AObject: TPLHTMLObject;
  const ADebug: boolean);
begin
  if not Assigned(AObject) then exit;

  DrawObjectShadow(AObject);
  DrawObjectBackground(AObject);
  DrawObjectBorder(AObject);
  DrawObjectOutline(AObject);
  DrawObjectTextShadow(AObject);
  DrawObjectOutlineText(AObject);
  DrawObjectText(AObject);
  DrawObjectPseudoBefore(AObject);
  DrawObjectPseudoAfter(AObject);

  if ADebug then DrawObjectDebug(AObject);
end;

procedure TPLAbstractDrawer.DrawObjectOutline(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectOutlineText(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectPseudoAfter(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectPseudoBefore(const AObject: TPLHTMLObject
  );
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectShadow(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectText(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawObjectTextShadow(const AObject: TPLHTMLObject);
begin
  if not Assigned(AObject) then exit;
end;

procedure TPLAbstractDrawer.DrawFrame(const ABorders: TPLDrawingBorders;
  const ARect: TPLRectF);
begin
  //
end;

end.

