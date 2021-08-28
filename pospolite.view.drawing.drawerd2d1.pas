unit Pospolite.View.Drawing.DrawerD2D1;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
    Some parts of the code are based on Codebot Pascal Library by Anthony Walter
    https://github.com/sysrpl/Cross.Codebot/tree/master/source
}

{$mode objfpc}{$H+}
{$warnings off}

interface

uses
  Windows, Classes, SysUtils, Graphics, Pospolite.View.Basics, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Drawer, Pospolite.View.Drawing.DrawerD2D1.Definitions,
  math;

type

  { TPLD2D1Drawer }

  TPLD2D1Drawer = class(TPLAbstractDrawer)
  public
    constructor Create(ACanvas: TCanvas); override;
    destructor Destroy; override;

    procedure DrawBox(ARect: TPLRectF; ABackground: IPLDrawingBrush;
      ABorders: TPLDrawingBorders); override;
    procedure DrawText(const AText: TPLString; ARect: TPLRectF;
      AFont: IPLDrawingFont; ADirection: TPLTextDirection); override;
  end;

var
  PLD2D1Locale: TPLString = 'en-us';

function NewDrawingMatrixD2D: IPLDrawingMatrix;
function NewDrawingPenD2D(ABrush: IPLDrawingBrush; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
function NewDrawingPenD2D(AColor: TPLColor; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
function NewDrawingSolidBrushD2D(AColor: TPLColor): IPLDrawingBrushSolid;
function NewDrawingBitmapBrushD2D(ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
function NewDrawingLinearGradientBrushD2D(
  const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
function NewDrawingRadialGradientBrushD2D(
  const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
function NewDrawingFontD2D(AFontData: TPLDrawingFontData): IPLDrawingFont;
function NewDrawingSurfaceD2D(ACanvas: TCanvas): IPLDrawingSurface;
function NewDrawingBitmapD2D(AWidth, AHeight: TPLInt): IPLDrawingBitmap;

implementation

operator := (c: TPLColor) r: TD3DColorValue;
begin
  r.r := c.Red / 255;
  r.g := c.Green / 255;
  r.b := c.Blue / 255;
  r.a := c.Alpha / 255;
end;

operator := (c: TD3DColorValue) r: TPLColor;
begin
  r.Red := round(c.r * 255);
  r.Green := round(c.g * 255);
  r.Blue := round(c.b * 255);
  r.Alpha := round(c.a * 255);
end;

operator := (p: TPLPointF) r: TD2D1Point2F;
begin
  r.x := p.X;
  r.y := p.Y;
end;

operator := (p: TD2D1Point2F) r: TPLPointF; inline;
begin
  r := TPLPointF.Create(p.x, p.y);
end;

operator := (p: TPLRectF) r: TD2D1RectF;
begin
  r.left := p.Left;
  r.top := p.Top;
  r.right := p.Right;
  r.bottom := p.Bottom;
end;

operator := (p: TD2D1RectF) r: TPLRectF; inline;
begin
  r := TPLRectF.Create(p.left, p.top, p.right - p.left, p.bottom - p.top);
end;

operator := (p: TPLRectI) r: TD2D1RectU;
begin
  r.left := p.Left;
  r.top := p.Top;
  r.right := p.Right;
  r.bottom := p.Bottom;
end;

type

  { TPLDrawingMatrixD2D }

  TPLDrawingMatrixD2D = class(TInterfacedObject, IPLDrawingMatrix)
  private
    FMatrix: TD2DMatrix3X2F;
    FChanged: TPLBool;
    FIsIdentity: TPLBool;
  public
    constructor Create(const AMatrix: TD2DMatrix3X2F);

    function Clone: IPLDrawingMatrix;

    procedure Multiply(AMatrix: IPLDrawingMatrix);
    procedure Scale(AX, AY: TPLFloat);
    procedure Translate(AX, AY: TPLFloat);
    function Transform(AP: TPLPointF): TPLPointF;
    procedure Identify;
    procedure Rotate(ARad: TPLFloat);

    property Matrix: TD2DMatrix3x2F read FMatrix;
    property Changed: TPLBool read FChanged;
    property Identity: TPLBool read FIsIdentity;
  end;

  { TPLDrawingPenD2D }

  TPLDrawingPenD2D = class(TInterfacedObject, IPLDrawingPen)
  private
    FTarget: ID2D1RenderTarget;
    FID: TPLInt;
    FFill: ID2D1Brush;
    FStyle: ID2D1StrokeStyle;
    FBrush: IPLDrawingBrush;
    FColor: TPLColor;
    FWidth: TPLFloat;
    FLineStyle: TPLDrawingPenStyle;
    FLineStyleOffset: TPLFloat;
    FLineCap: TPLDrawingPenEndCap;
    FLineJoin: TPLDrawingPenJoinStyle;
    FMiterLimit: TPLFloat;

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
  protected
    function Acquire(ATarget: ID2D1RenderTarget; AID: TPLInt): TPLBool;
    function CurrentBrush: ID2D1Brush;
    function HandleAvailable: TPLBool;
  public
    constructor Create(AB: IPLDrawingBrush; AW: TPLFloat); overload;
    constructor Create(const AC: TPLColor; AW: TPLFloat); overload;

    property Color: TPLColor read GetColor write SetColor;
    property Width: TPLFloat read GetWidth write SetWidth;
    property Brush: IPLDrawingBrush read GetBrush write SetBrush;
    property Style: TPLDrawingPenStyle read GetStyle write SetStyle;
    property EndCap: TPLDrawingPenEndCap read GetEndCap write SetEndCap;
    property JoinStyle: TPLDrawingPenJoinStyle read GetJoinStyle write SetJoinStyle;
    property StyleOffset: TPLFloat read GetStyleOffset write SetStyleOffset;
    property MiterLimit: TPLFloat read GetMiterLimit write SetMiterLimit;
  end;

  { TPLDrawingBrushD2D }

  TPLDrawingBrushD2D = class(TInterfacedObject, IPLDrawingBrush)
  private
    FTarget: ID2D1RenderTarget;
    FID: TPLInt;
    FBrush: ID2D1Brush;
    FMatrix: IPLDrawingMatrix;
    FAlpha: Byte;

    function GetAlpha: Byte;
    function GetMatrix: IPLDrawingMatrix;
    procedure SetAlpha(AValue: Byte);
    procedure SetMatrix(AValue: IPLDrawingMatrix);
  protected
    function Acquire(ATarget: ID2D1RenderTarget; AID: TPLInt): TPLBool;
    function HandleAvailable: TPLBool; virtual;
  public
    constructor Create;

    property Alpha: Byte read GetAlpha write SetAlpha;
    property Matrix: IPLDrawingMatrix read GetMatrix write SetMatrix;
  end;

  { TPLDrawingBrushSolidD2D }

  TPLDrawingBrushSolidD2D = class(TPLDrawingBrushD2D, IPLDrawingBrushSolid)
  private
    FColor: TPLColor;

    function GetColor: TPLColor;
    procedure SetColor(AValue: TPLColor);
  protected
    function HandleAvailable: TPLBool; override;
  public
    constructor Create(AC: TPLColor);

    property Color: TPLColor read GetColor write SetColor;
  end;

  { TPLDrawingGradientStopsD2D }

  TPLDrawingGradientStopsD2D = specialize TPLList<TD2D1GradientStop>;

  { TPLDrawingBrushGradientD2D }

  TPLDrawingBrushGradientD2D = class(TPLDrawingBrushD2D, IPLDrawingBrushGradient)
  private
    FCreated: TPLBool;
    FMidPoint: TPLPointF;
    FWrap: TPLDrawingBrushGradientWrap;
    FStops: TPLDrawingGradientStopsD2D;

    function GetWrap: TPLDrawingBrushGradientWrap;
    procedure SetWrap(AValue: TPLDrawingBrushGradientWrap);
  protected
    function HandleAvailable: TPLBool; override;
  public
    constructor Create(AMiddle: TPLPointF);
    destructor Destroy; override;
    procedure AddStop(AColor: TPLColor; AOffset: TPLFloat);

    property Wrap: TPLDrawingBrushGradientWrap read GetWrap write SetWrap;
  end;

  { TPLDrawingBrushGradientLinearD2D }

  TPLDrawingBrushGradientLinearD2D = class(TPLDrawingBrushGradientD2D, IPLDrawingBrushGradientLinear)
  private
    FA: TPLPointF;
    FB: TPLPointF;
  protected
    function HandleAvailable: TPLBool; override;
  public
    constructor Create(const A, B: TPLPointF);
  end;

  { TPLDrawingBrushGradientRadialD2D }

  TPLDrawingBrushGradientRadialD2D = class(TPLDrawingBrushGradientD2D, IPLDrawingBrushGradientRadial)
  private
    FRect: TPLRectF;
  protected
    function HandleAvailable: TPLBool; override;
  public
    constructor Create(const R: TPLRectF);
  end;

  { TPLDrawingBrushBitmapD2D }

  TPLDrawingBrushBitmapD2D = class(TPLDrawingBrushD2D, IPLDrawingBrushBitmap)
  private
    FBitmap: IPLDrawingBitmap;
  protected
    function HandleAvailable: TPLBool; override;
  public
    constructor Create(B: IPLDrawingBitmap);
  end;

  { TPLDrawingFontD2D }

  TPLDrawingFontD2D = class(TInterfacedObject, IPLDrawingFont)
  private
    FFormat: IDWriteTextFormat;
    FData: TPLDrawingFontData;

    function GetColor: TPLColor;
    function GetDecoration: TPLDrawingFontDecorations;
    function GetName: TPLString;
    function GetQuality: TFontQuality;
    function GetSize: TPLFloat;
    function GetStretch: TPLDrawingFontStretch;
    function GetStyle: TPLDrawingFontStyle;
    function GetWeight: TPLDrawingFontWeight;
    procedure SetColor(AValue: TPLColor);
    procedure SetDecoration(AValue: TPLDrawingFontDecorations);
    procedure SetName(AValue: TPLString);
    procedure SetQuality(AValue: TFontQuality);
    procedure SetSize(AValue: TPLFloat);
    procedure SetStretch(AValue: TPLDrawingFontStretch);
    procedure SetStyle(AValue: TPLDrawingFontStyle);
    procedure SetWeight(AValue: TPLDrawingFontWeight);
    function Format: IDWriteTextFormat;
  public
    constructor Create(AFontData: TPLDrawingFontData);

    property Name: TPLString read GetName write SetName;
    property Color: TPLColor read GetColor write SetColor;
    property Quality: TFontQuality read GetQuality write SetQuality;
    property Size: TPLFloat read GetSize write SetSize;
    property Weight: TPLDrawingFontWeight read GetWeight write SetWeight;
    property Style: TPLDrawingFontStyle read GetStyle write SetStyle;
    property Stretch: TPLDrawingFontStretch read GetStretch write SetStretch;
    property Decoration: TPLDrawingFontDecorations read GetDecoration write SetDecoration;
  end;

  { TPLDrawingPathDataD2D }

  TPLDrawingPathDataD2D = class(TInterfacedObject, IPLDrawingPathData)
  private
    FPath: ID2D1Geometry;
  public
    constructor Create(APath: ID2D1Geometry);
  end;

  TPLDrawingSurfaceD2D = class;
  TPLD2D1GeometryList = specialize TPLList<ID2D1Geometry>;

  { TPLDrawingSurfacePathD2D }

  TPLDrawingSurfacePathD2D = class(TInterfacedObject, IPLDrawingPath)
  private
    FSurface: TPLDrawingSurfaceD2D;
    FClipStack: TPLD2D1GeometryList;
    FClipHeight: Integer;
    FData: TPLD2D1GeometryList;
    FFigure: ID2D1PathGeometry;
    FFigureSink: ID2D1GeometrySink;
    FFigureOrigin: TD2DPoint2f;
    FFigureOpened: Boolean;

    function ClipStack: TPLD2D1GeometryList;
  protected
    function HandleAvailable: TPLBool;
    procedure Open; overload;
    procedure Open(X, Y: TPLFloat); overload;
    procedure SaveClipStack;
    procedure RestoreClipStack;
  public
    constructor Create(AC: TPLDrawingSurfaceD2D);
    destructor Destroy; override;

    function Clone: IPLDrawingPathData;
    procedure Add; overload;
    procedure Add(G: ID2D1Geometry); overload;
    procedure Clip;
    procedure JoinData(APath: IPLDrawingPathData);
    procedure Remove;
    procedure Unclip;
  end;

  { TPLDrawingSurfaceStateD2D }

  TPLDrawingSurfaceStateD2D = class
  public
    ClipStack: TPLD2D1GeometryList;
    Data: TPLD2D1GeometryList;
    Matrix: IPLDrawingMatrix;

    constructor Create(C: TPLDrawingSurfaceD2D);
    destructor Destroy; override;
    procedure Restore(C: TPLDrawingSurfaceD2D);
  end;

  { TPLDrawingSurfaceStateD2DList }

  TPLDrawingSurfaceStateD2DList = specialize TPLObjectList<TPLDrawingSurfaceStateD2D>;

  { TPLDrawingSurfaceD2D }

  TPLDrawingSurfaceD2D = class(TInterfacedObject, IPLDrawingSurface)
  private
    FTarget: ID2D1RenderTarget;
    FID: TPLInt;
    FPath: IPLDrawingPath;
    FMatrix: IPLDrawingMatrix;
    FStateStack: TPLDrawingSurfaceStateD2DList;
    FDrawing: TPLBool;

    function GetHandle: Pointer;
    function GetMatrix: IPLDrawingMatrix;
    function GetPath: IPLDrawingPath;
    procedure SetMatrix(AValue: IPLDrawingMatrix);
    function Path: TPLDrawingSurfacePathD2D;
    function Matrix: TPLDrawingMatrixD2D;
    procedure Draw;
  protected
    procedure AcquireTarget(Target: ID2D1RenderTarget);
    function AcquireBrush(Brush: IPLDrawingBrush; out B: ID2D1Brush): TPLBool;
    function AcquirePen(Pen: IPLDrawingPen; out B: ID2D1Brush; out S: ID2D1StrokeStyle): TPLBool;
    function HandleAvailable: TPLBool; virtual;
    procedure HandleRelease; virtual;
  public
    constructor Create(T: ID2D1RenderTarget);
    destructor Destroy; override;
    procedure ShareRelease; virtual;

    procedure Save;
    procedure Restore;
    procedure Flush; virtual;
    procedure Clear(const AColor: TPLColor);
    procedure MoveTo(const AX, AY: TPLFloat);
    procedure LineTo(const AX, AY: TPLFloat);
    procedure ArcTo(const ARect: TPLRectF; const ABeginAngle, AEndAngle: TPLFloat);
    procedure CurveTo(const AX, AY: TPLFloat; const AC1, AC2: TPLPointF);
    procedure Ellipse(const ARect: TPLRectF);
    procedure FillOrStroke(ABrush: IPLDrawingBrush; APen: IPLDrawingPen; APreserve: TPLBool);
    procedure Stroke(APen: IPLDrawingPen; const APreserve: TPLBool = false);
    procedure Fill(ABrush: IPLDrawingBrush; const APreserve: TPLBool = false);
    function TextSize(AFont: IPLDrawingFont; const AText: string): TPLPointF;
    function TextHeight(AFont: IPLDrawingFont; const AText: string; const AWidth: TPLFloat): TPLFloat;
    procedure TextOut(AFont: IPLDrawingFont; const AText: string; const ARect: TPLRectF;
      const ADirection: TPLTextDirection; const AImmediate: TPLBool = true);

    property Handle: Pointer read GetHandle;
    //property Matrix: IPLDrawingMatrix read GetMatrix write SetMatrix;
    //property Path: IPLDrawingPath read GetPath;
  end;

  { IPLSharedBitmapTargetD2D }

  IPLSharedBitmapTargetD2D = interface
    ['{0959D148-21A8-4A20-9EAB-2503BB6C3A9F}']

    function ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
    procedure ShareRelease;
  end;

  TPLDrawingBitmapD2D = class;

  { TBitmapSurfaceD2D }

  TBitmapSurfaceD2D = class(TPLDrawingSurfaceD2D, IPLSharedBitmapTargetD2D)
  private
    FBitmap: TPLDrawingBitmapD2D;
    FSurfaceBitmap: ID2D1Bitmap;
    FSharedTarget: ID2D1RenderTarget;
    FSharedBitmap: ID2D1Bitmap;
  protected
    function HandleAvailable: TPLBool; override;
    procedure HandleRelease; override;
  public
    constructor Create(B: TPLDrawingBitmapD2D);
    function ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
    procedure ShareRelease; override;
  end;

  { TPLDrawingBitmapD2D }

  TPLDrawingBitmapD2D = class(TPLDrawingInterfacedBitmap)
  protected
    procedure HandleRelease; override;
    function GetSurface: IPLDrawingSurface; override;
    function GetPixels: PPLPixel; override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  end;

{ Matrix math }

const
  MatrixIdentity: TD2DMatrix3X2F = (
    m11: 1; m12: 0; m21: 0; m22: 1; m31: 0; m32: 0);

function MatrixMultiply(const A, B: TD2DMatrix3X2F): TD2DMatrix3X2F;
begin
  with Result do
  begin
    m11 := A.m11 * B.m11 + A.m12 * B.m21;
    m12 := A.m11 * B.m12 + A.m12 * B.m22;
    m21 := A.m21 * B.m11 + A.m22 * B.m21;
    m22 := A.m21 * B.m12 + A.m22 * B.m22;
    m31 := A.m31 * B.m11 + A.m32 * B.m21 + B.m31;
    m32 := A.m31 * B.m12 + A.m32 * B.m22 + B.m32;
  end;
end;

procedure MatrixTranslate(var M: TD2DMatrix3X2F; X, Y: TPLFloat);
var
  T: TD2DMatrix3X2F;
begin
  with T do
  begin
    m11 := 1;
    m12 := 0;
    m21 := 0;
    m22 := 1;
    m31 := X;
    m32 := Y;
  end;
  M := MatrixMultiply(M, T);
end;

procedure MatrixRotate(var M: TD2DMatrix3X2F; A: single);
var
  C: TD2D1Point2F;
  R: TD2DMatrix3X2F;
begin
  C.x := 0;
  C.y := 0;
  D2D1MakeRotateMatrix(A / Pi * 180, C, @R);
  M := MatrixMultiply(M, R);
end;

procedure MatrixScale(var M: TD2DMatrix3X2F; X, Y: TPLFloat);
var
  S: TD2DMatrix3X2F;
begin
  with S do
  begin
    m11 := X;
    m12 := 0;
    m21 := 0;
    m22 := Y;
    m31 := 0;
    m32 := 0;
  end;
  M := MatrixMultiply(M, S);
end;

{ Direct2D helper routines }

var
  RenderFactoryInstance: ID2D1Factory;
  WriteFactoryInstance: IDWriteFactory;

function RenderFactory: ID2D1Factory;
begin
  if RenderFactoryInstance = nil then
    D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, IID_ID2D1Factory, nil,
      RenderFactoryInstance);
  Result := RenderFactoryInstance;
end;

function WriteFactory: IDWriteFactory;
begin
  if WriteFactoryInstance = nil then
    DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory,
      WriteFactoryInstance);
  Result := WriteFactoryInstance;
end;

{ RenderFactory object creation routines }

function DefaultPixelFormat: TD2D1PixelFormat;
begin
  Result.format := DXGI_FORMAT_B8G8R8A8_UNORM;
  Result.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
end;

function DefaultRenderTargetProperties: TD2D1RenderTargetProperties;
begin
  Result._type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
  Result.pixelFormat := DefaultPixelFormat;
  Result.dpiX := 0;
  Result.dpiY := 0;
  { TODO: Review performance of D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE }
  Result.usage := D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE;
  Result.minLevel := D2D1_FEATURE_LEVEL_DEFAULT;
end;

function CreateDCTarget: ID2D1DCRenderTarget;
var
  Prop: TD2D1RenderTargetProperties;
begin
  Prop := DefaultRenderTargetProperties;
  RenderFactory.CreateDCRenderTarget(Prop, Result);
end;

function CreateWndTarget(Wnd: HWND; out Rect: TRect): ID2D1HwndRenderTarget;
var
  Prop: TD2D1HwndRenderTargetProperties;
begin
  Rect := TRect.Empty;
  Result := nil;
  if not IsWindow(Wnd) then
    Exit;
  GetClientRect(Wnd, Rect);
  Rect.Right := Rect.Right;
  Rect.Bottom := Rect.Bottom;
  Prop.hwnd := Wnd;
  Prop.pixelSize.Width := Rect.Right - Rect.Left;
  Prop.pixelSize.Height := Rect.Bottom - Rect.Top;
  Prop.presentOptions := D2D1_PRESENT_OPTIONS_NONE;
  RenderFactory.CreateHwndRenderTarget(DefaultRenderTargetProperties, Prop, Result);
end;

function CreateBitmap(Target: ID2D1RenderTarget; Width, Height: integer;
  Bits: Pointer = nil): ID2D1Bitmap;
var
  Size: TD2DSizeU;
  Stride: cardinal;
  Prop: TD2D1BitmapProperties;
begin
  Result := nil;
  if (Width < 1) or (Height < 1) then
    exit;

  Size.Width := Width;
  Size.Height := Height;
  Stride := Width * SizeOf(TPLColor);
  Prop.pixelFormat := DefaultPixelFormat;
  Prop.dpiX := 0;
  Prop.dpiY := 0;
  Target.CreateBitmap(Size, Bits, Stride, Prop, Result);
end;

function CreateSharedBitmap(Target: ID2D1RenderTarget; Bitmap: ID2D1Bitmap): ID2D1Bitmap;
var
  Prop: TD2D1BitmapProperties;
begin
  Prop.pixelFormat := DefaultPixelFormat;
  Prop.dpiX := 0;
  Prop.dpiY := 0;
  Target.CreateSharedBitmap(ID2D1Bitmap, Pointer(Bitmap), @Prop, Result);
end;

function CreatePenStyle(Pen: IPLDrawingPen): ID2D1StrokeStyle;
const
  Caps: array[TPLDrawingPenEndCap] of D2D1_CAP_STYLE =
    (D2D1_CAP_STYLE_FLAT, D2D1_CAP_STYLE_SQUARE, D2D1_CAP_STYLE_ROUND,
    D2D1_CAP_STYLE_TRIANGLE);
  Joins: array[TPLDrawingPenJoinStyle] of D2D1_LINE_JOIN =
    (D2D1_LINE_JOIN_MITER, D2D1_LINE_JOIN_BEVEL, D2D1_LINE_JOIN_ROUND);
  Dashes: array[TPLDrawingPenStyle] of D2D1_DASH_STYLE =
    (D2D1_DASH_STYLE_SOLID, D2D1_DASH_STYLE_DASH,
    D2D1_DASH_STYLE_DOT, D2D1_DASH_STYLE_DASH_DOT, D2D1_DASH_STYLE_DASH_DOT_DOT,
    D2D1_DASH_STYLE_CUSTOM);
var
  Prop: TD2D1StrokeStyleProperties;
begin
  Prop.startCap := Caps[Pen.EndCap];
  Prop.endCap := Prop.startCap;
  if (Pen.Style = dpsDot) and (Pen.EndCap = dpecFlat) then
    Prop.dashCap := D2D1_CAP_STYLE_SQUARE
  else
    Prop.dashCap := Prop.startCap;
  Prop.lineJoin := Joins[Pen.JoinStyle];
  Prop.miterLimit := Pen.MiterLimit;
  Prop.dashStyle := Dashes[Pen.Style];
  Prop.dashOffset := Pen.StyleOffset;
  RenderFactory.CreateStrokeStyle(Prop, nil, 0, Result);
end;

function CreateSolidBrush(Target: ID2D1RenderTarget; const C: TPLColor;
  Opacity: byte): ID2D1SolidColorBrush;
var
  Prop: TD2D1BrushProperties;
begin
  Prop.opacity := Opacity / $FF;
  Prop.transform := MatrixIdentity;
  Target.CreateSolidColorBrush(C, @Prop, Result);
end;

function CreateLinearGradientBrush(Target: ID2D1RenderTarget;
  const A, B: TPLPointF; Opacity: byte; const Stops: TPLDrawingGradientStopsD2D;
  Wrap: TPLDrawingBrushGradientWrap): ID2D1LinearGradientBrush;
const
  Wraps: array[TPLDrawingBrushGradientWrap] of TD2D1ExtendMode =
    (D2D1_EXTEND_MODE_CLAMP, D2D1_EXTEND_MODE_WRAP, D2D1_EXTEND_MODE_MIRROR);
  EmptyStops: TD2D1GradientStop = ();
var
  LineProp: TD2D1LinearGradientBrushProperties;
  BrushProp: TD2D1BrushProperties;
  Collection: ID2D1GradientStopCollection;
  Gamma: TD2D1Gamma;
  S: Pointer;
  I: integer;
begin
  LineProp.startPoint := A;
  LineProp.endPoint := B;
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  if Stops.Empty then
  begin
    S := @EmptyStops;
    I := 1;
  end
  else
  begin
    S := Stops.Data;
    I := Stops.Count;
  end;
  Gamma := D2D1_GAMMA_2_2;
  //WriteLn(I);
  Target.CreateGradientStopCollection(S, I, Gamma, Wraps[Wrap], Collection);
  Target.CreateLinearGradientBrush(LineProp, @BrushProp, Collection, Result);
end;

function CreateRadialGradientBrush(Target: ID2D1RenderTarget;
  const Rect: TPLRectF; Opacity: byte; const Stops: TPLDrawingGradientStopsD2D;
  Wrap: TPLDrawingBrushGradientWrap): ID2D1RadialGradientBrush;
const
  Wraps: array[TPLDrawingBrushGradientWrap] of TD2D1ExtendMode =
    (D2D1_EXTEND_MODE_CLAMP, D2D1_EXTEND_MODE_WRAP, D2D1_EXTEND_MODE_MIRROR);
  EmptyStops: TD2D1GradientStop = ();
  Offset: TD2D1Point2F = ();
var
  RadProp: TD2D1RadialGradientBrushProperties;
  BrushProp: TD2D1BrushProperties;
  Collection: ID2D1GradientStopCollection;
  Gamma: TD2D1Gamma;
  S: Pointer;
  I: integer;
begin
  RadProp.center := Rect.Middle;
  RadProp.gradientOriginOffset := Offset;
  RadProp.radiusX := Rect.Width / 2;
  RadProp.radiusY := Rect.Height / 2;
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  if Stops.Empty then
  begin
    S := @EmptyStops;
    I := 1;
  end
  else
  begin
    S := Stops.Data;
    I := Stops.Count;
  end;
  Gamma := D2D1_GAMMA_2_2;
  Target.CreateGradientStopCollection(S, I, Gamma, Wraps[Wrap], Collection);
  Target.CreateRadialGradientBrush(RadProp, @BrushProp, Collection, Result);
end;

function CreateBitmapBrush(Target: ID2D1RenderTarget; Bitmap: IPLDrawingBitmap;
  Opacity: byte): ID2D1BitmapBrush;
var
  BitTarget: ID2D1Bitmap;
  BitProp: TD2D1BitmapBrushProperties;
  BrushProp: TD2D1BrushProperties;
begin
  if Bitmap.Empty then
    Exit(nil);
  BitTarget := CreateBitmap(Target, Bitmap.Width, Bitmap.Height, Bitmap.Pixels);
  BitProp.extendModeX := D2D1_EXTEND_MODE_WRAP;
  BitProp.extendModeY := D2D1_EXTEND_MODE_WRAP;
  BitProp.interpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_LINEAR;
  BrushProp.opacity := Opacity / $FF;
  BrushProp.transform := MatrixIdentity;
  Target.CreateBitmapBrush(BitTarget, @BitProp, @BrushProp, Result);
end;

function CreateFigure: ID2D1PathGeometry;
begin
  RenderFactory.CreatePathGeometry(Result);
end;

function CreateEllispe(const Rect: TPLRectF): ID2D1EllipseGeometry;
var
  E: TD2D1Ellipse;
begin
  E.point := Rect.Middle;
  E.radiusX := Rect.Width / 2;
  E.radiusY := Rect.Height / 2;
  RenderFactory.CreateEllipseGeometry(E, Result);
end;

function CreateRectangle(const Rect: TPLRectF): ID2D1RectangleGeometry;
begin
  RenderFactory.CreateRectangleGeometry(Rect, Result);
end;

function CreateRoundRectangle(const Rect: TPLRectF;
  Radius: TPLFloat): ID2D1RoundedRectangleGeometry;
var
  R: TD2D1RoundedRect;
begin
  R.rect := Rect;
  if Rect.Width < Radius * 2 then
    Radius := Rect.Width / 2;
  if Rect.Height < Radius * 2 then
    Radius := Rect.Height / 2;
  R.radiusX := Radius;
  R.radiusY := Radius;
  RenderFactory.CreateRoundedRectangleGeometry(R, Result);
end;

function CreateTransformed(G: ID2D1Geometry; Matrix: IPLDrawingMatrix): ID2D1Geometry;
var
  M: TPLDrawingMatrixD2D;
  T: ID2D1TransformedGeometry;
begin
  if Matrix is TPLDrawingMatrixD2D then
  begin
    M := Matrix as TPLDrawingMatrixD2D;
    if TPLDrawingMatrixD2D(M).Identity then
      Result := G
    else
    begin
      RenderFactory.CreateTransformedGeometry(G, TPLDrawingMatrixD2D(M).Matrix, T);
      Result := T;
    end;
  end
  else
    Result := G;
end;

function CreateGroup(G: PID2D1Geometry; Count: integer): ID2D1GeometryGroup;
begin
  if Count < 1 then
    Count := 0;
  RenderFactory.CreateGeometryGroup(D2D1_FILL_MODE_WINDING, G, Count, Result);
end;

function CreateLayerParameters(G: ID2D1Geometry): TD2D1LayerParameters;
const
  InfiniteRect: TD2D1RectF =
    (left: -$400000; top: -$400000;
    right: $800000; bottom: $800000);
begin
  with Result do
  begin
    contentBounds := InfiniteRect;
    geometricMask := G;
    maskAntialiasMode := D2D1_ANTIALIAS_MODE_PER_PRIMITIVE;
    maskTransform := MatrixIdentity;
    opacity := 1;
    opacityBrush := nil;
    layerOptions := D2D1_LAYER_OPTIONS_NONE;
  end;
end;

{ WriteFactory object creation routines }

function CreateTextFormat(Font: IPLDrawingFont): IDWriteTextFormat;
const
  Factor = 72 / 96;
  Weight: array[TPLDrawingFontWeight] of DWRITE_FONT_WEIGHT =
    (DWRITE_FONT_WEIGHT_THIN, DWRITE_FONT_WEIGHT_EXTRA_LIGHT, DWRITE_FONT_WEIGHT_LIGHT,
    DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_WEIGHT_MEDIUM, DWRITE_FONT_WEIGHT_SEMI_BOLD,
    DWRITE_FONT_WEIGHT_BOLD, DWRITE_FONT_WEIGHT_EXTRA_BOLD, DWRITE_FONT_WEIGHT_BLACK
    );
  Style: array[TPLDrawingFontStyle] of DWRITE_FONT_STYLE =
    (DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STYLE_ITALIC, DWRITE_FONT_STYLE_OBLIQUE);
  Stretch: array[TPLDrawingFontStretch] of DWRITE_FONT_STRETCH =
    (DWRITE_FONT_STRETCH_ULTRA_CONDENSED, DWRITE_FONT_STRETCH_EXTRA_CONDENSED,
    DWRITE_FONT_STRETCH_CONDENSED, DWRITE_FONT_STRETCH_SEMI_CONDENSED,
    DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STRETCH_SEMI_EXPANDED,
    DWRITE_FONT_STRETCH_EXPANDED, DWRITE_FONT_STRETCH_EXTRA_EXPANDED,
    DWRITE_FONT_STRETCH_ULTRA_EXPANDED
    );
var
  Size: TPLFloat;
  Name: WideString;
begin
  Size := Font.Size * Factor;
  Name := Font.Name;
  if WriteFactory.CreateTextFormat(PWideChar(Name), nil, Weight[Font.Weight],
    Style[Font.Style], Stretch[Font.Stretch], Size, PWideChar(PLD2D1Locale),
    Result) <> S_OK then
    Result := nil;
end;

function CreateTextLayout(Format: IDWriteTextFormat; const Text: string;
  Width, Height: TPLFloat): IDWriteTextLayout;
var
  S: WideString;
begin
  if Format = nil then
    Result := nil;
  S := Text;
  if WriteFactory.CreateTextLayout(PWideChar(S), Length(S), Format,
    Width, Height, Result) <> S_OK then
    Result := nil;
end;

function CreateGdiTextLayout(Format: IDWriteTextFormat; const Text: string;
  Width, Height: TPLFloat): IDWriteTextLayout;
var
  S: WideString;
begin
  if Format = nil then
    Result := nil;
  S := Text;
  if WriteFactory.CreateGdiCompatibleTextLayout(PWideChar(S), Length(S),
    Format, Width, Height, 1, nil, True, Result) <> S_OK then
    Result := nil;
end;


{ TPLDrawingMatrixD2D }

constructor TPLDrawingMatrixD2D.Create(const AMatrix: TD2DMatrix3X2F);
begin
  FMatrix := AMatrix;
  FChanged := True;
  FIsIdentity := CompareMem(@FMatrix, @MatrixIdentity, SizeOf(FMatrix));
end;

function TPLDrawingMatrixD2D.Clone: IPLDrawingMatrix;
begin
  Result := TPLDrawingMatrixD2D.Create(FMatrix);
end;

procedure TPLDrawingMatrixD2D.Multiply(AMatrix: IPLDrawingMatrix);
begin
  if AMatrix is TPLDrawingMatrixD2D then
    FMatrix := MatrixMultiply(FMatrix, (AMatrix as TPLDrawingMatrixD2D).FMatrix);
  FChanged := True;
  FIsIdentity := False;
end;

procedure TPLDrawingMatrixD2D.Scale(AX, AY: TPLFloat);
begin
  MatrixScale(FMatrix, AX, AY);
  FChanged := True;
  FIsIdentity := False;
end;

procedure TPLDrawingMatrixD2D.Translate(AX, AY: TPLFloat);
begin
  MatrixTranslate(FMatrix, AX, AY);
  FChanged := True;
  FIsIdentity := False;
end;

function TPLDrawingMatrixD2D.Transform(AP: TPLPointF): TPLPointF;
begin
  Result.X := FMatrix.m11 * AP.X + FMatrix.m12 * AP.Y + FMatrix.m31;
  Result.Y := FMatrix.m21 * AP.X + FMatrix.m22 * AP.Y + FMatrix.m32;
end;

procedure TPLDrawingMatrixD2D.Identify;
begin
  FMatrix := MatrixIdentity;
  FChanged := True;
  FIsIdentity := True;
end;

procedure TPLDrawingMatrixD2D.Rotate(ARad: TPLFloat);
begin
  MatrixRotate(FMatrix, ARad);
  FChanged := True;
  FIsIdentity := False;
end;

{ TPLDrawingPenD2D }

function TPLDrawingPenD2D.GetBrush: IPLDrawingBrush;
begin
  Result := FBrush;
end;

function TPLDrawingPenD2D.GetColor: TPLColor;
begin
  Result := FColor;
end;

function TPLDrawingPenD2D.GetEndCap: TPLDrawingPenEndCap;
begin
  Result := FLineCap;
end;

function TPLDrawingPenD2D.GetJoinStyle: TPLDrawingPenJoinStyle;
begin
  Result := FLineJoin;
end;

function TPLDrawingPenD2D.GetMiterLimit: TPLFloat;
begin
  Result := FMiterLimit;
end;

function TPLDrawingPenD2D.GetStyle: TPLDrawingPenStyle;
begin
  Result := FLineStyle;
end;

function TPLDrawingPenD2D.GetStyleOffset: TPLFloat;
begin
  Result := FLineStyleOffset;
end;

function TPLDrawingPenD2D.GetWidth: TPLFloat;
begin
  Result := FWidth;
end;

procedure TPLDrawingPenD2D.SetBrush(AValue: IPLDrawingBrush);
begin
  FBrush := AValue;
end;

procedure TPLDrawingPenD2D.SetColor(AValue: TPLColor);
begin
  FColor := AValue;
end;

procedure TPLDrawingPenD2D.SetEndCap(AValue: TPLDrawingPenEndCap);
begin
  if AValue = FLineCap then exit;

  FLineCap := AValue;
  FStyle := nil;
end;

procedure TPLDrawingPenD2D.SetJoinStyle(AValue: TPLDrawingPenJoinStyle);
begin
  if AValue = FLineJoin then exit;

  FLineJoin := AValue;
  FStyle := nil;
end;

procedure TPLDrawingPenD2D.SetMiterLimit(AValue: TPLFloat);
begin
  if AValue = FMiterLimit then exit;

  FMiterLimit := AValue;
  FStyle := nil;
end;

procedure TPLDrawingPenD2D.SetStyle(AValue: TPLDrawingPenStyle);
begin
  if AValue = FLineStyle then exit;

  FLineStyle := AValue;
  FStyle := nil;
end;

procedure TPLDrawingPenD2D.SetStyleOffset(AValue: TPLFloat);
begin
  if AValue = FLineStyleOffset then exit;

  FLineStyleOffset := AValue;
  FStyle := nil;
end;

procedure TPLDrawingPenD2D.SetWidth(AValue: TPLFloat);
begin
  FWidth := AValue;
end;

function TPLDrawingPenD2D.Acquire(ATarget: ID2D1RenderTarget; AID: TPLInt
  ): TPLBool;
begin
  if ATarget = nil then
    exit(False);

  Result := True;

  if FBrush is TPLDrawingBrushD2D then
    Result := (FBrush as TPLDrawingBrushD2D).Acquire(ATarget, AID);

  if not Result then
    exit;

  if AID <> FID then
    FFill := nil;

  FTarget := ATarget;
  FID := AID;
  Result := HandleAvailable;
end;

function TPLDrawingPenD2D.CurrentBrush: ID2D1Brush;
begin
  if FFill <> nil then
    Result := FFill
  else if FBrush is TPLDrawingBrushD2D then
    Result := (FBrush as TPLDrawingBrushD2D).FBrush
  else
    Result := nil;
end;

function TPLDrawingPenD2D.HandleAvailable: TPLBool;
begin
  if FStyle = nil then
    FStyle := CreatePenStyle(Self);

  Result := False;

  if FBrush is TPLDrawingBrushD2D then
    Result := (FBrush as TPLDrawingBrushD2D).HandleAvailable;

  if Result then
    FFill := nil
  else if FFill = nil then
    FFill := CreateSolidBrush(FTarget, Color, $FF);

  Result := CurrentBrush <> nil;
end;

constructor TPLDrawingPenD2D.Create(AB: IPLDrawingBrush; AW: TPLFloat);
begin
  inherited Create;

  FBrush := AB;
  FWidth := AW;
  FColor := '#000';
  FMiterLimit := 10;
end;

constructor TPLDrawingPenD2D.Create(const AC: TPLColor; AW: TPLFloat);
begin
  inherited Create;

  FColor := AC;
  FWidth := AW;
  FMiterLimit := 10;
end;

{ TPLDrawingBrushD2D }

function TPLDrawingBrushD2D.GetAlpha: Byte;
begin
  Result := FAlpha;
end;

function TPLDrawingBrushD2D.GetMatrix: IPLDrawingMatrix;
begin
  if FMatrix = nil then
    FMatrix := NewDrawingMatrixD2D;

  Result := FMatrix;
end;

procedure TPLDrawingBrushD2D.SetAlpha(AValue: Byte);
begin
  FAlpha := AValue;

  if FBrush <> nil then
    FBrush.SetOpacity(FAlpha / 255);
end;

procedure TPLDrawingBrushD2D.SetMatrix(AValue: IPLDrawingMatrix);
begin
  if AValue is TPLDrawingMatrixD2D then
  begin
    (Matrix as TPLDrawingMatrixD2D).FMatrix := (AValue as TPLDrawingMatrixD2D).FMatrix;
    (Matrix as TPLDrawingMatrixD2D).FChanged := True;
  end else
    Matrix.Identify;
end;

function TPLDrawingBrushD2D.Acquire(ATarget: ID2D1RenderTarget; AID: TPLInt
  ): TPLBool;
begin
  if ATarget = nil then
    exit(False);

  if AID <> FID then
  begin
    FBrush := nil;
    if FMatrix is TPLDrawingMatrixD2D then
      (FMatrix as TPLDrawingMatrixD2D).FChanged := True;
  end;

  FTarget := ATarget;
  FID := AID;
  Result := HandleAvailable;
end;

function TPLDrawingBrushD2D.HandleAvailable: TPLBool;
var
  M: IPLDrawingMatrix;
begin
  Result := FBrush <> nil;

  if not Result then exit;

  M := nil;
  if (FMatrix is TPLDrawingMatrixD2D) and (FMatrix as TPLDrawingMatrixD2D).FChanged then
  begin
    M := FMatrix;
    (FMatrix as TPLDrawingMatrixD2D).FChanged := False;
  end;

  if M <> nil then
    FBrush.SetTransform((M as TPLDrawingMatrixD2D).FMatrix);
end;

constructor TPLDrawingBrushD2D.Create;
begin
  inherited Create;

  FAlpha := 255;
end;

{ TPLDrawingBrushSolidD2D }

procedure TPLDrawingBrushSolidD2D.SetColor(AValue: TPLColor);
begin
  FColor := AValue;
  FBrush := nil;
end;

function TPLDrawingBrushSolidD2D.HandleAvailable: TPLBool;
begin
  if FTarget = nil then
    exit(False);

  if FBrush = nil then
    FBrush := CreateSolidBrush(FTarget, Color, Alpha);

  Result := inherited HandleAvailable;
end;

function TPLDrawingBrushSolidD2D.GetColor: TPLColor;
begin
  Result := FColor;
end;

constructor TPLDrawingBrushSolidD2D.Create(AC: TPLColor);
begin
  inherited Create;

  FColor := AC;
end;

{ TPLDrawingBrushGradientD2D }

function TPLDrawingBrushGradientD2D.GetWrap: TPLDrawingBrushGradientWrap;
begin
  Result := FWrap;
end;

procedure TPLDrawingBrushGradientD2D.SetWrap(AValue: TPLDrawingBrushGradientWrap
  );
begin
  if AValue = FWrap then exit;

  FWrap := AValue;
  FBrush := nil;
end;

function TPLDrawingBrushGradientD2D.HandleAvailable: TPLBool;
var
  M: IPLDrawingMatrix;
begin
  Result := FBrush <> nil;

  if not Result then exit;

  M  := nil;
  if (FMatrix is TPLDrawingMatrixD2D) and (FMatrix as TPLDrawingMatrixD2D).FChanged then
  begin
    (FMatrix as TPLDrawingMatrixD2D).FChanged := False;
    M := FMatrix.Clone;
    FCreated := True;
  end
  else if FCreated then
    M := NewDrawingMatrixD2D;

  if FCreated then
  begin
    M.Translate(FMidPoint.X, FMidPoint.Y);
    FBrush.SetTransform((M as TPLDrawingMatrixD2D).FMatrix);
    FCreated := False;
  end;
end;

constructor TPLDrawingBrushGradientD2D.Create(AMiddle: TPLPointF);
begin
  inherited Create;

  FCreated := True;
  FMidPoint := AMiddle;
  FWrap := dbgwWrap;
  FStops := TPLDrawingGradientStopsD2D.Create;
end;

destructor TPLDrawingBrushGradientD2D.Destroy;
begin
  FStops.Free;

  inherited Destroy;
end;

procedure TPLDrawingBrushGradientD2D.AddStop(AColor: TPLColor; AOffset: TPLFloat
  );
var
  S: TD2D1GradientStop;
begin
  FBrush := nil;
  S.color := AColor;
  S.position := AOffset;
  FStops.Add(S);
end;

{ TPLDrawingBrushGradientLinearD2D }

function TPLDrawingBrushGradientLinearD2D.HandleAvailable: TPLBool;
begin
  if FTarget = nil then
    exit(False);

  FCreated := FBrush = nil;

  if FCreated then
    FBrush := CreateLinearGradientBrush(FTarget, FA, FB, FAlpha, FStops, FWrap);

  Result := inherited HandleAvailable;
end;

constructor TPLDrawingBrushGradientLinearD2D.Create(const A, B: TPLPointF);
var
  M: TPLPointF;
begin
  M := TPLPointF.Create((A.X + B.X) / 2, (A.Y + B.Y) / 2);

  inherited Create(M);

  FA := A - M;
  FB := B - M;
end;

{ TPLDrawingBrushGradientRadialD2D }

function TPLDrawingBrushGradientRadialD2D.HandleAvailable: TPLBool;
begin
  if FTarget = nil then
    exit(False);

  FCreated := FBrush = nil;

  if FCreated then
    FBrush := CreateRadialGradientBrush(FTarget, FRect, FAlpha, FStops, FWrap);

  Result := inherited HandleAvailable;
end;

constructor TPLDrawingBrushGradientRadialD2D.Create(const R: TPLRectF);
begin
  inherited Create(R.Middle);

  FRect := R;
  FRect := FRect ** TPLPointF.Create(0, 0);
end;

{ TPLDrawingBrushBitmapD2D }

function TPLDrawingBrushBitmapD2D.HandleAvailable: TPLBool;
begin
  if FTarget = nil then
    exit(False);

  if FBrush = nil then
    FBrush := CreateBitmapBrush(FTarget, FBitmap, FAlpha);

  Result := inherited HandleAvailable;
end;

constructor TPLDrawingBrushBitmapD2D.Create(B: IPLDrawingBitmap);
begin
  inherited Create;

  FBitmap := B.Clone;
end;

{ TPLDrawingFontD2D }

function TPLDrawingFontD2D.GetColor: TPLColor;
begin
  Result := FData.Color;
end;

function TPLDrawingFontD2D.GetDecoration: TPLDrawingFontDecorations;
begin
  Result := FData.Decoration;
end;

function TPLDrawingFontD2D.GetName: TPLString;
begin
  Result := FData.Name;
end;

function TPLDrawingFontD2D.GetQuality: TFontQuality;
begin
  Result := FData.Quality;
end;

function TPLDrawingFontD2D.GetSize: TPLFloat;
begin
  Result := FData.Size;
end;

function TPLDrawingFontD2D.GetStretch: TPLDrawingFontStretch;
begin
  Result := FData.Stretch;
end;

function TPLDrawingFontD2D.GetStyle: TPLDrawingFontStyle;
begin
  Result := FData.Style;
end;

function TPLDrawingFontD2D.GetWeight: TPLDrawingFontWeight;
begin
  Result := FData.Weight;
end;

procedure TPLDrawingFontD2D.SetColor(AValue: TPLColor);
begin
  FData.Color := AValue;
end;

procedure TPLDrawingFontD2D.SetDecoration(AValue: TPLDrawingFontDecorations);
begin
  FData.Decoration := AValue;
end;

procedure TPLDrawingFontD2D.SetName(AValue: TPLString);
begin
  FData.Name := AValue;
end;

procedure TPLDrawingFontD2D.SetQuality(AValue: TFontQuality);
begin
  FData.Quality := AValue;
end;

procedure TPLDrawingFontD2D.SetSize(AValue: TPLFloat);
begin
  if (AValue < 1) or (AValue = FData.Size) then exit;

  FData.Size := AValue;
  FFormat := nil;
end;

procedure TPLDrawingFontD2D.SetStretch(AValue: TPLDrawingFontStretch);
begin
  FData.Stretch := AValue;
end;

procedure TPLDrawingFontD2D.SetStyle(AValue: TPLDrawingFontStyle);
begin
  FData.Style := AValue;
end;

procedure TPLDrawingFontD2D.SetWeight(AValue: TPLDrawingFontWeight);
begin
  FData.Weight := AValue;
end;

function TPLDrawingFontD2D.Format: IDWriteTextFormat;
begin
  if FFormat = nil then
    FFormat := CreateTextFormat(Self);

  Result := FFormat;
end;

constructor TPLDrawingFontD2D.Create(AFontData: TPLDrawingFontData);
begin
  inherited Create;

  FData := AFontData;
end;

{ TPLDrawingPathDataD2D }

constructor TPLDrawingPathDataD2D.Create(APath: ID2D1Geometry);
begin
  inherited Create;

  FPath := APath;
end;

{ TPLDrawingSurfacePathD2D }

function TPLDrawingSurfacePathD2D.ClipStack: TPLD2D1GeometryList;
begin
  if not Assigned(FClipStack) then FClipStack := TPLD2D1GeometryList.Create;

  Result := FClipStack;
end;

function TPLDrawingSurfacePathD2D.HandleAvailable: TPLBool;
begin
  Result := (FSurface <> nil) and (FSurface.FTarget <> nil);
end;

procedure TPLDrawingSurfacePathD2D.Open;
begin
  if not HandleAvailable then exit;

  if not FFigureOpened then
    Open(FFigureOrigin.x, FFigureOrigin.y);
end;

procedure TPLDrawingSurfacePathD2D.Open(X, Y: TPLFloat);
begin
  if not HandleAvailable then exit;

  if FFigure = nil then begin
    FFigure := CreateFigure;
    FFigure.Open(FFigureSink);
  end else if FFigureOpened then
    FFigureSink.EndFigure(D2D1_FIGURE_END_OPEN);

  FFigureOrigin.x := X;
  FFigureOrigin.y := Y;
  FFigureSink.BeginFigure(FFigureOrigin, D2D1_FIGURE_BEGIN_FILLED);
  FFigureOpened := True;
end;

procedure TPLDrawingSurfacePathD2D.SaveClipStack;
begin
  if not HandleAvailable then exit;

  FSurface.Draw;
  while FClipHeight > 0 do
  begin
    FSurface.FTarget.PopLayer;
    Dec(FClipHeight);
  end;
end;

procedure TPLDrawingSurfacePathD2D.RestoreClipStack;
var
  Params: TD2D1LayerParameters;
  G: ID2D1Geometry;
  L: ID2D1Layer;
  i: SizeInt;
begin
  if not HandleAvailable then exit;

  FSurface.Draw;

  if (FClipHeight > 0) or (FClipStack = nil) then exit;

  for i := 0 to FClipStack.Count-1 do
  begin
    G := FClipStack[i];
    FSurface.FTarget.CreateLayer(nil, L);
    Params := CreateLayerParameters(G);
    FSurface.FTarget.PushLayer(Params, L);
    Inc(FClipHeight);
  end;
end;

constructor TPLDrawingSurfacePathD2D.Create(AC: TPLDrawingSurfaceD2D);
begin
  inherited Create;

  FSurface := AC;
  FData := TPLD2D1GeometryList.Create;
end;

destructor TPLDrawingSurfacePathD2D.Destroy;
begin
  FData.Free;

  inherited Destroy;
end;

function TPLDrawingSurfacePathD2D.Clone: IPLDrawingPathData;
var
  G: ID2D1Geometry;
  I: SizeInt;
begin
  Add;
  I := FData.Count;

  if I = 0 then
    G := nil
  else if I = 1 then
    G := FData.First
  else
    G := CreateGroup(FData.Data, I);

  Result := TPLDrawingPathDataD2D.Create(G);
end;

procedure TPLDrawingSurfacePathD2D.Add;
begin
  if not HandleAvailable then exit;

  if FFigure <> nil then begin
    if FFigureOpened then
      FFigureSink.EndFigure(D2D1_FIGURE_END_OPEN);

    FFigureSink.Close;
    FData.Add(CreateTransformed(FFigure, FSurface.Matrix));
    FFigure := nil;
    FFigureSink := nil;
    FFigureOpened := False;
  end;
end;

procedure TPLDrawingSurfacePathD2D.Add(G: ID2D1Geometry);
begin
  if not HandleAvailable then exit;

  Add;
  FData.Add(CreateTransformed(G, FSurface.Matrix));
end;

procedure TPLDrawingSurfacePathD2D.Clip;
var
  Params: TD2D1LayerParameters;
  P: IPLDrawingPathData;
  G: ID2D1Geometry;
  L: ID2D1Layer;
begin
  if not HandleAvailable then exit;

  FSurface.Draw;
  Add;
  P := Clone;
  G := (P as TPLDrawingPathDataD2D).FPath;

  if G = nil then exit;

  FSurface.FTarget.CreateLayer(nil, L);
  Params := CreateLayerParameters(G);
  FSurface.FTarget.PushLayer(Params, L);
  ClipStack.Add(G);
  Inc(FClipHeight);
  Remove;
end;

procedure TPLDrawingSurfacePathD2D.JoinData(APath: IPLDrawingPathData);
var
  G: ID2D1Geometry;
begin
  G := (APath as TPLDrawingPathDataD2D).FPath;

  if G <> nil then Add(G);
end;

procedure TPLDrawingSurfacePathD2D.Remove;
begin
  if not HandleAvailable then exit;

  Add;
  if Assigned(FData) then FData.Free;
  FData := TPLD2D1GeometryList.Create;
end;

procedure TPLDrawingSurfacePathD2D.Unclip;
begin
  if not HandleAvailable then exit;

  SaveClipStack;
  if Assigned(FClipStack) then FClipStack.Free;
  FClipStack := nil;
  FClipHeight := 0;
end;

{ TPLDrawingSurfaceStateD2D }

constructor TPLDrawingSurfaceStateD2D.Create(C: TPLDrawingSurfaceD2D);
var
  Path: TPLDrawingSurfacePathD2D;
  i: SizeInt;
begin
  inherited Create;

  Path := C.Path;

  if Path.FClipStack <> nil then begin
    ClipStack := TPLD2D1GeometryList.Create;

    for i := 0 to Path.FClipStack.Count-1 do
      ClipStack.Add(Path.FClipStack[i]);
  end else ClipStack := nil;

  Data := TPLD2D1GeometryList.Create;
  for i := 0 to Path.FData.Count-1 do
    Data.Add(Path.FData[i]);

  Matrix := C.Matrix.Clone;
end;

destructor TPLDrawingSurfaceStateD2D.Destroy;
begin
  if Assigned(ClipStack) then ClipStack.Free;
  if Assigned(Data) then Data.Free;

  inherited Destroy;
end;

procedure TPLDrawingSurfaceStateD2D.Restore(C: TPLDrawingSurfaceD2D);
var
  Path: TPLDrawingSurfacePathD2D;
begin
  Path := C.Path;
  Path.SaveClipStack;
  Path.FClipStack := ClipStack;
  Path.FClipHeight := 0;
  Path.RestoreClipStack;
  Path.Add;
  Path.FData := Path.FData;
  C.SetMatrix(Matrix);
end;

{ TPLDrawingSurfaceD2D }

function TPLDrawingSurfaceD2D.GetHandle: Pointer;
begin
  if not HandleAvailable then exit(nil);

  Result := FTarget;
end;

function TPLDrawingSurfaceD2D.GetMatrix: IPLDrawingMatrix;
begin
  Result := FMatrix;
end;

function TPLDrawingSurfaceD2D.GetPath: IPLDrawingPath;
begin
  Result := FPath;
end;

procedure TPLDrawingSurfaceD2D.SetMatrix(AValue: IPLDrawingMatrix);
begin
  FMatrix := AValue;
end;

function TPLDrawingSurfaceD2D.Path: TPLDrawingSurfacePathD2D;
begin
  Result := FPath as TPLDrawingSurfacePathD2D;
end;

function TPLDrawingSurfaceD2D.Matrix: TPLDrawingMatrixD2D;
begin
  Result := FMatrix as TPLDrawingMatrixD2D;
end;

procedure TPLDrawingSurfaceD2D.Draw;
begin
  if not FDrawing then begin
    FTarget.BeginDraw;
    FDrawing := True;
    Path.RestoreClipStack;
  end;
end;

procedure TPLDrawingSurfaceD2D.AcquireTarget(Target: ID2D1RenderTarget);
const
  NextSurfaceID: Integer = 0;
begin
  FTarget := Target;
  FID := InterLockedIncrement(NextSurfaceID);
end;

function TPLDrawingSurfaceD2D.AcquireBrush(Brush: IPLDrawingBrush; out
  B: ID2D1Brush): TPLBool;
var
  D: TPLDrawingBrushD2D;
begin
  B := nil;

  if not HandleAvailable then exit(False);

  D := Brush as TPLDrawingBrushD2D;
  Result := D.Acquire(FTarget, FID);

  if Result then B := D.FBrush;
end;

function TPLDrawingSurfaceD2D.AcquirePen(Pen: IPLDrawingPen; out B: ID2D1Brush;
  out S: ID2D1StrokeStyle): TPLBool;
var
  D: TPLDrawingPenD2D;
begin
  B := nil;
  S := nil;
  if not HandleAvailable then exit(False);

  D := Pen as TPLDrawingPenD2D;
  Result := D.Acquire(FTarget, FID);

  if Result then begin
    B := D.CurrentBrush;
    S := D.FStyle;
  end;
end;

function TPLDrawingSurfaceD2D.HandleAvailable: TPLBool;
begin
  Result := FTarget <> nil;
end;

procedure TPLDrawingSurfaceD2D.HandleRelease;
begin
  Flush;
  FTarget := nil;
  Path.FClipStack := nil;
  Path.FClipHeight := 0;
  if Assigned(FStateStack) then FStateStack.Free;
  FStateStack := nil;
end;

constructor TPLDrawingSurfaceD2D.Create(T: ID2D1RenderTarget);
begin
  inherited Create;

  FStateStack := TPLDrawingSurfaceStateD2DList.Create;

  AcquireTarget(T);
  FPath := TPLDrawingSurfacePathD2D.Create(Self);
  FMatrix := NewDrawingMatrixD2D;
end;

destructor TPLDrawingSurfaceD2D.Destroy;
begin
  HandleRelease;
  Path.FSurface := nil;

  FStateStack.Free;

  inherited Destroy;
end;

procedure TPLDrawingSurfaceD2D.ShareRelease;
begin
  //
end;

procedure TPLDrawingSurfaceD2D.Save;
begin
  if not HandleAvailable then exit;

  if FStateStack = nil then
    FStateStack := TPLDrawingSurfaceStateD2DList.Create(True);

  FStateStack.Add(TPLDrawingSurfaceStateD2D.Create(Self));
end;

procedure TPLDrawingSurfaceD2D.Restore;
var
  S: TPLDrawingSurfaceStateD2D;
begin
  if not HandleAvailable then
    exit;

  if FStateStack = nil then
    exit;

  S := FStateStack.Last;
  S.Restore(Self);
  FStateStack.Remove(S);

  if FStateStack.Count < 1 then
    FreeAndNil(FStateStack);
end;

procedure TPLDrawingSurfaceD2D.Flush;
begin
  if not HandleAvailable then exit;

  if FDrawing then begin
    Path.SaveClipStack;
    FTarget.EndDraw;
    FTarget.Flush(nil, nil);
  end;

  FDrawing := False;
end;

procedure TPLDrawingSurfaceD2D.Clear(const AColor: TPLColor);
begin
  ShareRelease;

  if not HandleAvailable then exit;

  Draw;
  FTarget.Clear(AColor);
end;

procedure TPLDrawingSurfaceD2D.MoveTo(const AX, AY: TPLFloat);
begin
  if not HandleAvailable then exit;

  Path.Open(AX, AY);
end;

procedure TPLDrawingSurfaceD2D.LineTo(const AX, AY: TPLFloat);
var
  L: TD2D1Point2F;
begin
  ShareRelease;

  if not HandleAvailable then exit;

  L.x := AX;
  L.y := AY;
  Path.Open;
  Path.FFigureSink.AddLine(L);
end;

procedure TPLDrawingSurfaceD2D.ArcTo(const ARect: TPLRectF; const ABeginAngle,
  AEndAngle: TPLFloat);
const
  Sweep: array[Boolean] of TD2D1SweepDirection =
    (D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE, D2D1_SWEEP_DIRECTION_CLOCKWISE);
  Size: array[Boolean] of TD2D1ArcSize =
    (D2D1_ARC_SIZE_SMALL, D2D1_ARC_SIZE_LARGE);
var
  P: TPLDrawingSurfacePathD2D;
  A: TD2D1ArcSegment;
  L: TD2D1Point2F;
  W, H: TPLFloat;
begin
  ShareRelease;

  if not HandleAvailable then
    Exit;

  P := Path;
  L.x := Sin(ABeginAngle);
  L.y := -Cos(ABeginAngle);
  W := ARect.Width / 2;
  H := ARect.Height / 2;

  with ARect.Middle do begin
    L.x := X + L.x * W;
    L.y := Y + L.y * H;
  end;

  if P.FFigureOpened then
    P.FFigureSink.AddLine(L)
  else
    P.Open(L.x, L.y);

  L.x := Sin(AEndAngle);
  L.y := -Cos(AEndAngle);

  with ARect.Middle do begin
    L.x := X + L.x * W;
    L.y := Y + L.y * H;
  end;

  A.point := L;
  A.size.width := W;
  A.size.height := H;
  A.rotationAngle := 0;
  A.sweepDirection := Sweep[AEndAngle > ABeginAngle];
  A.arcSize := Size[Abs(ABeginAngle - AEndAngle) > Pi];
  P.FFigureSink.AddArc(A);
end;

procedure TPLDrawingSurfaceD2D.CurveTo(const AX, AY: TPLFloat; const AC1,
  AC2: TPLPointF);
var
  B: TD2D1BezierSegment;
begin
  ShareRelease;

  if not HandleAvailable then exit;

  B.point1 := AC1;
  B.point2 := AC2;
  B.point3.x := AX;
  B.point3.y := AY;
  Path.Open;
  Path.FFigureSink.AddBezier(B);
end;

procedure TPLDrawingSurfaceD2D.Ellipse(const ARect: TPLRectF);
begin
  ShareRelease;
  if not HandleAvailable then exit;

  Path.Add(CreateEllispe(ARect));
end;

procedure ApplyMatrix(ABrush: ID2D1Brush; AMatrix: IPLDrawingMatrix; out AState: TD2D1Matrix3x2F);
var
  M: TD2D1Matrix3x2F;
begin
  AState := MatrixIdentity;

  if ABrush = nil then exit;

  M := (AMatrix as TPLDrawingMatrixD2D).FMatrix;
  ABrush.GetTransform(AState);
  M := MatrixMultiply(AState, M);
  ABrush.SetTransform(M);
end;

procedure RestoreMatrix(ABrush: ID2D1Brush; AState: TD2D1Matrix3x2F);
begin
  if ABrush = nil then exit;

  ABrush.SetTransform(AState);
end;

function PenWidth(AMatrix: IPLDrawingMatrix; Width: TPLFloat): TPLFloat;
const
  A: TPLPointF = (FX: 1; FY: 0);
  B: TPLPointF = (FX: 0; FY: 0);
begin
  Result := AMatrix.Transform(A) >< AMatrix.Transform(B);
  Result := abs(Result * Width);
end;

procedure TPLDrawingSurfaceD2D.FillOrStroke(ABrush: IPLDrawingBrush;
  APen: IPLDrawingPen; APreserve: TPLBool);
var
  Acquired: TPLBool;
  State: TD2D1Matrix3x2F;
  P: TPLDrawingSurfacePathD2D;
  B: ID2D1Brush;
  S: ID2D1StrokeStyle;
  G: ID2D1Geometry;
  I: Integer;
begin
  ShareRelease;
  B := nil;
  S := nil;

  if ABrush <> nil then begin
    Acquired := AcquireBrush(ABrush, B);
    if Acquired then ApplyMatrix(B, GetMatrix, State);
  end else begin
    Acquired := AcquirePen(APen, B, S);
    if Acquired then ApplyMatrix(B, GetMatrix, State);
  end;

  if not Acquired then exit;

  Draw;
  P := Path;
  P.Add;
  I := P.FData.Count;

  if I = 0 then exit;

  if I = 1 then
    G := P.FData.First
  else
    G := CreateGroup(P.FData.Data, I);

  if ABrush <> nil then
    FTarget.FillGeometry(G, B)
  else
    FTarget.DrawGeometry(G, B, PenWidth(GetMatrix, APen.Width), S);

  if not APreserve then P.Remove;

  if B <> nil then RestoreMatrix(B, State);
end;

procedure TPLDrawingSurfaceD2D.Stroke(APen: IPLDrawingPen;
  const APreserve: TPLBool);
begin
  FillOrStroke(nil, APen, APreserve);
end;

procedure TPLDrawingSurfaceD2D.Fill(ABrush: IPLDrawingBrush;
  const APreserve: TPLBool);
begin
  FillOrStroke(ABrush, nil, APreserve);
end;

function TPLDrawingSurfaceD2D.TextSize(AFont: IPLDrawingFont;
  const AText: string): TPLPointF;
begin

end;

function TPLDrawingSurfaceD2D.TextHeight(AFont: IPLDrawingFont;
  const AText: string; const AWidth: TPLFloat): TPLFloat;
begin

end;

procedure TPLDrawingSurfaceD2D.TextOut(AFont: IPLDrawingFont;
  const AText: string; const ARect: TPLRectF;
  const ADirection: TPLTextDirection; const AImmediate: TPLBool);
begin

end;

{ TBitmapSurfaceD2D }

function TBitmapSurfaceD2D.HandleAvailable: TPLBool;
begin
  Result:=inherited HandleAvailable;
end;

procedure TBitmapSurfaceD2D.HandleRelease;
begin
  inherited HandleRelease;
end;

constructor TBitmapSurfaceD2D.Create(B: TPLDrawingBitmapD2D);
begin

end;

function TBitmapSurfaceD2D.ShareCreate(Target: ID2D1RenderTarget): ID2D1Bitmap;
begin

end;

procedure TBitmapSurfaceD2D.ShareRelease;
begin
  inherited ShareRelease;
end;

{ TPLDrawingBitmapD2D }

procedure TPLDrawingBitmapD2D.HandleRelease;
begin
  inherited HandleRelease;
end;

function TPLDrawingBitmapD2D.GetSurface: IPLDrawingSurface;
begin
  Result:=inherited GetSurface;
end;

function TPLDrawingBitmapD2D.GetPixels: PPLPixel;
begin
  Result:=inherited GetPixels;
end;

destructor TPLDrawingBitmapD2D.Destroy;
begin
  inherited Destroy;
end;

procedure TPLDrawingBitmapD2D.Clear;
begin
  inherited Clear;
end;


{ TPLD2D1Drawer }

constructor TPLD2D1Drawer.Create(ACanvas: TCanvas);
begin
  inherited Create(ACanvas);

  FSurface := NewDrawingSurfaceD2D(ACanvas);
end;

destructor TPLD2D1Drawer.Destroy;
begin
  inherited Destroy;
end;

procedure TPLD2D1Drawer.DrawBox(ARect: TPLRectF; ABackground: IPLDrawingBrush;
  ABorders: TPLDrawingBorders);

  procedure CorrectRadius(var w: TPLFloat);
  var
    r: TPLFloat;
  begin
    r := min(ARect.Width, ARect.Height) / 2;
    if w < 0 then w := 0
    else if w > r then w := r;
  end;

  function DoublePenSize(s: TPLFloat): TPLFloat; inline;
  begin
    Result := ceil64(s / 3);
  end;

  procedure StrokePen(opts: TPLDrawingBorder; APart: byte = 1);
  var
    pen: IPLDrawingPen;
  begin
    if Assigned(ABorders.Image) and (opts.Style <> dbsNone) then begin
      pen := NewDrawingPenD2D(ABorders.Image, opts.Width);
      FSurface.Stroke(pen);
      exit;
    end;

    case opts.Style of
      dbsNone: begin
        // nothing
      end;
      dbsSolid: begin
        pen := NewDrawingPenD2D(opts.Color, opts.Width);
        FSurface.Stroke(pen);
      end;
      dbsDashed: begin
        pen := NewDrawingPenD2D(opts.Color, opts.Width);
        pen.SetStyle(dpsDash);
        FSurface.Stroke(pen);
      end;
      dbsDotted: begin
        pen := NewDrawingPenD2D(opts.Color, opts.Width);
        pen.SetStyle(dpsDot);
        FSurface.Stroke(pen);
      end;
      dbsDouble: begin
        pen := NewDrawingPenD2D(opts.Color, DoublePenSize(opts.Width / 3));
        FSurface.Stroke(pen);
      end;
      dbsInset: begin
        if APart in [1, 4] then pen := NewDrawingPenD2D(opts.Color.ChangeLightness(-0.3), opts.Width) else
          pen := NewDrawingPenD2D(opts.Color, opts.Width);
        FSurface.Stroke(pen);
      end;
      dbsOutset: begin
        if APart in [2, 3] then pen := NewDrawingPenD2D(opts.Color.ChangeLightness(-0.3), opts.Width) else
          pen := NewDrawingPenD2D(opts.Color, opts.Width);
        FSurface.Stroke(pen);
      end;
    end;
  end;

const
  _x_const = 0.2928932188134524755991556; // 1 - sqrt(2)/2

var
  x, r: TPLFloat;

  procedure DoPart(t: byte; cr: TPLRectF; dr: TPLFloat = 1);
  begin
    case t of
      1 {left}: begin
        r := ABorders.Radius[1] * dr;
        x := _x_const * r;
        FSurface.MoveTo(cr.Left + x, cr.Top + x);
        FSurface.ArcTo(TPLRectF.Create(cr.Left, cr.Top, r * 2, r * 2), -pi/4, -pi/2);

        r := ABorders.Radius[3] * dr;
        FSurface.LineTo(cr.Left, cr.Bottom - r);
        if r > 0 then
          FSurface.ArcTo(TPLRectF.Create(cr.Left, cr.Bottom - 2 * r, r * 2, r * 2), -pi/2, -3*pi/4)
        else
          FSurface.LineTo(cr.Left + ABorders.Left.Width / 2, cr.Bottom);
      end;
      2 {bottom}: begin
        r := ABorders.Radius[3] * dr;
        x := _x_const * r;
        //FSurface.MoveTo(cr.Left + x, cr.Bottom - x);
        FSurface.ArcTo(TPLRectF.Create(cr.Left, cr.Bottom - 2 * r, r * 2, r * 2), 5*pi/4, pi);

        r := ABorders.Radius[4] * dr;
        FSurface.LineTo(cr.Right - r, cr.Bottom);
        if r > 0 then
          FSurface.ArcTo(TPLRectF.Create(cr.Right - 2 * r, cr.Bottom - 2 * r, r * 2, r * 2), pi, 3*pi/4)
        else
          FSurface.LineTo(cr.Right - r, cr.Bottom - ABorders.Bottom.Width / 2);
      end;
      3 {right}: begin
        r := ABorders.Radius[4] * dr;
        x := _x_const * r;
        //FSurface.MoveTo(cr.Right - x, cr.Bottom - x);
        FSurface.ArcTo(TPLRectF.Create(cr.Right - 2 * r, cr.Bottom - 2 * r, r * 2, r * 2), 3*pi/4, pi/2);

        r := ABorders.Radius[2] * dr;
        if r = 0 then FSurface.LineTo(cr.Right, cr.Top + 2 * r);
        if r > 0 then
          FSurface.ArcTo(TPLRectF.Create(cr.Right - 2 * r, cr.Top, r * 2, r * 2), pi/2, pi/4)
        else
          FSurface.LineTo(cr.Right - ABorders.Bottom.Width / 2, cr.Top);
      end;
      4 {top}: begin
        r := ABorders.Radius[2] * dr;
        x := _x_const * r;
        //FSurface.MoveTo(cr.Right - x, cr.Top + x);
        FSurface.ArcTo(TPLRectF.Create(cr.Right - 2 * r, cr.Top, r * 2, r * 2), pi/4, 0);

        r := ABorders.Radius[1] * dr;
        if r = 0 then FSurface.LineTo(cr.Left + 2 * r, cr.Top);
        if r > 0 then
          FSurface.ArcTo(TPLRectF.Create(cr.Left, cr.Top, r * 2, r * 2), 0, -pi/4)
        else
          FSurface.LineTo(cr.Left, cr.Top + ABorders.Bottom.Width / 2);
      end;
    end;
  end;

var
  rec: TPLRectF;

begin
  CorrectRadius(ABorders.Radius[1]);
  CorrectRadius(ABorders.Radius[2]);
  CorrectRadius(ABorders.Radius[3]);
  CorrectRadius(ABorders.Radius[4]);

  if Assigned(ABackground) then begin
    DoPart(1, ARect);
    DoPart(2, ARect);
    DoPart(3, ARect);
    DoPart(4, ARect);
    FSurface.Path.Add;
    FSurface.Fill(ABackground);
    FSurface.Path.Remove;
  end;

  rec := ARect.Inflate(DoublePenSize(ABorders.Left.Width), DoublePenSize(ABorders.Top.Width), DoublePenSize(ABorders.Left.Width) + DoublePenSize(ABorders.Right.Width), DoublePenSize(ABorders.Top.Width) + DoublePenSize(ABorders.Bottom.Width));

  if ABorders.Left.Width > 0 then begin
    DoPart(1, ARect);
    StrokePen(ABorders.Left, 1);
    if ABorders.Left.Style = dbsDouble then begin
      DoPart(1, rec, rec.Width / ARect.Width);
      StrokePen(ABorders.Left);
    end;
    FSurface.Path.Remove;
  end;

  if ABorders.Bottom.Width > 0 then begin
    DoPart(2, ARect);
    StrokePen(ABorders.Bottom, 2);
    if ABorders.Bottom.Style = dbsDouble then begin
      DoPart(2, rec, rec.Height / ARect.Height);
      StrokePen(ABorders.Bottom);
    end;
    FSurface.Path.Remove;
  end;

  if ABorders.Right.Width > 0 then begin
    DoPart(3, ARect);
    StrokePen(ABorders.Right, 3);
    if ABorders.Right.Style = dbsDouble then begin
      DoPart(3, rec, rec.Width / ARect.Width);
      StrokePen(ABorders.Right);
    end;
    FSurface.Path.Remove;
  end;

  if ABorders.Top.Width > 0 then begin
    DoPart(4, ARect);
    StrokePen(ABorders.Top, 4);
    if ABorders.Top.Style = dbsDouble then begin
      DoPart(4, rec, rec.Height / ARect.Height);
      StrokePen(ABorders.Top);
    end;
    FSurface.Path.Remove;
  end;
end;

procedure TPLD2D1Drawer.DrawText(const AText: TPLString; ARect: TPLRectF;
  AFont: IPLDrawingFont; ADirection: TPLTextDirection);
begin
  inherited DrawText(AText, ARect, AFont, ADirection);
end;


// public functions

function NewDrawingMatrixD2D: IPLDrawingMatrix;
begin
  Result := TPLDrawingMatrixD2D.Create(MatrixIdentity);
end;

function NewDrawingPenD2D(ABrush: IPLDrawingBrush; AWidth: TPLFloat): IPLDrawingPen;
begin
  Result := TPLDrawingPenD2D.Create(ABrush, AWidth);
end;

function NewDrawingPenD2D(AColor: TPLColor; AWidth: TPLFloat): IPLDrawingPen;
begin
  Result := TPLDrawingPenD2D.Create(AColor, AWidth);
end;

function NewDrawingSolidBrushD2D(AColor: TPLColor): IPLDrawingBrushSolid;
begin
  Result := TPLDrawingBrushSolidD2D.Create(AColor);
end;

function NewDrawingBitmapBrushD2D(ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
begin
  Result := TPLDrawingBrushBitmapD2D.Create(ABitmap);
end;

function NewDrawingLinearGradientBrushD2D(
  const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
begin
  Result := TPLDrawingBrushGradientLinearD2D.Create(A, B);
end;

function NewDrawingRadialGradientBrushD2D(
  const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
begin
  Result := TPLDrawingBrushGradientRadialD2D.Create(ARect);
end;

function NewDrawingFontD2D(AFontData: TPLDrawingFontData): IPLDrawingFont;
begin
  Result := TPLDrawingFontD2D.Create(AFontData);
end;

var
  ScreenDC: HDC;

function NewDrawingSurfaceD2D(ACanvas: TCanvas): IPLDrawingSurface;
var
  T: ID2D1DCRenderTarget;
  R: TRect;
begin
  T := CreateDCTarget;

  if ACanvas = nil then
  begin
    if ScreenDC = 0 then ScreenDC := GetDC(0);

    GetWindowRect(GetDesktopWindow, R);
    T.BindDC(ScreenDC, TPLRectI.Create(0, 0, R.Right - R.Left, R.Bottom - R.Top));
  end else
    T.BindDC(ACanvas.Handle, TPLRectI.Create(0, 0, ACanvas.Width, ACanvas.Height));

  Result := TPLDrawingSurfaceD2D.Create(T);
end;

function NewDrawingBitmapD2D(AWidth, AHeight: TPLInt): IPLDrawingBitmap;
begin
  Result := TPLDrawingBitmapD2D.Create;
  Result.SetSize(AWidth, AHeight);
end;

initialization
  Direct2DInit;

end.
