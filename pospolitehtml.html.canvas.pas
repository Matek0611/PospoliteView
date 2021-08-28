unit PospoLiteHTML.HTML.Canvas;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, LCLType, PospoLiteHTML.CSS.Values;

type

  { IPLUint8ClampedArray }

  IPLHTMLUint8ClampedArray = interface
    ['{2B8C7C8E-D910-46B1-9CF4-ED35E6DE723C}']
    function GetData(AIndex: Cardinal): byte;
    procedure SetData(AIndex: Cardinal; AValue: byte);
    function GetLength: Cardinal;

    property Length: Cardinal read GetLength;
    property Data[AIndex: Cardinal]: byte read GetData write SetData;
  end;

  { IPLHTMLCanvasElement }

  IPLHTMLCanvasElement = interface
    ['{4E0E190D-58A4-47F2-B32F-84D408FDBD30}']
    function GetHeight: Cardinal;
    function GetWidth: Cardinal;

    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;
  end;

  { IPLHTMLCanvasGradient }

  IPLHTMLCanvasGradient = interface
    ['{5BA2A0DA-985D-4155-9D91-0F579B272171}']
    function GetData: Variant;
    procedure AddColorStop(AOffset: double; AColor: TPLCSSColor);
  end;

  { IPLHTMLCanvasPattern }

  IPLHTMLCanvasPattern = interface
    ['{9BBE98B4-E182-42AB-8C67-69AAB5360F30}']
    function GetData: Variant;
  end;

  { IPLHTMLCanvasTransformation }

  IPLHTMLCanvasTransformation = interface
    ['{D9BA061C-8410-4B3E-A63F-F60D235F9DD3}']
    procedure Scale(x, y: double);
    procedure Rotate(AAngle: double);
    procedure Translate(x, y: double);
    procedure Transform(a, b, c, d, e, f: double);
    procedure SetTransform(a, b, c, d, e, f: double);
  end;

  { IPLHTMLCanvasLineStyles }

  IPLHTMLCanvasLineStyles = interface
    ['{301468B4-3BBE-4806-BEF9-7A254596D4E7}']
    function GetLineWidth: double;
    function GetLineCap: string;
    function GetLineJoin: string;
    function GetMiterLimit: double;
    procedure SetLineWidth(AValue: double);
    procedure SetLineCap(AValue: string);
    procedure SetLineJoin(AValue: string);
    procedure SetMiterLimit(AValue: double);

    property LineWidth: double read GetLineWidth write SetLineWidth; // [1]
    property LineCap: string read GetLineCap write SetLineCap; // [butt], round, square
    property LineJoin: string read GetLineJoin write SetLineJoin; // [miter], round, bevel
    property MiterLimit: double read GetMiterLimit write SetMiterLimit; // [10]
  end;

  { IPLHTMLCanvasText }

  IPLHTMLCanvasText = interface
    ['{289C4707-EBD4-42EF-8D59-0ED8226D9654}']
    function GetFont: string;
    function GetTextAlign: string;
    function GetTextBaseline: string;
    procedure SetFont(AValue: string);
    procedure SetTextAlign(AValue: string);
    procedure SetTextBaseline(AValue: string);

    property Font: string read GetFont write SetFont; // [10px sans-serif]
    property TextAlign: string read GetTextAlign write SetTextAlign; // [start], end, left, right, center
    property TextBaseline: string read GetTextBaseline write SetTextBaseline; // top, hanging, middle, [alphabetic], ideographic, bottom
  end;

  { IPLHTMLCanvasPathMethods }

  IPLHTMLCanvasPathMethods = interface
    ['{6415F81D-A28B-498D-97A8-5088F9E981E1}']
    procedure ClosePath;
    procedure MoveTo(X, Y: double);
    procedure LineTo(X, Y: double);
    procedure QuadraticCurveTo(AControlPointX, AControlPointY, X, Y: double);
    procedure BezierCurveTo(AControlPoint1X, AControlPoint1Y, AControlPoint2X, AControlPoint2Y, X, Y: double);
    procedure ArcTo(X1, Y1, X2, Y2, ARadius: double);
    procedure Rect(X, Y, AWidth, AHeight: double);
    procedure Arc(X, Y, ARadius, AStartAngle, AEndAngle: double; AAntiClockwise: boolean = false);
  end;

  { IPLHTMLTextMetrics }

  IPLHTMLTextMetrics = interface
    ['{E141199C-1827-4F86-94D0-3E00E8BBD850}']
    function GetWidth: Real;

    property Width: Real read GetWidth;
  end;

  { IPLHTMLImageData }

  IPLHTMLImageData = interface
    ['{162B6D1F-3D62-46E9-BDCC-6DAB96452EC4}']
    function GetData: IPLHTMLUint8ClampedArray;
    function GetHeight: Cardinal;
    function GetWidth: Cardinal;

    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetWidth;
    property Data: IPLHTMLUint8ClampedArray read GetData;
  end;

  { IPLHTMLCanvas2DContext }

  IPLHTMLCanvas2DContext = interface
    ['{B4C2B582-13FB-4159-BC12-60051E7FDA75}']
    function GetCanvas: IPLHTMLCanvasElement;
    function GetFillStyle: Variant;
    function GetGlobalAlpha: double;
    function GetGlobalCompositeOperation: string;
    function GetShadowBlur: double;
    function GetShadowColor: TPLCSSColor;
    function GetShadowOffsetX: double;
    function GetShadowOffsetY: double;
    function GetStrokeStyle: Variant;
    procedure SetFillStyle(AValue: Variant);
    procedure SetGlobalAlpha(AValue: double);
    procedure SetGlobalCompositeOperation(AValue: string);
    procedure SetShadowBlur(AValue: double);
    procedure SetShadowColor(AValue: TPLCSSColor);
    procedure SetShadowOffsetX(AValue: double);
    procedure SetShadowOffsetY(AValue: double);
    procedure SetStrokeStyle(AValue: Variant);

    property Canvas: IPLHTMLCanvasElement read GetCanvas;

    procedure Save;
    procedure Restore;

    property GlobalAlpha: double read GetGlobalAlpha write SetGlobalAlpha;
    property GlobalCompositeOperation: string read GetGlobalCompositeOperation
      write SetGlobalCompositeOperation;

    property StrokeStyle: Variant read GetStrokeStyle write SetStrokeStyle;
    property FillStyle: Variant read GetFillStyle write SetFillStyle;
    function CreateLinearGradient(x0, y0, x1, y1: double): IPLHTMLCanvasGradient;
    function CreateRadialGradient(x0, y0, r0, x1, y1, r1: double): IPLHTMLCanvasGradient;
    function CreatePattern(AImage: IPLHTMLCanvasElement; ARepetition: string): IPLHTMLCanvasPattern;

    property ShadowOffsetX: double read GetShadowOffsetX write SetShadowOffsetX;
    property ShadowOffsetY: double read GetShadowOffsetY write SetShadowOffsetY;
    property ShadowBlur: double read GetShadowBlur write SetShadowBlur;
    property ShadowColor: TPLCSSColor read GetShadowColor write SetShadowColor;

    procedure ClearRect(X, Y, AWidth, AHeight: double);
    procedure FillRect(X, Y, AWidth, AHeight: double);
    procedure StrokeRect(X, Y, AWidth, AHeight: double);

    procedure BeginPath;
    procedure Fill;
    procedure Stroke;

    procedure ScrollPathIntoView;
    procedure Clip;
    function IsPointInPath(X, Y: double): boolean;

    procedure FillText(AText: string; x, y: double); overload;
    procedure FillText(AText: string; x, y: double; AMaxWidth: double); overload;
    procedure StrokeText(AText: string; x, y: double); overload;
    procedure StrokeText(AText: string; x, y: double; AMaxWidth: double); overload;
    function MeasureText(AText: string): IPLHTMLTextMetrics;

    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy: double); overload;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy, dw, dh: double); overload;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; sx, sy, sw, sh, dx, dy, dw, dh: double); overload;

    function CreateImageData(sw, sh: double): IPLHTMLImageData; overload;
    function CreateImageData(AImageData: IPLHTMLImageData): IPLHTMLImageData; overload;
    function GetImageData(ASourceX, ASourceY, ASourceWidth, ASourceHeight: double): IPLHTMLImageData;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX, ADestinationY: double); overload;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX,
      ADestinationY, ADirtyX, ADirtyY, ADirtyWidth, ADirtyHeight: double); overload;
  end;

  { TPLHTMLCustomCanvasElement }

  TPLHTMLCustomCanvasElement = class(TInterfacedObject, IPLHTMLCanvasElement)
  protected
    function GetHeight: Cardinal; virtual; abstract;
    function GetWidth: Cardinal; virtual; abstract;
  public
    property Height: Cardinal read GetHeight;
    property Width: Cardinal read GetWidth;
  end;

  { TPLHTMLCustomCanvasGradient }

  TPLHTMLCustomCanvasGradient = class(TInterfacedObject, IPLHTMLCanvasGradient)
  public
    function GetData: Variant; virtual; abstract;
    procedure AddColorStop(AOffset: Double; AColor: TPLCSSColor); virtual; abstract;
  end;

  { TPLHTMLTextMetrics }

  TPLHTMLTextMetrics = class(TInterfacedObject, IPLHTMLTextMetrics)
  private
    FWidth: Real;
    function GetWidth: Real;
  public
    constructor Create(AWidth: Real);

    property Width: Real read GetWidth;
  end;

  { TPLHTMLCustomCanvas2DContext }

  TPLHTMLCustomCanvas2DContext = class(TInterfacedObject, IPLHTMLCanvas2DContext,
    IPLHTMLCanvasTransformation, IPLHTMLCanvasLineStyles, IPLHTMLCanvasText,
    IPLHTMLCanvasPathMethods)
  private
    FCanvasElement: TPLHTMLCustomCanvasElement;
    FFillStyle: Variant;
    FFont: string;
    FGlobalAlpha: double;
    FLineWidth: double;
    FMiterLimit: double;
    FShadowBlur: double;
    FShadowColor: TPLCSSColor;
    FShadowOffsetX: double;
    FShadowOffsetY: double;
    FStrokeStyle: Variant;
  protected
    function GetCanvas: IPLHTMLCanvasElement; virtual;
    function GetFillStyle: Variant; virtual;
    function GetGlobalAlpha: double; virtual;
    function GetGlobalCompositeOperation: string; virtual; abstract;
    function GetShadowBlur: double; virtual;
    function GetShadowColor: TPLCSSColor; virtual;
    function GetShadowOffsetX: double; virtual;
    function GetShadowOffsetY: double; virtual;
    function GetStrokeStyle: Variant; virtual;
    procedure SetFillStyle(AValue: Variant); virtual;
    procedure SetGlobalAlpha(AValue: double); virtual;
    procedure SetGlobalCompositeOperation(AValue: string); virtual; abstract;
    procedure SetShadowBlur(AValue: double); virtual;
    procedure SetShadowColor(AValue: TPLCSSColor); virtual;
    procedure SetShadowOffsetX(AValue: double); virtual;
    procedure SetShadowOffsetY(AValue: double); virtual;
    procedure SetStrokeStyle(AValue: Variant); virtual;

    function GetLineWidth: double; virtual;
    function GetLineCap: string; virtual; abstract;
    function GetLineJoin: string; virtual; abstract;
    function GetMiterLimit: double; virtual;
    procedure SetLineWidth(AValue: double); virtual;
    procedure SetLineCap(AValue: string); virtual; abstract;
    procedure SetLineJoin(AValue: string); virtual; abstract;
    procedure SetMiterLimit(AValue: double); virtual;

    function GetFont: string; virtual;
    function GetTextAlign: string; virtual; abstract;
    function GetTextBaseline: string; virtual; abstract;
    procedure SetFont(AValue: string); virtual;
    procedure SetTextAlign(AValue: string); virtual; abstract;
    procedure SetTextBaseline(AValue: string); virtual; abstract;

    procedure FillStyleChanged; virtual; abstract;
    procedure FontChanged; virtual; abstract;
    procedure GlobalAlphaChanged; virtual; abstract;
    procedure LineWidthChanged; virtual; abstract;
    procedure MiterLimitChanged; virtual; abstract;
    procedure StrokeStyleChanged; virtual; abstract;
    procedure ShadowBlurChanged; virtual; abstract;
    procedure ShadowColorChanged; virtual; abstract;
    procedure ShadowOffsetXChanged; virtual; abstract;
    procedure ShadowOffsetYChanged; virtual; abstract;

    property CanvasElement: TPLHTMLCustomCanvasElement read FCanvasElement;
  public
    constructor Create(ACanvasElement: TPLHTMLCustomCanvasElement); virtual;
    destructor Destroy; override;

    procedure Save; virtual; abstract;
    procedure Restore; virtual; abstract;

    function CreateLinearGradient(x0, y0, x1, y1: double): IPLHTMLCanvasGradient; virtual; abstract;
    function CreateRadialGradient(x0, y0, r0, x1, y1, r1: double): IPLHTMLCanvasGradient; virtual; abstract;
    function CreatePattern(AImage: IPLHTMLCanvasElement; ARepetition: string): IPLHTMLCanvasPattern; virtual; abstract;

    procedure ClearRect(X, Y, AWidth, AHeight: double); virtual; abstract;
    procedure FillRect(X, Y, AWidth, AHeight: double); virtual; abstract;
    procedure StrokeRect(X, Y, AWidth, AHeight: double); virtual; abstract;

    procedure BeginPath; virtual; abstract;
    procedure Fill; virtual; abstract;
    procedure Stroke; virtual; abstract;

    procedure ScrollPathIntoView; virtual; abstract;
    procedure Clip; virtual; abstract;
    function IsPointInPath(X, Y: double): boolean; virtual; abstract;

    procedure FillText(AText: string; x, y: double); overload; virtual; abstract;
    procedure FillText(AText: string; x, y: double; AMaxWidth: double); overload; virtual; abstract;
    procedure StrokeText(AText: string; x, y: double); overload; virtual; abstract;
    procedure StrokeText(AText: string; x, y: double; AMaxWidth: double); overload; virtual; abstract;
    function MeasureText(AText: string): IPLHTMLTextMetrics; virtual; abstract;

    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy: double); overload; virtual; abstract;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy, dw, dh: double); overload; virtual; abstract;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; sx, sy, sw, sh, dx, dy, dw, dh: double); overload; virtual; abstract;

    function CreateImageData(sw, sh: double): IPLHTMLImageData; overload; virtual; abstract;
    function CreateImageData(AImageData: IPLHTMLImageData): IPLHTMLImageData; overload; virtual; abstract;
    function GetImageData(ASourceX, ASourceY, ASourceWidth, ASourceHeight: double): IPLHTMLImageData; virtual; abstract;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX, ADestinationY: double); overload; virtual; abstract;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX,
      ADestinationY, ADirtyX, ADirtyY, ADirtyWidth, ADirtyHeight: double); overload; virtual; abstract;

    procedure Scale(x, y: double); virtual; abstract;
    procedure Rotate(AAngle: double); virtual; abstract;
    procedure Translate(x, y: double); virtual; abstract;
    procedure Transform(a, b, c, d, e, f: double); virtual; abstract;
    procedure SetTransform(a, b, c, d, e, f: double); virtual; abstract;

    procedure ClosePath; virtual; abstract;
    procedure MoveTo(X, Y: double); virtual; abstract;
    procedure LineTo(X, Y: double); virtual; abstract;
    procedure QuadraticCurveTo(AControlPointX, AControlPointY, X, Y: double); virtual; abstract;
    procedure BezierCurveTo(AControlPoint1X, AControlPoint1Y, AControlPoint2X, AControlPoint2Y, X, Y: double); virtual; abstract;
    procedure ArcTo(X1, Y1, X2, Y2, ARadius: double); virtual; abstract;
    procedure Rect(X, Y, AWidth, AHeight: double); virtual; abstract;
    procedure Arc(X, Y, ARadius, AStartAngle, AEndAngle: double; AAntiClockwise: boolean = false); virtual; abstract;

    property Canvas: IPLHTMLCanvasElement read GetCanvas;

    property GlobalAlpha: double read GetGlobalAlpha write SetGlobalAlpha;
    property GlobalCompositeOperation: string read GetGlobalCompositeOperation
      write SetGlobalCompositeOperation;

    property StrokeStyle: Variant read GetStrokeStyle write SetStrokeStyle;
    property FillStyle: Variant read GetFillStyle write SetFillStyle;

    property ShadowOffsetX: double read GetShadowOffsetX write SetShadowOffsetX;
    property ShadowOffsetY: double read GetShadowOffsetY write SetShadowOffsetY;
    property ShadowBlur: double read GetShadowBlur write SetShadowBlur;
    property ShadowColor: TPLCSSColor read GetShadowColor write SetShadowColor;

    property Font: string read GetFont write SetFont; // [10px sans-serif]
    property TextAlign: string read GetTextAlign write SetTextAlign; // [start], end, left, right, center
    property TextBaseline: string read GetTextBaseline write SetTextBaseline; // top, hanging, middle, [alphabetic], ideographic, bottom

    property LineWidth: double read GetLineWidth write SetLineWidth; // [1]
    property LineCap: string read GetLineCap write SetLineCap; // [butt], round, square
    property LineJoin: string read GetLineJoin write SetLineJoin; // [miter], round, bevel
    property MiterLimit: double read GetMiterLimit write SetMiterLimit; // [10]
  end;

implementation

{ TPLHTMLTextMetrics }

function TPLHTMLTextMetrics.GetWidth: Real;
begin
  Result := FWidth;
end;

constructor TPLHTMLTextMetrics.Create(AWidth: Real);
begin
  inherited Create;

  FWidth := AWidth;
end;

{ TPLHTMLCustomCanvas2DContext }

function TPLHTMLCustomCanvas2DContext.GetCanvas: IPLHTMLCanvasElement;
begin
  Result := FCanvasElement;
end;

function TPLHTMLCustomCanvas2DContext.GetFillStyle: Variant;
begin
  Result := FFillStyle;
end;

function TPLHTMLCustomCanvas2DContext.GetGlobalAlpha: double;
begin
  Result := FGlobalAlpha;
end;

function TPLHTMLCustomCanvas2DContext.GetShadowBlur: double;
begin
  Result := FShadowBlur;
end;

function TPLHTMLCustomCanvas2DContext.GetShadowColor: TPLCSSColor;
begin
  Result := FShadowColor;
end;

function TPLHTMLCustomCanvas2DContext.GetShadowOffsetX: double;
begin
  Result := FShadowOffsetX;
end;

function TPLHTMLCustomCanvas2DContext.GetShadowOffsetY: double;
begin
  Result := FShadowOffsetY;
end;

function TPLHTMLCustomCanvas2DContext.GetStrokeStyle: Variant;
begin
  Result := FStrokeStyle;
end;

procedure TPLHTMLCustomCanvas2DContext.SetFillStyle(AValue: Variant);
begin
  if AValue <> FFillStyle then begin
    FFillStyle := AValue;
    FillStyleChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetGlobalAlpha(AValue: double);
begin
  if FGlobalAlpha <> AValue then begin
    FGlobalAlpha := AValue;
    GlobalAlphaChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetShadowBlur(AValue: double);
begin
  if FShadowBlur <> AValue then begin
    FShadowBlur := AValue;
    ShadowBlurChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetShadowColor(AValue: TPLCSSColor);
begin
  if FShadowColor <> AValue then
  begin
    FShadowColor := AValue;
    ShadowColorChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetShadowOffsetX(AValue: double);
begin
  if FShadowOffsetX <> AValue then begin
    FShadowOffsetX := AValue;
    ShadowOffsetXChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetShadowOffsetY(AValue: double);
begin
  if FShadowOffsetY <> AValue then begin
    FShadowOffsetY := AValue;
    ShadowOffsetYChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetStrokeStyle(AValue: Variant);
begin
  if AValue <> FStrokeStyle then begin
    FStrokeStyle := AValue;
    StrokeStyleChanged;
  end;
end;

function TPLHTMLCustomCanvas2DContext.GetLineWidth: double;
begin
  Result := FLineWidth;
end;

function TPLHTMLCustomCanvas2DContext.GetMiterLimit: double;
begin
  Result := FMiterLimit;
end;

procedure TPLHTMLCustomCanvas2DContext.SetLineWidth(AValue: double);
begin
  if AValue <> FLineWidth then begin
    FLineWidth := AValue;
    LineWidthChanged;
  end;
end;

procedure TPLHTMLCustomCanvas2DContext.SetMiterLimit(AValue: double);
begin
  if AValue <> FMiterLimit then begin
    FMiterLimit := AValue;
    MiterLimitChanged;
  end;
end;

function TPLHTMLCustomCanvas2DContext.GetFont: string;
begin
  Result := FFont;
end;

procedure TPLHTMLCustomCanvas2DContext.SetFont(AValue: string);
begin
  if AValue <> FFont then begin
    FFont := AValue;
    FontChanged;
  end;
end;

constructor TPLHTMLCustomCanvas2DContext.Create(
  ACanvasElement: TPLHTMLCustomCanvasElement);
begin
  inherited Create;

  FCanvasElement := ACanvasElement;

  FGlobalAlpha := 1;
  FShadowBlur := 0;
  FShadowOffsetX := 0;
  FShadowOffsetY := 0;
  FShadowColor := 'rgba(0, 0, 0, 0)';
  FLineWidth := 1;
  FMiterLimit := 10;
end;

destructor TPLHTMLCustomCanvas2DContext.Destroy;
begin
  FCanvasElement.Free;

  inherited Destroy;
end;

end.

