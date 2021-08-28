unit PospoLiteHTML.HTML.Canvas.BGRA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, math, PospoLiteHTML.CSS.Values,
  PospoLiteHTML.HTML.Canvas, BGRABitmap, BGRABitmapTypes, Controls, Types,
  BGRACanvas2D, variants, PospoLiteHTML.CSS.Basics;

type

  { TPLHTMLBGRACanvasElement }

  TPLHTMLBGRACanvasElement = class(TPLHTMLCustomCanvasElement)
  protected
    FBitmap: TBGRABitmap;
    FControl: TCustomControl;
    function GetHeight: Cardinal; override;
    function GetWidth: Cardinal; override;
    public procedure OnPaint(Sender: TObject);
  public
    constructor Create(AControl: TCustomControl);
    destructor Destroy; override;
    procedure UpdateBitmap;
  end;

  { TPLHTMLBGRACanvasGradient }

  TPLHTMLBGRACanvasGradient = class(TPLHTMLCustomCanvasGradient)
  private
    FGradient: string;
  public
    constructor Create(AGradient: IBGRACanvasGradient2D);
    procedure AddColorStop(AOffset: Double; AColor: TPLCSSColor); override;
    function GetData: Variant; override;
  end;

  { TPLHTMLBGRACanvas2DContext }

  TPLHTMLBGRACanvas2DContext = class(TPLHTMLCustomCanvas2DContext)
  private
    FGlobalCompositeOperation: string;
  protected
    function GetGlobalCompositeOperation: string; override;
    procedure SetGlobalCompositeOperation(AValue: string); override;

    function GetLineCap: string; override;
    function GetLineJoin: string; override;
    procedure SetLineCap(AValue: string); override;
    procedure SetLineJoin(AValue: string); override;

    function GetTextAlign: string; override;
    function GetTextBaseline: string; override;
    procedure SetTextAlign(AValue: string); override;
    procedure SetTextBaseline(AValue: string); override;

    procedure FillStyleChanged; override;
    procedure FontChanged; override;
    procedure GlobalAlphaChanged; override;
    procedure LineWidthChanged; override;
    procedure MiterLimitChanged; override;
    procedure ShadowBlurChanged; override;
    procedure ShadowColorChanged; override;
    procedure ShadowOffsetXChanged; override;
    procedure ShadowOffsetYChanged; override;
    procedure StrokeStyleChanged; override;

    function ctx: TBGRACanvas2D;
  public
    constructor Create(ACanvasElement: TPLHTMLCustomCanvasElement); override;

    procedure Save; override;
    procedure Restore; override;

    function CreateLinearGradient(x0, y0, x1, y1: double): IPLHTMLCanvasGradient; override;
    function CreateRadialGradient(x0, y0, r0, x1, y1, r1: double): IPLHTMLCanvasGradient; override;
    function CreatePattern(AImage: IPLHTMLCanvasElement; ARepetition: string): IPLHTMLCanvasPattern; override;

    procedure ClearRect(X, Y, AWidth, AHeight: double); override;
    procedure FillRect(X, Y, AWidth, AHeight: double); override;
    procedure StrokeRect(X, Y, AWidth, AHeight: double); override;

    procedure BeginPath; override;
    procedure Fill; override;
    procedure Stroke; override;

    procedure ScrollPathIntoView; override;
    procedure Clip; override;
    function IsPointInPath(X, Y: double): boolean; override;

    procedure FillText(AText: string; x, y: double); overload; override;
    procedure FillText(AText: string; x, y: double; AMaxWidth: double); overload; override;
    procedure StrokeText(AText: string; x, y: double); overload; override;
    procedure StrokeText(AText: string; x, y: double; AMaxWidth: double); overload; override;
    function MeasureText(AText: string): IPLHTMLTextMetrics; override;

    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy: double); overload; override;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; dx, dy, dw, dh: double); overload; override;
    procedure DrawImage(AImage: IPLHTMLCanvasElement; sx, sy, sw, sh, dx, dy, dw, dh: double); overload; override;

    function CreateImageData(sw, sh: double): IPLHTMLImageData; overload; override;
    function CreateImageData(AImageData: IPLHTMLImageData): IPLHTMLImageData; overload; override;
    function GetImageData(ASourceX, ASourceY, ASourceWidth, ASourceHeight: double): IPLHTMLImageData; override;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX, ADestinationY: double); overload; override;
    procedure PutImageData(AImagedata: IPLHTMLImageData; ADestinationX,
      ADestinationY, ADirtyX, ADirtyY, ADirtyWidth, ADirtyHeight: double); overload; override;

    procedure Scale(x, y: double); override;
    procedure Rotate(AAngle: double); override;
    procedure Translate(x, y: double); override;
    procedure Transform(a, b, c, d, e, f: double); override;
    procedure SetTransform(a, b, c, d, e, f: double); override;

    procedure ClosePath; override;
    procedure MoveTo(X, Y: double); override;
    procedure LineTo(X, Y: double); override;
    procedure QuadraticCurveTo(AControlPointX, AControlPointY, X, Y: double); override;
    procedure BezierCurveTo(AControlPoint1X, AControlPoint1Y, AControlPoint2X, AControlPoint2Y, X, Y: double); override;
    procedure ArcTo(X1, Y1, X2, Y2, ARadius: double); override;
    procedure Rect(X, Y, AWidth, AHeight: double); override;
    procedure Arc(X, Y, ARadius, AStartAngle, AEndAngle: double; AAntiClockwise: boolean = false); override;
  end;

implementation

{ TPLHTMLBGRACanvas2DContext }

function TPLHTMLBGRACanvas2DContext.GetGlobalCompositeOperation: string;
begin
  Result := FGlobalCompositeOperation;
end;

procedure TPLHTMLBGRACanvas2DContext.SetGlobalCompositeOperation(AValue: string
  );
begin
  if FGlobalCompositeOperation <> AValue then begin
    FGlobalCompositeOperation := AValue;
  end;
end;

function TPLHTMLBGRACanvas2DContext.GetLineCap: string;
begin
  Result := ctx.lineCap;
end;

function TPLHTMLBGRACanvas2DContext.GetLineJoin: string;
begin
  Result := ctx.lineJoin;
end;

procedure TPLHTMLBGRACanvas2DContext.SetLineCap(AValue: string);
begin
  ctx.lineCap := AValue;
end;

procedure TPLHTMLBGRACanvas2DContext.SetLineJoin(AValue: string);
begin
  ctx.lineJoin := AValue;
end;

function TPLHTMLBGRACanvas2DContext.GetTextAlign: string;
begin
  Result := ctx.textAlign;
end;

function TPLHTMLBGRACanvas2DContext.GetTextBaseline: string;
begin
  Result := ctx.textBaseline;
end;

procedure TPLHTMLBGRACanvas2DContext.SetTextAlign(AValue: string);
begin
  ctx.textAlign := AValue;
end;

procedure TPLHTMLBGRACanvas2DContext.SetTextBaseline(AValue: string);
begin
  ctx.textBaseline := AValue;
end;

procedure TPLHTMLBGRACanvas2DContext.FillStyleChanged;
var
  tmp: string;
  gr: IBGRACanvasGradient2D;
begin
  if VarIsStr(FillStyle) then begin
    case Copy(VarToStr(FillStyle), 1, 2) of
      'g:': begin
        tmp := Copy(VarToStr(FillStyle), 3, Length(VarToStr(FillStyle)));
        //gr := ctx.createLinearGradient();
      end;
      'p:': begin

      end
      else ctx.fillStyle(BGRAColorFromCSS(TPLCSSColor.Create(FillStyle)));
    end;
  end;
end;

procedure TPLHTMLBGRACanvas2DContext.FontChanged;
begin
  ctx.font := Font;
end;

procedure TPLHTMLBGRACanvas2DContext.GlobalAlphaChanged;
begin
  ctx.globalAlpha := GlobalAlpha;
end;

procedure TPLHTMLBGRACanvas2DContext.LineWidthChanged;
begin
  ctx.lineWidth := LineWidth;
end;

procedure TPLHTMLBGRACanvas2DContext.MiterLimitChanged;
begin
  ctx.miterLimit := MiterLimit;
end;

procedure TPLHTMLBGRACanvas2DContext.ShadowBlurChanged;
begin
  ctx.shadowBlur := ShadowBlur;
end;

procedure TPLHTMLBGRACanvas2DContext.ShadowColorChanged;
begin
  ctx.shadowColor(BGRAColorFromCSS(ShadowColor));
end;

procedure TPLHTMLBGRACanvas2DContext.ShadowOffsetXChanged;
begin
  ctx.shadowOffsetX := ShadowOffsetX;
end;

procedure TPLHTMLBGRACanvas2DContext.ShadowOffsetYChanged;
begin
  ctx.shadowOffsetY := ShadowOffsetY;
end;

procedure TPLHTMLBGRACanvas2DContext.StrokeStyleChanged;
begin
  //ctx.strokeStyle();
end;

function TPLHTMLBGRACanvas2DContext.ctx: TBGRACanvas2D;
begin
  Result := TPLHTMLBGRACanvasElement(CanvasElement).FBitmap.Canvas2D;
end;

constructor TPLHTMLBGRACanvas2DContext.Create(
  ACanvasElement: TPLHTMLCustomCanvasElement);
begin
  inherited Create(ACanvasElement);


end;

procedure TPLHTMLBGRACanvas2DContext.Save;
begin
  ctx.save;
end;

procedure TPLHTMLBGRACanvas2DContext.Restore;
begin
  ctx.restore;
end;

function TPLHTMLBGRACanvas2DContext.CreateLinearGradient(x0, y0, x1, y1: double
  ): IPLHTMLCanvasGradient;
begin
  Result := TPLHTMLBGRACanvasGradient.Create(ctx.createLinearGradient(x0, y0, x1, y1));
end;

function TPLHTMLBGRACanvas2DContext.CreateRadialGradient(x0, y0, r0, x1, y1,
  r1: double): IPLHTMLCanvasGradient;
begin

end;

function TPLHTMLBGRACanvas2DContext.CreatePattern(AImage: IPLHTMLCanvasElement;
  ARepetition: string): IPLHTMLCanvasPattern;
begin

end;

procedure TPLHTMLBGRACanvas2DContext.ClearRect(X, Y, AWidth, AHeight: double);
begin
  ctx.clearRect(X, Y, AWidth, AHeight);
end;

procedure TPLHTMLBGRACanvas2DContext.FillRect(X, Y, AWidth, AHeight: double);
begin
  ctx.fillRect(X, Y, AWidth, AHeight);
end;

procedure TPLHTMLBGRACanvas2DContext.StrokeRect(X, Y, AWidth, AHeight: double);
begin
  ctx.strokeRect(X, Y, AWidth, AHeight);
end;

procedure TPLHTMLBGRACanvas2DContext.BeginPath;
begin
  ctx.beginPath;
end;

procedure TPLHTMLBGRACanvas2DContext.Fill;
begin
  ctx.fill;
end;

procedure TPLHTMLBGRACanvas2DContext.Stroke;
begin
  ctx.stroke;
end;

procedure TPLHTMLBGRACanvas2DContext.ScrollPathIntoView;
begin
  //
end;

procedure TPLHTMLBGRACanvas2DContext.Clip;
begin
  ctx.clip;
end;

function TPLHTMLBGRACanvas2DContext.IsPointInPath(X, Y: double): boolean;
begin
  Result := ctx.isPointInPath(X, Y);
end;

procedure TPLHTMLBGRACanvas2DContext.FillText(AText: string; x, y: double);
begin
  ctx.fillText(AText, x, y);
end;

procedure TPLHTMLBGRACanvas2DContext.FillText(AText: string; x, y: double;
  AMaxWidth: double);
begin
  ctx.fillText(AText, x, y);
end;

procedure TPLHTMLBGRACanvas2DContext.StrokeText(AText: string; x, y: double);
begin
  ctx.strokeText(AText, x, y);
end;

procedure TPLHTMLBGRACanvas2DContext.StrokeText(AText: string; x, y: double;
  AMaxWidth: double);
begin
  ctx.strokeText(AText, x, y);
end;

function TPLHTMLBGRACanvas2DContext.MeasureText(AText: string
  ): IPLHTMLTextMetrics;
begin
  Result := TPLHTMLTextMetrics.Create(ctx.measureText(AText).width);
end;

procedure TPLHTMLBGRACanvas2DContext.DrawImage(AImage: IPLHTMLCanvasElement;
  dx, dy: double);
begin

end;

procedure TPLHTMLBGRACanvas2DContext.DrawImage(AImage: IPLHTMLCanvasElement;
  dx, dy, dw, dh: double);
begin

end;

procedure TPLHTMLBGRACanvas2DContext.DrawImage(AImage: IPLHTMLCanvasElement;
  sx, sy, sw, sh, dx, dy, dw, dh: double);
begin

end;

function TPLHTMLBGRACanvas2DContext.CreateImageData(sw, sh: double
  ): IPLHTMLImageData;
begin

end;

function TPLHTMLBGRACanvas2DContext.CreateImageData(AImageData: IPLHTMLImageData
  ): IPLHTMLImageData;
begin

end;

function TPLHTMLBGRACanvas2DContext.GetImageData(ASourceX, ASourceY,
  ASourceWidth, ASourceHeight: double): IPLHTMLImageData;
begin

end;

procedure TPLHTMLBGRACanvas2DContext.PutImageData(AImagedata: IPLHTMLImageData;
  ADestinationX, ADestinationY: double);
begin

end;

procedure TPLHTMLBGRACanvas2DContext.PutImageData(AImagedata: IPLHTMLImageData;
  ADestinationX, ADestinationY, ADirtyX, ADirtyY, ADirtyWidth,
  ADirtyHeight: double);
begin

end;

procedure TPLHTMLBGRACanvas2DContext.Scale(x, y: double);
begin
  ctx.scale(x, y);
end;

procedure TPLHTMLBGRACanvas2DContext.Rotate(AAngle: double);
begin
  ctx.rotate(AAngle);
end;

procedure TPLHTMLBGRACanvas2DContext.Translate(x, y: double);
begin
  ctx.translate(x, y);
end;

procedure TPLHTMLBGRACanvas2DContext.Transform(a, b, c, d, e, f: double);
begin
  ctx.transform(a, b, c, d, e, f);
end;

procedure TPLHTMLBGRACanvas2DContext.SetTransform(a, b, c, d, e, f: double);
begin
  ctx.setTransform(a, b, c, d, e, f);
end;

procedure TPLHTMLBGRACanvas2DContext.ClosePath;
begin
  ctx.closePath;
end;

procedure TPLHTMLBGRACanvas2DContext.MoveTo(X, Y: double);
begin
  ctx.moveTo(X, Y);
end;

procedure TPLHTMLBGRACanvas2DContext.LineTo(X, Y: double);
begin
  ctx.lineTo(X, Y);
end;

procedure TPLHTMLBGRACanvas2DContext.QuadraticCurveTo(AControlPointX,
  AControlPointY, X, Y: double);
begin
  ctx.quadraticCurveTo(AControlPointX, AControlPointY, X, Y);
end;

procedure TPLHTMLBGRACanvas2DContext.BezierCurveTo(AControlPoint1X,
  AControlPoint1Y, AControlPoint2X, AControlPoint2Y, X, Y: double);
begin
  ctx.bezierCurveTo(AControlPoint1X, AControlPoint1Y, AControlPoint2X, AControlPoint2Y, X, Y);
end;

procedure TPLHTMLBGRACanvas2DContext.ArcTo(X1, Y1, X2, Y2, ARadius: double);
begin
  ctx.arcTo(X1, Y1, X2, Y2, ARadius);
end;

procedure TPLHTMLBGRACanvas2DContext.Rect(X, Y, AWidth, AHeight: double);
begin
  ctx.rect(X, Y, AWidth, AHeight);
end;

procedure TPLHTMLBGRACanvas2DContext.Arc(X, Y, ARadius, AStartAngle,
  AEndAngle: double; AAntiClockwise: boolean);
begin
  ctx.arc(X, Y, ARadius, AStartAngle, AEndAngle, AAntiClockwise);
end;

{ TPLHTMLBGRACanvasGradient }

constructor TPLHTMLBGRACanvasGradient.Create(AGradient: IBGRACanvasGradient2D);
begin
  inherited Create;

  FGradient := 'g:';
  if Assigned(AGradient) then begin

  end;
end;

procedure TPLHTMLBGRACanvasGradient.AddColorStop(AOffset: Double;
  AColor: TPLCSSColor);
begin
  //FGradient.addColorStop(AOffset, BGRAColorFromCSS(AColor));
  if FGradient <> 'g:' then FGradient := FGradient + ',';
  FGradient := FGradient + FloatToStr(AOffset, PLCSSFormatSettingsDef) + ',' + AColor.AsHex;
end;

function TPLHTMLBGRACanvasGradient.GetData: Variant;
begin
  Result := FGradient;
end;

{ TPLHTMLBGRACanvasElement }

function TPLHTMLBGRACanvasElement.GetHeight: Cardinal;
begin
  Result := FBitmap.Height;
end;

function TPLHTMLBGRACanvasElement.GetWidth: Cardinal;
begin
  Result := FBitmap.Width;
end;

procedure TPLHTMLBGRACanvasElement.OnPaint(Sender: TObject);
begin
  UpdateBitmap;
  FBitmap.Draw(FControl.Canvas, 0, 0, false);
end;

constructor TPLHTMLBGRACanvasElement.Create(AControl: TCustomControl);
begin
  inherited Create;

  FControl := AControl;
  FControl.OnPaint := @OnPaint;
  FBitmap := TBGRABitmap.Create(FControl.Width, FControl.Height);
end;

destructor TPLHTMLBGRACanvasElement.Destroy;
begin
  FBitmap.Free;

  inherited Destroy;
end;

procedure TPLHTMLBGRACanvasElement.UpdateBitmap;
var
  tmp: TBGRABitmap;
begin
  if (FBitmap.Width <> FControl.Width) or (FBitmap.Height <> FControl.Height) then begin
    tmp := TBGRABitmap.Create(FBitmap.Width, FBitmap.Height);
    try
      FBitmap.Canvas2D.save;
      tmp.PutImage(0, 0, FBitmap, dmDrawWithTransparency);
      FBitmap.SetSize(FControl.Width, FControl.Height);
      FBitmap.PutImage(0, 0, tmp, dmDrawWithTransparency);
      FBitmap.Canvas2D.restore;
    finally
      tmp.Free;
    end;
  end;
end;

end.

