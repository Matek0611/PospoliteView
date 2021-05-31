unit Pospolite.View.Drawing.Renderer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Pospolite.View.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.CSS.Basics, Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Drawer
  {$ifdef windows}, Pospolite.View.Drawing.DrawerD2D1{$endif};

type

  {$ifdef windows}
    TPLDrawingDrawerDef = TPLD2D1Drawer;
  {$else}
    TPLDrawingDrawerDef = TPLAbstractDrawer;
  {$endif}

  { IPLDrawingRenderer }

  IPLDrawingRenderer = interface
    procedure DrawBox(ARect: TPLRectF; AProperties: TPLCSSDeclarations);
  end;

  { TPLDrawingRenderer }

  TPLDrawingRenderer = class(TInterfacedObject, IPLDrawingRenderer)
  private
    FDrawer: TPLDrawingDrawerDef;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    procedure DrawBox(ARect: TPLRectF; AProperties: TPLCSSDeclarations);
  end;

  function NewDrawingRenderer(ACanvas: TCanvas): IPLDrawingRenderer;

implementation

function NewDrawingMatrix: IPLDrawingMatrix;
begin
  {$ifdef windows}
    Result := NewDrawingMatrixD2D;
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingPen(ABrush: IPLDrawingBrush; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
begin
  {$ifdef windows}
    Result := NewDrawingPenD2D(ABrush, AWidth);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingPen(AColor: TPLColor; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
begin
  {$ifdef windows}
    Result := NewDrawingPenD2D(AColor, AWidth);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingSolidBrush(AColor: TPLColor): IPLDrawingBrushSolid;
begin
  {$ifdef windows}
    Result := NewDrawingSolidBrushD2D(AColor);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingBitmapBrush(ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
begin
  {$ifdef windows}
    Result := NewDrawingBitmapBrushD2D(ABitmap);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingLinearGradientBrush(
  const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
begin
  {$ifdef windows}
    Result := NewDrawingLinearGradientBrush(A, B);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingRadialGradientBrush(
  const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
begin
  {$ifdef windows}
    Result := NewDrawingRadialGradientBrush(ARect);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingFont(AFontData: TPLDrawingFontData): IPLDrawingFont;
begin
  {$ifdef windows}
    Result := NewDrawingFont(AFontData);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingSurface(ACanvas: TCanvas): IPLDrawingSurface;
begin
  {$ifdef windows}
    Result := NewDrawingSurfaceD2D(ACanvas);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingBitmap(AWidth, AHeight: TPLInt): IPLDrawingBitmap;
begin
  {$ifdef windows}
    Result := NewDrawingBitmapD2D(AWidth, AHeight);
  {$else}
    Result := nil;
  {$endif}
end;


function ColorVal(part: TPLCSSPropertyValuePart): TPLColor;
begin
  if part is TPLCSSPropertyValuePartStringOrIdentifier then
    Result := part.AsString
  else if part is TPLCSSPropertyValuePartFunction then
    Result := TPLCSSPropertyValuePartFunction(part)
  else
    Result := TPLColor.Transparent;
end;

function ExtractCSSBackground(AStyles: TPLCSSDeclarations): IPLDrawingBrush;
var
  prop: TPLCSSProperty;
begin
  Result := nil;

  if AStyles.Exists('background-color', prop) and (prop.Value.Count > 0) then begin
    Result := NewDrawingSolidBrush(ColorVal(prop.Value[0]));
  end;
end;

{ TPLDrawingRenderer }

constructor TPLDrawingRenderer.Create(ACanvas: TCanvas);
begin
  inherited Create;

  FDrawer := TPLDrawingDrawerDef.Create(ACanvas);
end;

destructor TPLDrawingRenderer.Destroy;
begin
  FDrawer.Free;

  inherited Destroy;
end;

procedure TPLDrawingRenderer.DrawBox(ARect: TPLRectF;
  AProperties: TPLCSSDeclarations);
var
  bg: IPLDrawingBrush = nil;
  brd: TPLDrawingBorders;
begin
  bg := ExtractCSSBackground(AProperties);

  brd := Default(TPLDrawingBorders);

  FDrawer.DrawBox(ARect, bg, brd);
end;


function NewDrawingRenderer(ACanvas: TCanvas): IPLDrawingRenderer;
begin
  Result := TPLDrawingRenderer.Create(ACanvas);
end;

end.

