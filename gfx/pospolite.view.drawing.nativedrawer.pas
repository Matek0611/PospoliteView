unit Pospolite.View.Drawing.NativeDrawer;

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

interface

uses
  Classes, SysUtils, Graphics, Pospolite.View.Basics, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Drawer, math;

type

  { TPLNativeDrawer }

  TPLNativeDrawer = class(TPLAbstractDrawer)
  public
    constructor Create(ACanvas: TCanvas); override;
    destructor Destroy; override;

    procedure DrawObjectBackground(const AObject: TPLHTMLObject); override;
    procedure DrawObjectBorder(const AObject: TPLHTMLObject); override;
    procedure DrawObjectOutline(const AObject: TPLHTMLObject); override;
    procedure DrawObjectOutlineText(const AObject: TPLHTMLObject); override;
    procedure DrawObjectText(const AObject: TPLHTMLObject); override;
    procedure DrawObjectDebug(const AObject: TPLHTMLObject); override;
    procedure DrawObjectShadow(const AObject: TPLHTMLObject); override;
    procedure DrawObjectTextShadow(const AObject: TPLHTMLObject); override;
    procedure DrawObjectPseudoBefore(const AObject: TPLHTMLObject); override;
    procedure DrawObjectPseudoAfter(const AObject: TPLHTMLObject); override;

    procedure DrawFrame(const ABorders: TPLDrawingBorders; const ARect: TPLRectF); override;
  end;

function NewDrawingMatrixNative: IPLDrawingMatrix;
function NewDrawingPenNative(ABrush: IPLDrawingBrush; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
function NewDrawingPenNative(AColor: TPLColor; AWidth: TPLFloat = 1): IPLDrawingPen;
  overload;
function NewDrawingSolidBrushNative(AColor: TPLColor): IPLDrawingBrushSolid;
function NewDrawingBitmapBrushNative(ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
function NewDrawingLinearGradientBrushNative(
  const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
function NewDrawingRadialGradientBrushNative(
  const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
function NewDrawingFontNative(AFontData: TPLDrawingFontData): IPLDrawingFont;
function NewDrawingSurfaceNative(ACanvas: TCanvas): IPLDrawingSurface;
function NewDrawingBitmapNative(AWidth, AHeight: TPLInt): IPLDrawingBitmap;

implementation

{ TPLNativeDrawer }

constructor TPLNativeDrawer.Create(ACanvas: TCanvas);
begin
  inherited Create(ACanvas);
end;

destructor TPLNativeDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TPLNativeDrawer.DrawObjectBackground(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectBackground(AObject);
end;

procedure TPLNativeDrawer.DrawObjectBorder(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectBorder(AObject);
end;

procedure TPLNativeDrawer.DrawObjectOutline(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectOutline(AObject);
end;

procedure TPLNativeDrawer.DrawObjectOutlineText(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectOutlineText(AObject);
end;

procedure TPLNativeDrawer.DrawObjectText(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectText(AObject);
end;

procedure TPLNativeDrawer.DrawObjectDebug(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectDebug(AObject);
end;

procedure TPLNativeDrawer.DrawObjectShadow(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectShadow(AObject);
end;

procedure TPLNativeDrawer.DrawObjectTextShadow(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectTextShadow(AObject);
end;

procedure TPLNativeDrawer.DrawObjectPseudoBefore(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectPseudoBefore(AObject);
end;

procedure TPLNativeDrawer.DrawObjectPseudoAfter(const AObject: TPLHTMLObject);
begin
  inherited DrawObjectPseudoAfter(AObject);
end;

procedure TPLNativeDrawer.DrawFrame(const ABorders: TPLDrawingBorders;
  const ARect: TPLRectF);
begin
  //inherited DrawFrame(ABorders);
end;


function NewDrawingMatrixNative: IPLDrawingMatrix;
begin

end;

function NewDrawingPenNative(ABrush: IPLDrawingBrush; AWidth: TPLFloat
  ): IPLDrawingPen;
begin

end;

function NewDrawingPenNative(AColor: TPLColor; AWidth: TPLFloat): IPLDrawingPen;
begin

end;

function NewDrawingSolidBrushNative(AColor: TPLColor): IPLDrawingBrushSolid;
begin

end;

function NewDrawingBitmapBrushNative(ABitmap: IPLDrawingBitmap
  ): IPLDrawingBrushBitmap;
begin

end;

function NewDrawingLinearGradientBrushNative(const A, B: TPLPointF
  ): IPLDrawingBrushGradientLinear;
begin

end;

function NewDrawingRadialGradientBrushNative(const ARect: TPLRectF
  ): IPLDrawingBrushGradientRadial;
begin

end;

function NewDrawingFontNative(AFontData: TPLDrawingFontData): IPLDrawingFont;
begin

end;

function NewDrawingSurfaceNative(ACanvas: TCanvas): IPLDrawingSurface;
begin

end;

function NewDrawingBitmapNative(AWidth, AHeight: TPLInt): IPLDrawingBitmap;
begin

end;

end.

