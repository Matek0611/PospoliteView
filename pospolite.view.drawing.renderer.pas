unit Pospolite.View.Drawing.Renderer;

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
  Classes, SysUtils, Graphics, Pospolite.View.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.CSS.Basics, Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Drawer
  {$ifdef windows}, Pospolite.View.Drawing.DrawerD2D1{$endif};

type

  {$ifdef windows}
    TPLDrawingDrawerDef = TPLD2D1Drawer;
  {$else}
    TPLDrawingDrawerDef = TPLAbstractDrawer; // to do TPLNativeDrawer
  {$endif}

  { IPLDrawingRenderer }

  IPLDrawingRenderer = interface
    ['{23939734-BAA9-459A-91D0-FC34700E3833}']
    function GetDrawer: TPLDrawingDrawerDef;

    procedure DrawBox(ARect: TPLRectF; AProperties: TPLCSSDeclarations;
      AHTMLObject: TPLHTMLObject = nil; ADrawOutline: TPLBool = False;
      AFreePropertiesAfterDrawing: TPLBool = False);

    property Drawer: TPLDrawingDrawerDef read GetDrawer;
  end;

  { TPLDrawingRenderer }

  TPLDrawingRenderer = class(TInterfacedObject, IPLDrawingRenderer)
  private
    FDrawer: TPLDrawingDrawerDef;
    function GetDrawer: TPLDrawingDrawerDef;
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    procedure DrawBox(ARect: TPLRectF; AProperties: TPLCSSDeclarations;
      AHTMLObject: TPLHTMLObject = nil; ADrawOutline: TPLBool = False;
      AFreePropertiesAfterDrawing: TPLBool = False);

    property Drawer: TPLDrawingDrawerDef read GetDrawer;
  end;

  function NewDrawingRenderer(ACanvas: TCanvas): IPLDrawingRenderer;

type

  TPLDrawingRendererManager = class;

  { TPLDrawingRendererThread }

  TPLDrawingRendererThread = class(TThread)
  private
    FEnabled: TPLBool;
    FManager: TPLDrawingRendererManager;

    procedure UpdateRendering;
  public
    constructor Create(AManager: TPLDrawingRendererManager);

    procedure Execute; override;

    property Enabled: TPLBool read FEnabled write FEnabled;
  end;

  TPLDrawingRendererFPS = 1..120;

  { TPLDrawingRendererManager }

  TPLDrawingRendererManager = class
  private
    FControl: TPLCustomControl;
    FMaxFPS: TPLDrawingRendererFPS;
    FThread: TPLDrawingRendererThread;
  public
    constructor Create(AControl: TPLCustomControl);
    destructor Destroy; override;

    procedure StartRendering;
    procedure StopRendering;
    function IsRendering: TPLBool; //inline;

    property Control: TPLCustomControl read FControl write FControl;
    property MaxFPS: TPLDrawingRendererFPS read FMaxFPS write FMaxFPS default 60;
  end;

implementation

uses {$ifdef windows}Windows,{$endif} math, Dialogs;

function NewDrawingMatrix: IPLDrawingMatrix; inline;
begin
  {$ifdef windows}
    Result := NewDrawingMatrixD2D;
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingPen(ABrush: IPLDrawingBrush; AWidth: TPLFloat = 1): IPLDrawingPen; inline;
  overload;
begin
  {$ifdef windows}
    Result := NewDrawingPenD2D(ABrush, AWidth);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingPen(AColor: TPLColor; AWidth: TPLFloat = 1): IPLDrawingPen; inline;
  overload;
begin
  {$ifdef windows}
    Result := NewDrawingPenD2D(AColor, AWidth);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingSolidBrush(AColor: TPLColor): IPLDrawingBrushSolid; inline;
begin
  {$ifdef windows}
    Result := NewDrawingSolidBrushD2D(AColor);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingBitmapBrush(ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap; inline;
begin
  {$ifdef windows}
    Result := NewDrawingBitmapBrushD2D(ABitmap);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingLinearGradientBrush(
  const A, B: TPLPointF): IPLDrawingBrushGradientLinear; inline;
begin
  {$ifdef windows}
    Result := NewDrawingLinearGradientBrushD2D(A, B);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingRadialGradientBrush(
  const ARect: TPLRectF): IPLDrawingBrushGradientRadial; inline;
begin
  {$ifdef windows}
    Result := NewDrawingRadialGradientBrushD2D(ARect);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingFont(AFontData: TPLDrawingFontData): IPLDrawingFont; inline;
begin
  {$ifdef windows}
    Result := NewDrawingFontD2D(AFontData);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingSurface(ACanvas: TCanvas): IPLDrawingSurface; inline;
begin
  {$ifdef windows}
    Result := NewDrawingSurfaceD2D(ACanvas);
  {$else}
    Result := nil;
  {$endif}
end;

function NewDrawingBitmap(AWidth, AHeight: TPLInt): IPLDrawingBitmap; inline;
begin
  {$ifdef windows}
    Result := NewDrawingBitmapD2D(AWidth, AHeight);
  {$else}
    Result := nil;
  {$endif}
end;


function ColorVal(constref part: TPLCSSPropertyValuePart): TPLColor; inline;
begin
  if part is TPLCSSPropertyValuePartStringOrIdentifier then
    Result := part.AsString
  else if part is TPLCSSPropertyValuePartFunction then
    Result := TPLCSSPropertyValuePartFunction(part)
  else
    Result := TPLColor.Transparent;
end;

function ExtractCSSGradientLinear(part: TPLCSSPropertyValuePartFunction;
  ARect: TPLRectF; AHTMLObject: TPLHTMLObject = nil): IPLDrawingBrushGradientLinear;

  procedure CorrectPoint(var pt: TPLPointF);
  begin
    if pt.X < ARect.Left then pt.X := ARect.Left else
    if pt.X > ARect.Right then pt.X := ARect.Right;
    if pt.Y < ARect.Top then pt.Y := ARect.Top else
    if pt.Y > ARect.Bottom then pt.Y := ARect.Bottom;
  end;

var
  pt1, pt2, ptc: TPLPointF;
  pos, x, y, a, b, c: TPLInt;
  action: byte = 0;
  arg, arg2: TPLCSSPropertyValuePart;
  angle, d, last: TPLFloat;
  tmp: array[1..2] of TPLString = ('', '');
  pom: TPLString;
  col: TPLColor;
begin
  pos := 0;
  y := 0;
  last := 0;

  while pos < part.Arguments.Count do begin
    arg := part.Arguments[pos];

    case action of
      0: begin
        angle := 180;
        x := 0;

        if arg.AsString.ToLower = 'to' then begin
          if (pos+1 < part.Arguments.Count) and (part.Arguments[pos+1].AsString.ToLower in TPLStringFuncs.NewArray(['left', 'top', 'right', 'bottom'])) then begin
            tmp[1] := part.Arguments[pos+1].AsString.ToLower;
            x := 1;
            if (pos+2 < part.Arguments.Count) and (part.Arguments[pos+2].AsString.ToLower in TPLStringFuncs.NewArray(['left', 'top', 'right', 'bottom'])) then begin
              tmp[2] := part.Arguments[pos+2].AsString.ToLower;
              x := 2;
            end;

            if tmp[1].IsEmpty then begin
              if tmp[2].IsEmpty then tmp[1] := 'bottom' else
                TPLStringFuncs.Swap(tmp[1], tmp[2]);
            end;

            pom := tmp[1] + ',' + tmp[2];

            case pom of
              'top,': angle := 0;
              'bottom,': angle := 180;
              'left,': angle := 270;
              'right,': angle := 90;
              'top,left', 'left,top': angle := 315;
              'top,right', 'bottom,top': angle := 45;
              'bottom,left', 'left,bottom': angle := 225;
              'bottom,right', 'right,bottom': angle := 135;
            end;
          end;
        end else if arg is TPLCSSPropertyValuePartDimension then begin
          angle := AngleDeg(TPLCSSPropertyValuePartDimension(arg).Value,
            TPLCSSPropertyValuePartDimension(arg).&Unit);
        end;

        ptc := ARect.Middle;
        d := (ARect.TopLeft >< ARect.BottomRight)/2;
        pt1 := ptc - TPLPointF.Create(0, d);
        pt2 := ptc + TPLPointF.Create(0, d);
        pt1 := pt1.RotateClockwise(ptc, angle);
        pt2 := pt2.RotateClockwise(ptc, angle);
        CorrectPoint(pt1);
        CorrectPoint(pt2);

        Result := NewDrawingLinearGradientBrush(pt1, pt2);
        Result.Wrap := dbgwMirror;
        action := 1;
        if x = 0 then begin
          if arg is TPLCSSPropertyValuePartDimension then Inc(pos);
          continue;
        end;
        pos += x + 1;
        y := x;
        continue;
      end;
      1: begin
        col := arg.AsString;

        if pos+1 = part.Arguments.Count then begin
          Result.AddStop(col, 1);
          last := 1;
        end else
        if (part.Arguments[pos+1] is TPLCSSPropertyValuePartStringOrIdentifier) or (part.Arguments[pos+1] is TPLCSSPropertyValuePartFunction) then begin
          a := part.Arguments.CountTypes([TPLCSSPropertyValuePartStringOrIdentifier, TPLCSSPropertyValuePartFunction], y)-1;
          if a > 0 then begin
            b := part.Arguments.CountTypes([TPLCSSPropertyValuePartStringOrIdentifier, TPLCSSPropertyValuePartFunction], pos+1);
            c := a - b;
            if (c < 0) then c := 0;
            d := c / a;
            Result.AddStop(col, d);
            last := d;
          end;
        end else begin
          x := 1;

          while (pos+x < part.Arguments.Count) and not ((part.Arguments[pos+x] is TPLCSSPropertyValuePartStringOrIdentifier) or (part.Arguments[pos+x] is TPLCSSPropertyValuePartFunction)) do begin
            d := 0;
            arg2:= part.Arguments[pos+x];

            if arg2 is TPLCSSPropertyValuePartDimension then begin
              if  TPLCSSPropertyValuePartDimension(arg2).&Unit = '%' then
                d := TPLCSSPropertyValuePartDimension(arg2).Value
              else
                d := AutoLengthToPx(TPLCSSPropertyValuePartDimension(arg2).Value, TPLCSSPropertyValuePartDimension(arg2).&Unit, AHTMLObject) / max(ARect.Width, ARect.Height) * 100;
            end
            else if arg2 is TPLCSSPropertyValuePartNumber then
              d := TPLCSSPropertyValuePartNumber(arg2).Value
            else if pos+x+1 = part.Arguments.Count then d := 100;

            d *= 0.01;

            Result.AddStop(col, d);
            last := d;

            Inc(x);
          end;

          pos += x;
          continue;
        end;
      end;
    end;

    Inc(pos);
  end;
end;

function ExtractCSSBackground(AStyles: TPLCSSDeclarations; APart: TPLString;
  ARect: TPLRectF; AHTMLObject: TPLHTMLObject = nil; AIndex: TPLInt = 0): IPLDrawingBrush; inline;
var
  prop: TPLCSSProperty;
  part: TPLCSSPropertyValuePartFunction;
  i: TPLInt absolute AIndex;
begin
  Result := NewDrawingSolidBrush(TPLColor.Transparent);

  if (APart = 'color') and AStyles.Exists('background-color', prop) and (prop.Value.Count > 0) and Assigned(prop.Value[i]) then begin
    Result := NewDrawingSolidBrush(ColorVal(prop.Value[0]));
  end else
  if (APart = 'image') and AStyles.Exists('background-image', prop) and (prop.Value.Count > 0) and Assigned(prop.Value[i]) and (prop.Value[i] is TPLCSSPropertyValuePartFunction) then begin
    part := prop.Value[i] as TPLCSSPropertyValuePartFunction;

    case part.Name.ToLower of
      'linear-gradient': Result := ExtractCSSGradientLinear(part, ARect, AHTMLObject);
      //'repeating-linear-gradient': Result := ExtractCSSGradientLinearRepeating(part, ARect);
      //'url': Result := NewDrawingBitmapBrush(ExtractCSSImage(part, ARect));
    end;
  end;
end;

// border and outline
function ExtractCSSBorder(AStyles: TPLCSSDeclarations; AHTMLObject: TPLHTMLObject = nil; AType: TPLString = 'border'): TPLDrawingBorders;

  function GetLVal(a: TPLCSSPropertyValuePart; v: TPLString = ''; id: TPLInt = 0): TPLFloat;
  begin
    if a is TPLCSSPropertyValuePartNumber then Result := TPLCSSPropertyValuePartNumber(a).Value else
    if a is TPLCSSPropertyValuePartDimension then Result := AutoLengthToPx(TPLCSSPropertyValuePartDimension(a).Value, TPLCSSPropertyValuePartDimension(a).&Unit, AHTMLObject) else
    if a is TPLCSSPropertyValuePartStringOrIdentifier then
      case a.AsString.ToLower of
        'thin': Result := AutoLengthToPx(1, 'px', AHTMLObject);
        'medium': Result := AutoLengthToPx(3, 'px', AHTMLObject);
        'thick': Result := AutoLengthToPx(5, 'px', AHTMLObject);
        'inherit': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InheritValueOf(v, id) else Result := 1;
        'initial': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InitialValueOf(v, id) else Result := 1;
        'unset': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_UnsetValueOf(v, id) else Result := 1;
        'revert': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_RevertValueOf(v, id) else Result := 1;
      end
    else Result := 0;

    if Result < 0 then Result := 0;
  end;

  function GetSVal(a: TPLCSSPropertyValuePart; v: TPLString = ''; id: TPLInt = 0): TPLString;
  begin
    case a.AsString.ToLower of
      'inherit': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InheritValueOf(v, id) else Result := 'none';
      'initial': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InitialValueOf(v, id) else Result := 'none';
      'unset': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_UnsetValueOf(v, id) else Result := 'none';
      'revert': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_RevertValueOf(v, id) else Result := 'none';
      else Result := a.AsString;
    end;
  end;

  function GetCVal(a: TPLCSSPropertyValuePart; v: TPLString = ''; id: TPLInt = 0): TPLColor;
  begin
    if a is TPLCSSPropertyValuePartStringOrIdentifier then
      case a.AsString.ToLower of
        'inherit': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InheritValueOf(v, id) else Result := TPLColor.Black;
        'initial': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InitialValueOf(v, id) else Result := TPLColor.Black;
        'unset': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_UnsetValueOf(v, id) else Result := TPLColor.Black;
        'revert': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_RevertValueOf(v, id) else Result := TPLColor.Black;
        else Result := a.AsString;
      end
    else if a is TPLCSSPropertyValuePartFunction then
      Result := TPLCSSPropertyValuePartFunction(a)
    else
      Result := TPLColor.Transparent;
  end;

  function GetBIWVal(a: TPLCSSPropertyValuePart; b: TPLFloat; v: TPLString = ''; id: TPLInt = 0): TPLFloat;
  begin
    if a is TPLCSSPropertyValuePartNumber then Result := TPLCSSPropertyValuePartNumber(a).Value * b
    else if a is TPLCSSPropertyValuePartDimension then Result := AutoLengthToPx(TPLCSSPropertyValuePartDimension(a).Value, TPLCSSPropertyValuePartDimension(a).&Unit, AHTMLObject)
    else if a is TPLCSSPropertyValuePartStringOrIdentifier then
      case a.AsString.ToLower of
        'auto': Result := b;
        'inherit': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InheritValueOf(v, id) else Result := b;
        'initial': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_InitialValueOf(v, id) else Result := b;
        'unset': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_UnsetValueOf(v, id) else Result := b;
        'revert': if Assigned(AHTMLObject) then Result := AHTMLObject.CSS_RevertValueOf(v, id) else Result := b;
      end
    else Result := 0;

    if Result < 0 then Result := 0;
  end;

var
  prop: TPLCSSProperty;
  part: TPLCSSPropertyValuePartFunction;
  pom: array of Variant;
begin
  if AType <> 'outline' then
    Result := PLDrawingBordersDef
  else
    Result := PLDrawingBordersOutlineDef;

  if AStyles.Exists(AType, prop) then begin
    case prop.Value.Count of
      1: begin

      end;
      2: begin

      end;
      3: begin

      end;
    end;
  end;

  if AStyles.Exists(AType + '-radius', prop) then begin
    case prop.Value.Count of
      1: Result.Radius := PLDrawingBordersRadiusData(GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[0], AType + '-radius'));
      2: Result.Radius := PLDrawingBordersRadiusData(GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[1], AType + '-radius', 1), GetLVal(prop.Value[1], AType + '-radius', 1), GetLVal(prop.Value[0], AType + '-radius'));
      3: Result.Radius := PLDrawingBordersRadiusData(GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[1], AType + '-radius', 1), GetLVal(prop.Value[1], AType + '-radius', 1), GetLVal(prop.Value[2], AType + '-radius', 2));
      4: Result.Radius := PLDrawingBordersRadiusData(GetLVal(prop.Value[0], AType + '-radius'), GetLVal(prop.Value[1], AType + '-radius', 1), GetLVal(prop.Value[3], AType + '-radius', 3), GetLVal(prop.Value[2], AType + '-radius', 2));
    end;
  end;

  if AStyles.Exists(AType + '-top-left-radius', prop) then begin
    if prop.Value.Count > 0 then Result.Radius[1] := GetLVal(prop.Value[0], AType + '-top-left-radius');
  end;
  if AStyles.Exists(AType + '-top-right-radius', prop) then begin
    if prop.Value.Count > 0 then Result.Radius[2] := GetLVal(prop.Value[0], AType + '-top-right-radius');
  end;
  if AStyles.Exists(AType + '-bottom-left-radius', prop) then begin
    if prop.Value.Count > 0 then Result.Radius[3] := GetLVal(prop.Value[0], AType + '-bottom-left-radius');
  end;
  if AStyles.Exists(AType + '-bottom-right-radius', prop) then begin
    if prop.Value.Count > 0 then Result.Radius[4] := GetLVal(prop.Value[0], AType + '-bottom-right-radius');
  end;

  if AStyles.Exists(AType + '-width', prop) then begin
    case prop.Value.Count of
      1: begin
        Result.Left.Width := GetLVal(prop.Value[0], AType + '-width');
        Result.Top.Width := Result.Left.Width;
        Result.Right.Width := Result.Left.Width;
        Result.Bottom.Width := Result.Left.Width;
      end;
      2: begin
        Result.Left.Width := GetLVal(prop.Value[0], AType + '-width');
        Result.Top.Width := GetLVal(prop.Value[1], AType + '-width', 1);
        Result.Right.Width := Result.Left.Width;
        Result.Bottom.Width := Result.Top.Width;
      end;
      3: begin
        Result.Left.Width := GetLVal(prop.Value[1], AType + '-width', 1);
        Result.Top.Width := GetLVal(prop.Value[0], AType + '-width');
        Result.Right.Width := Result.Left.Width;
        Result.Bottom.Width := GetLVal(prop.Value[2], AType + '-width', 2);
      end;
      4: begin
        Result.Left.Width := GetLVal(prop.Value[3], AType + '-width', 3);
        Result.Top.Width := GetLVal(prop.Value[0], AType + '-width');
        Result.Right.Width := GetLVal(prop.Value[1], AType + '-width', 1);
        Result.Bottom.Width := GetLVal(prop.Value[2], AType + '-width', 2);
      end;
    end;
  end;

  if AStyles.Exists(AType + '-left-width', prop) then begin
    if prop.Value.Count > 0 then Result.Left.Width := GetLVal(prop.Value[0], AType + '-left-width');
  end;
  if AStyles.Exists(AType + '-right-width', prop) then begin
    if prop.Value.Count > 0 then Result.Right.Width := GetLVal(prop.Value[0], AType + '-right-width');
  end;
  if AStyles.Exists(AType + '-top-width', prop) then begin
    if prop.Value.Count > 0 then Result.Top.Width := GetLVal(prop.Value[0], AType + '-top-width');
  end;
  if AStyles.Exists(AType + '-bottom-width', prop) then begin
    if prop.Value.Count > 0 then Result.Bottom.Width := GetLVal(prop.Value[0], AType + '-bottom-width');
  end;

  if AStyles.Exists(AType + '-style', prop) then begin
    case prop.Value.Count of
      1: begin
        Result.Top.Style := GetSVal(prop.Value[0], AType + '-style');
        Result.Right.Style := Result.Top.Style;
        Result.Bottom.Style := Result.Top.Style;
        Result.Left.Style := Result.Top.Style;
      end;
      2: begin
        Result.Top.Style := GetSVal(prop.Value[0], AType + '-style');
        Result.Right.Style := GetSVal(prop.Value[1], AType + '-style', 1);
        Result.Bottom.Style := Result.Top.Style;
        Result.Left.Style := Result.Right.Style;
      end;
      3: begin
        Result.Top.Style := GetSVal(prop.Value[0], AType + '-style');
        Result.Right.Style := GetSVal(prop.Value[1], AType + '-style', 1);
        Result.Bottom.Style := GetSVal(prop.Value[2], AType + '-style', 2);
        Result.Left.Style := Result.Right.Style;
      end;
      4: begin
        Result.Top.Style := GetSVal(prop.Value[0], AType + '-style');
        Result.Right.Style := GetSVal(prop.Value[1], AType + '-style', 1);
        Result.Bottom.Style := GetSVal(prop.Value[2], AType + '-style', 2);
        Result.Left.Style := GetSVal(prop.Value[3], AType + '-style', 3);
      end;
    end;
  end;

  if AStyles.Exists(AType + '-left-style', prop) then begin
    if prop.Value.Count > 0 then Result.Left.Style := GetSVal(prop.Value[0], AType + '-left-style');
  end;
  if AStyles.Exists(AType + '-right-style', prop) then begin
    if prop.Value.Count > 0 then Result.Right.Style := GetSVal(prop.Value[0], AType + '-right-style');
  end;
  if AStyles.Exists(AType + '-top-style', prop) then begin
    if prop.Value.Count > 0 then Result.Top.Style := GetSVal(prop.Value[0], AType + '-top-style');
  end;
  if AStyles.Exists(AType + '-bottom-style', prop) then begin
    if prop.Value.Count > 0 then Result.Bottom.Style := GetSVal(prop.Value[0], AType + '-bottom-style');
  end;

  if AStyles.Exists(AType + '-color', prop) then begin
    case prop.Value.Count of
      1: begin
        Result.Top.Color := GetCVal(prop.Value[0], AType + '-color');
        Result.Right.Color := Result.Top.Color;
        Result.Bottom.Color := Result.Top.Color;
        Result.Left.Color := Result.Top.Color;
      end;
      2: begin
        Result.Top.Color := GetCVal(prop.Value[0], AType + '-color');
        Result.Right.Color := GetCVal(prop.Value[1], AType + '-color', 1);
        Result.Bottom.Color := Result.Top.Color;
        Result.Left.Color := Result.Right.Color;
      end;
      3: begin
        Result.Top.Color := GetCVal(prop.Value[0], AType + '-color');
        Result.Right.Color := GetCVal(prop.Value[1], AType + '-color', 1);
        Result.Bottom.Color := GetCVal(prop.Value[2], AType + '-color', 2);
        Result.Left.Color := Result.Right.Color;
      end;
      4: begin
        Result.Top.Color := GetCVal(prop.Value[0], AType + '-color');
        Result.Right.Color := GetCVal(prop.Value[1], AType + '-color', 1);
        Result.Bottom.Color := GetCVal(prop.Value[2], AType + '-color', 2);
        Result.Left.Color := GetCVal(prop.Value[3], AType + '-color', 3);
      end;
    end;
  end;

  if AStyles.Exists(AType + '-left-color', prop) then begin
    if prop.Value.Count > 0 then Result.Left.Color := GetCVal(prop.Value[0], AType + '-left-color');
  end;
  if AStyles.Exists(AType + '-right-color', prop) then begin
    if prop.Value.Count > 0 then Result.Right.Color := GetCVal(prop.Value[0], AType + '-right-color');
  end;
  if AStyles.Exists(AType + '-top-color', prop) then begin
    if prop.Value.Count > 0 then Result.Top.Color := GetCVal(prop.Value[0], AType + '-top-color');
  end;
  if AStyles.Exists(AType + '-bottom-color', prop) then begin
    if prop.Value.Count > 0 then Result.Bottom.Color := GetCVal(prop.Value[0], AType + '-bottom-color');
  end;

  // basic border-image support:

  if AStyles.Exists(AType + '-image', prop) then begin
    if (prop.Value.Count > 0) and (prop.Value[0] is TPLCSSPropertyValuePartFunction) then begin
      part := prop.Value[0] as TPLCSSPropertyValuePartFunction;

      // dorobić width

      case part.Name.ToLower of
        'linear-gradient': Result.Image := ExtractCSSGradientLinear(part, TPLRectF.Create(0, 0, 10, 10), AHTMLObject);
        else Result.Image := nil;
      end;
    end;
  end;

  SetLength(pom, 3);
  pom[0] := 1;
  pom[1] := 1;
  pom[2] := false;

  if AStyles.Exists(AType + '-image-width', prop) then begin
    case prop.Value.Count of
      1: begin
        pom[0] := GetBIWVal(prop.Value[0], Result.AverageBorderSize, AType + '-image-width');
        pom[1] := pom[0];
      end;
      2: begin
        pom[0] := GetBIWVal(prop.Value[0], Result.AverageBorderSize, AType + '-image-width');
        pom[1] := GetBIWVal(prop.Value[1], Result.AverageBorderSize, AType + '-image-width', 1);
      end;
    end;
  end;

  // border-image-slice & border-image-outset ignored, border-image-repeat always = repeat

  if AStyles.Exists(AType + '-image-source', prop) then begin
    if (prop.Value.Count > 0) and (prop.Value[0] is TPLCSSPropertyValuePartFunction) then pom[2] := true;
  end;

  if pom[2] = true then begin
    part := prop.Value[0] as TPLCSSPropertyValuePartFunction;

    case part.Name.ToLower of
      'linear-gradient': Result.Image := ExtractCSSGradientLinear(part, TPLRectF.Create(0, 0, pom[0], pom[1]), AHTMLObject);
    end;
  end;      // co gdy border: image ...; bez width a border-image-width jest ustawione???
end;

{ TPLDrawingRenderer }

function TPLDrawingRenderer.GetDrawer: TPLDrawingDrawerDef;
begin
  Result := FDrawer;
end;

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
  AProperties: TPLCSSDeclarations; AHTMLObject: TPLHTMLObject;
  ADrawOutline: TPLBool; AFreePropertiesAfterDrawing: TPLBool);
var
  brd, brdn: TPLDrawingBorders;
  i: TPLInt;
  prop: TPLCSSProperty;
  v: TPLFloat;
begin
  brd := ExtractCSSBorder(AProperties, AHTMLObject);
  brdn := brd;
  brdn.Left.Width := 0;
  brdn.Top.Width := 0;
  brdn.Right.Width := 0;
  brdn.Bottom.Width := 0;

  FDrawer.DrawBox(ARect, ExtractCSSBackground(AProperties, 'color', ARect), brdn);
  if AProperties.Exists('background-image', prop) then begin
    for i := 0 to prop.Value.Count-1 do
      FDrawer.DrawBox(ARect, ExtractCSSBackground(AProperties, 'image', ARect, AHTMLObject, i), brdn);
  end;

  FDrawer.DrawBox(ARect, nil, brd);

  if ADrawOutline then begin
    if AProperties.Exists('outline-offset', prop) then begin
      if prop.Value.Count > 0 then begin
        v := 0;
        if prop.Value[0] is TPLCSSPropertyValuePartNumber then v := TPLCSSPropertyValuePartNumber(prop.Value[0]).Value
        else if prop.Value[0] is TPLCSSPropertyValuePartDimension then v := AutoLengthToPx(TPLCSSPropertyValuePartDimension(prop.Value[0]).Value, TPLCSSPropertyValuePartDimension(prop.Value[0]).&Unit);
        ARect := ARect.Inflate(-v, -v);
      end;
    end;
    ARect := ARect.Inflate(-brd.AverageBorderSize, -brd.AverageBorderSize);
    FDrawer.DrawBox(ARect, nil, ExtractCSSBorder(AProperties, AHTMLObject, 'outline'));
  end;

  if AFreePropertiesAfterDrawing then AProperties.Free;
end;

function NewDrawingRenderer(ACanvas: TCanvas): IPLDrawingRenderer;
begin
  Result := TPLDrawingRenderer.Create(ACanvas);
end;

{ TPLDrawingRendererThread }

procedure TPLDrawingRendererThread.UpdateRendering;
begin
  if Assigned(FManager) and Assigned(FManager.FControl) then
    FManager.FControl.Invalidate;
end;

constructor TPLDrawingRendererThread.Create(AManager: TPLDrawingRendererManager
  );
begin
  inherited Create(true);

  FManager := AManager;
  Suspended := true;
  FreeOnTerminate := false;
  FEnabled := false;
end;

procedure TPLDrawingRendererThread.Execute;
var
  delay: Cardinal;
begin
  delay := round(1000 / FManager.FMaxFPS);

  while FEnabled do begin
    Synchronize(@UpdateRendering);

    Sleep(delay);
  end;
end;

{ TPLDrawingRendererManager }

constructor TPLDrawingRendererManager.Create(AControl: TPLCustomControl);
begin
  inherited Create;

  FControl := AControl;
  FMaxFPS := 30;
  FThread := TPLDrawingRendererThread.Create(self);
end;

destructor TPLDrawingRendererManager.Destroy;
begin
  FThread.Enabled := false;
  FThread.Terminate;
  FThread.Free;

  inherited Destroy;
end;

procedure TPLDrawingRendererManager.StartRendering;
begin
  FThread.Enabled := true;
  FThread.Start;
end;

procedure TPLDrawingRendererManager.StopRendering;
begin
  FThread.Enabled := false;
  FThread.Suspended := true;
end;

function TPLDrawingRendererManager.IsRendering: TPLBool;
begin
  Result := not FThread.Finished and not FThread.Suspended;
end;

end.

