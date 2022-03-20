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
  Pospolite.View.CSS.Basics, Pospolite.View.Drawing.Basics, Pospolite.View.Drawing.Drawer,
  Pospolite.View.Drawing.NativeDrawer
  {$ifdef windows}, Pospolite.View.Drawing.DrawerD2D1{$endif};

type

  { IPLDrawingDrawerFactory }

  IPLDrawingDrawerFactory = interface
    ['{8D3CFB7F-A8BF-4204-A5F0-8167F100C1DF}']

    function NewMatrix: IPLDrawingMatrix;
    function NewFont(const AFontData: TPLDrawingFontData): IPLDrawingFont;
    function NewPen(const ABrush: IPLDrawingBrush; const AWidth: TPLFloat = 1): IPLDrawingPen; overload;
    function NewPen(const AColor: TPLColor; const AWidth: TPLFloat = 1): IPLDrawingPen; overload;
    function NewBrushSolid(const AColor: TPLColor): IPLDrawingBrushSolid;
    function NewBrushBitmap(const ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
    function NewBrushGradientLinear(const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
    function NewBrushGradientRadial(const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
    function NewBitmap(const AWidth, AHeight: TPLInt): IPLDrawingBitmap;
    function NewSurface(const ACanvas: TCanvas): IPLDrawingSurface;
  end;

  { TPLDrawingDrawerCustomFactory }

  TPLDrawingDrawerCustomFactory = class(TInterfacedObject, IPLDrawingDrawerFactory)
  public
    function NewMatrix: IPLDrawingMatrix; virtual; abstract;
    function NewFont(const AFontData: TPLDrawingFontData): IPLDrawingFont; virtual; abstract;
    function NewPen(const ABrush: IPLDrawingBrush; const AWidth: TPLFloat = 1): IPLDrawingPen; overload; virtual; abstract;
    function NewPen(const AColor: TPLColor; const AWidth: TPLFloat = 1): IPLDrawingPen; overload; virtual; abstract;
    function NewBrushSolid(const AColor: TPLColor): IPLDrawingBrushSolid; virtual; abstract;
    function NewBrushBitmap(const ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap; virtual; abstract;
    function NewBrushGradientLinear(const A, B: TPLPointF): IPLDrawingBrushGradientLinear; virtual; abstract;
    function NewBrushGradientRadial(const ARect: TPLRectF): IPLDrawingBrushGradientRadial; virtual; abstract;
    function NewBitmap(const AWidth, AHeight: TPLInt): IPLDrawingBitmap; virtual; abstract;
    function NewSurface(const ACanvas: TCanvas): IPLDrawingSurface; virtual; abstract;
  end;

  TPLDrawingDrawerClass = class of TPLAbstractDrawer;
  TPLDrawingDrawerKind = (
      dkAbstract, dkNative,
      {$ifdef windows}dkDirect2D, {$endif}
      dkCustom
    );

const
  dkDefault = {$ifdef windows}dkDirect2D{$else}dkNative{$endif};

var
  PLDrawerKind: TPLDrawingDrawerKind = dkDefault;
  PLDrawerCustomClass: TPLDrawingDrawerClass = TPLAbstractDrawer;
  PLDrawerCustomFactoryInstance: TPLDrawingDrawerCustomFactory = nil;

type

  { IPLDrawingRenderer }

  IPLDrawingRenderer = interface(IPLDrawingDrawerFactory)
    ['{23939734-BAA9-459A-91D0-FC34700E3833}']
    function GetDebugMode: boolean;
    function GetDrawer: TPLAbstractDrawer;
    procedure SetDebugMode(AValue: boolean);

    procedure DrawHTMLObject(const AHTMLObject: TPLHTMLObject);

    property Drawer: TPLAbstractDrawer read GetDrawer;
    property DebugMode: boolean read GetDebugMode write SetDebugMode;
  end;

  { TPLDrawingRenderer }

  TPLDrawingRenderer = class(TInterfacedObject, IPLDrawingRenderer)
  private
    FDrawer: TPLAbstractDrawer;
    FDebugMode: boolean;
    function GetDebugMode: boolean;
    function GetDrawer: TPLAbstractDrawer;
    procedure SetDebugMode(AValue: boolean);
  public
    constructor Create(ACanvas: TCanvas);
    destructor Destroy; override;

    procedure DrawHTMLObject(const AHTMLObject: TPLHTMLObject);

    function NewMatrix: IPLDrawingMatrix;
    function NewFont(const AFontData: TPLDrawingFontData): IPLDrawingFont;
    function NewPen(const ABrush: IPLDrawingBrush; const AWidth: TPLFloat = 1): IPLDrawingPen; overload;
    function NewPen(const AColor: TPLColor; const AWidth: TPLFloat = 1): IPLDrawingPen; overload;
    function NewBrushSolid(const AColor: TPLColor): IPLDrawingBrushSolid;
    function NewBrushBitmap(const ABitmap: IPLDrawingBitmap): IPLDrawingBrushBitmap;
    function NewBrushGradientLinear(const A, B: TPLPointF): IPLDrawingBrushGradientLinear;
    function NewBrushGradientRadial(const ARect: TPLRectF): IPLDrawingBrushGradientRadial;
    function NewBitmap(const AWidth, AHeight: TPLInt): IPLDrawingBitmap;
    function NewSurface(const ACanvas: TCanvas): IPLDrawingSurface;

    property Drawer: TPLAbstractDrawer read GetDrawer;
    property DebugMode: boolean read GetDebugMode write SetDebugMode;
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
    FRenderingFlag: TPLBool;
    FThread: TPLDrawingRendererThread;
    FCS: TRTLCriticalSection;
    procedure SetRenderingFlag(AValue: TPLBool);
  public
    constructor Create(AControl: TPLCustomControl);
    destructor Destroy; override;

    procedure StartRendering;
    procedure StopRendering;
    function IsRendering: TPLBool; //inline;

    property Control: TPLCustomControl read FControl write FControl;
    property MaxFPS: TPLDrawingRendererFPS read FMaxFPS write FMaxFPS default 60;
    property RenderingFlag: TPLBool read FRenderingFlag write SetRenderingFlag;
  end;

implementation

uses {$ifdef windows}Windows,{$endif} math, Dialogs, Forms;

function NewDrawingRenderer(ACanvas: TCanvas): IPLDrawingRenderer;
begin
  Result := TPLDrawingRenderer.Create(ACanvas);
end;

{ TPLDrawingRenderer }

function TPLDrawingRenderer.GetDrawer: TPLAbstractDrawer;
begin
  Result := FDrawer;
end;

function TPLDrawingRenderer.GetDebugMode: boolean;
begin
  Result := FDebugMode;
end;

procedure TPLDrawingRenderer.SetDebugMode(AValue: boolean);
begin
  if FDebugMode = AValue then exit;

  FDebugMode := AValue;
end;

constructor TPLDrawingRenderer.Create(ACanvas: TCanvas);
begin
  inherited Create;

  case PLDrawerKind of
    dkAbstract: FDrawer := TPLAbstractDrawer.Create(ACanvas);
    dkNative: FDrawer := TPLNativeDrawer.Create(ACanvas);
    {$ifdef windows}
    dkDirect2D: FDrawer := TPLD2D1Drawer.Create(ACanvas);
    {$endif}
    dkCustom: FDrawer := PLDrawerCustomClass.Create(ACanvas);
  end;

  FDebugMode := false;
end;

destructor TPLDrawingRenderer.Destroy;
begin
  FDrawer.Free;

  inherited Destroy;
end;

procedure TPLDrawingRenderer.DrawHTMLObject(const AHTMLObject: TPLHTMLObject);
begin
  FDrawer.DrawObjectFully(AHTMLObject, FDebugMode);
end;

function TPLDrawingRenderer.NewMatrix: IPLDrawingMatrix;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingMatrixD2D;{$endif}
    dkNative: Result := NewDrawingMatrixNative;
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewMatrix
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewFont(const AFontData: TPLDrawingFontData
  ): IPLDrawingFont;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingFontD2D(AFontData);{$endif}
    dkNative: Result := NewDrawingFontNative(AFontData);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewFont(AFontData)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewPen(const ABrush: IPLDrawingBrush;
  const AWidth: TPLFloat): IPLDrawingPen;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingPenD2D(ABrush, AWidth);{$endif}
    dkNative: Result := NewDrawingPenNative(ABrush, AWidth);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewPen(ABrush, AWidth)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewPen(const AColor: TPLColor;
  const AWidth: TPLFloat): IPLDrawingPen;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingPenD2D(AColor, AWidth);{$endif}
    dkNative: Result := NewDrawingPenNative(AColor, AWidth);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewPen(AColor, AWidth)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewBrushSolid(const AColor: TPLColor
  ): IPLDrawingBrushSolid;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingSolidBrushD2D(AColor);{$endif}
    dkNative: Result := NewDrawingSolidBrushNative(AColor);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewBrushSolid(AColor)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewBrushBitmap(const ABitmap: IPLDrawingBitmap
  ): IPLDrawingBrushBitmap;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingBitmapBrushD2D(ABitmap);{$endif}
    dkNative: Result := NewDrawingBitmapBrushNative(ABitmap);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewBrushBitmap(ABitmap)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewBrushGradientLinear(const A, B: TPLPointF
  ): IPLDrawingBrushGradientLinear;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingLinearGradientBrushD2D(A, B);{$endif}
    dkNative: Result := NewDrawingLinearGradientBrushNative(A, B);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewBrushGradientLinear(A, B)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewBrushGradientRadial(const ARect: TPLRectF
  ): IPLDrawingBrushGradientRadial;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingRadialGradientBrushD2D(ARect);{$endif}
    dkNative: Result := NewDrawingRadialGradientBrushNative(ARect);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewBrushGradientRadial(ARect)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewBitmap(const AWidth, AHeight: TPLInt
  ): IPLDrawingBitmap;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingBitmapD2D(AWidth, AHeight);{$endif}
    dkNative: Result := NewDrawingBitmapNative(AWidth, AHeight);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewBitmap(AWidth, AHeight)
      else Result := nil;
    else Result := nil;
  end;
end;

function TPLDrawingRenderer.NewSurface(const ACanvas: TCanvas
  ): IPLDrawingSurface;
begin
  case PLDrawerKind of
    {$ifdef windows}dkDirect2D: Result := NewDrawingSurfaceD2D(ACanvas);{$endif}
    dkNative: Result := NewDrawingSurfaceNative(ACanvas);
    dkCustom:
      if Assigned(PLDrawerCustomFactoryInstance) then
        Result := PLDrawerCustomFactoryInstance.NewSurface(ACanvas)
      else Result := nil;
    else Result := nil;
  end;
end;

{ TPLDrawingRendererThread }

procedure TPLDrawingRendererThread.UpdateRendering;
begin
  if Assigned(FManager) and Assigned(FManager.FControl) and FManager.RenderingFlag then begin
    FManager.FControl.Repaint;
  end;
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

  while FEnabled and not Suspended and not Terminated do begin
    if not FManager.Control.IsResizing then begin

      FManager.RenderingFlag := not FManager.RenderingFlag;

      if FManager.RenderingFlag then UpdateRendering
      else FManager.FControl.Redraw;
    end;

    if not FEnabled or Suspended or Terminated then break;

    Sleep(delay);
  end;
end;

{ TPLDrawingRendererManager }

procedure TPLDrawingRendererManager.SetRenderingFlag(AValue: TPLBool);
begin
  if FRenderingFlag = AValue then exit;

  if TryEnterCriticalSection(FCS) then
  try
    FRenderingFlag := AValue;
  finally
    LeaveCriticalSection(FCS);
  end;
end;

constructor TPLDrawingRendererManager.Create(AControl: TPLCustomControl);
begin
  inherited Create;

  InitializeCriticalSection(FCS);
  FRenderingFlag := false;

  FControl := AControl;
  FMaxFPS := 30;
  FThread := TPLDrawingRendererThread.Create(self);
end;

destructor TPLDrawingRendererManager.Destroy;
begin
  DeleteCriticalSection(FCS);

  FThread.Enabled := false;
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

