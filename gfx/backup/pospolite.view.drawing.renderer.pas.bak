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

  TPLDrawingDrawerClass = class of TPLAbstractDrawer;

  { IPLDrawingRenderer }

  IPLDrawingRenderer = interface
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

  FDrawer :=
    {$ifdef windows}
      TPLD2D1Drawer
    {$else}
      TPLAbstractDrawer // to do TPLNativeDrawer
    {$endif}.Create(ACanvas);
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

