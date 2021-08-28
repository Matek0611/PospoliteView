unit PospoLiteHTML.CSS.Classes;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil, Controls, strutils, math, RegExpr,
  contnrs, Forms, LMessages, BGRABitmap, BGRABitmapTypes, BGRACanvas2D, fgl,
  LCLIntf, BGRAPath, FPCanvas, PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Values,
  PospoLiteHTML.CSS.Stylesheet;

type

  { TPLCSSCustomControl }

  TPLCSSCustomControl = class(TCustomControl, IPLCSSCustomControl)
  private
    FBasePos: TPLRealPos;
    FBrowser: TPLCSSCustomControl;
    FContentEditable: boolean;
    FIsDebug: boolean;
    function GetZoom: TPLCSSFloat;
    procedure SetIsDebug(AValue: boolean);
    procedure SetZoom(AValue: TPLCSSFloat);
  protected
    FStyles: TPLCSSStyleSheet;
    FZoom: TPLCSSFloat;
    FFilename: TPLCSSString;
    {$ifdef Windows}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    {$endif}
    procedure Paint; override;
    procedure DoChangeZoom; virtual;
  public
    FPrevState, FNextState: TPLCSSString;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetParent: TPLCSSCustomControl;
    procedure UpdateStyles; virtual;
    procedure UpdateAllStyles; virtual;
    procedure FastUpdate(AObject: TPLCSSCustomControl); virtual;
    function Style(AProperty: TPLCSSString; APseudo: TPLCSSString = ''): TPLCSSString; virtual;
    procedure DrawTo(var ABitmap: TBGRABitmap); virtual;
    procedure SetStates(APrev, ANext: TPLCSSString);
    function GetRealRect: TPLRealRect;
    function GetViewport: IPLCSSCustomControl;
    function GetWindow: TCustomForm;

    property Styles: TPLCSSStyleSheet read FStyles write FStyles;
    property Browser: TPLCSSCustomControl read FBrowser write FBrowser; // if nil: self = browser
    property Zoom: TPLCSSFloat read GetZoom write SetZoom;
    property Filename: TPLCSSString read FFilename;
    property ContentEditable: boolean read FContentEditable write FContentEditable;

    procedure UpdateBasePos(AToCurrent: boolean = true; ALeft: SizeInt = 0; ATop: SizeInt = 0);
    property BasePos: TPLRealPos read FBasePos write FBasePos;

    property IsDebug: boolean read FIsDebug write SetIsDebug;
  end;

implementation

uses InterfaceBase, PospoLiteHTML.Internet;

{ TPLCSSCustomControl }

function TPLCSSCustomControl.GetZoom: TPLCSSFloat;
var
  c: TPLCSSCustomControl;
begin
  if Assigned(FBrowser) then c := FBrowser else c := self;
  Result := c.FZoom;
end;

procedure TPLCSSCustomControl.SetIsDebug(AValue: boolean);
begin
  if FIsDebug = AValue then exit;
  FIsDebug := AValue;

  Invalidate;
end;

procedure TPLCSSCustomControl.SetZoom(AValue: TPLCSSFloat);
begin
  if FZoom = AValue then exit;
  FZoom := AValue;

  Invalidate;

  DoChangeZoom;
end;

{$IFDEF Windows}
procedure TPLCSSCustomControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  Message.Result := 1; // avoid flickering when resize
end;
{$ENDIF}

procedure TPLCSSCustomControl.Paint;
begin
  inherited Paint;

  if FIsDebug then begin
    Canvas.Pen.Color := $00434343;
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width-1, Height-1);
  end;
end;

procedure TPLCSSCustomControl.DoChangeZoom;
begin
  //
end;

function TPLCSSCustomControl.GetViewport: IPLCSSCustomControl;
begin
  if Assigned(FBrowser) then Result := FBrowser else Result := Self;
end;

function TPLCSSCustomControl.GetWindow: TCustomForm;
begin
  if Assigned(FBrowser) then Result := FBrowser.Owner as TCustomForm else Result := Owner as TCustomForm;
end;

constructor TPLCSSCustomControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStyles := TPLCSSStyleSheet.Create;
  FBrowser := nil;
  FZoom := 1;
  FFilename := '';
  FIsDebug := false;
  FPrevState := '';
  FNextState := '';

  ControlStyle := ControlStyle - [csOpaque, csNoFocus] + [csParentBackground];
  ParentBackground := true;
  DoubleBuffered := WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;

  Canvas.Font.Assign(Screen.SystemFont);
end;

destructor TPLCSSCustomControl.Destroy;
begin
  FStyles.Free;

  inherited Destroy;
end;

function TPLCSSCustomControl.GetParent: TPLCSSCustomControl;
begin
  if (Parent <> nil) and (Parent is TPLCSSCustomControl) then
    Result := Parent as TPLCSSCustomControl else Result := nil;
end;

procedure TPLCSSCustomControl.UpdateStyles;
var
  i: SizeInt;
begin
  for i := 0 to ControlCount-1 do begin
    if Controls[i] is TPLCSSCustomControl then
      TPLCSSCustomControl(Controls[i]).UpdateStyles;
  end;
end;

procedure TPLCSSCustomControl.UpdateAllStyles;
begin
  //
end;

procedure TPLCSSCustomControl.FastUpdate(AObject: TPLCSSCustomControl);
begin
  //
end;

function TPLCSSCustomControl.Style(AProperty: TPLCSSString; APseudo: TPLCSSString): TPLCSSString;
begin
  Result := '';
end;

procedure TPLCSSCustomControl.DrawTo(var ABitmap: TBGRABitmap);
begin
  //
end;

procedure TPLCSSCustomControl.SetStates(APrev, ANext: TPLCSSString);
begin
  FPrevState := APrev;
  FNextState := ANext;
end;

function TPLCSSCustomControl.GetRealRect: TPLRealRect;
begin
  Result.Position.Left := Left;
  Result.Position.Top := Top;
  Result.Size.Width := Width;
  Result.Size.Height := Height;
end;

procedure TPLCSSCustomControl.UpdateBasePos(AToCurrent: boolean;
  ALeft: SizeInt; ATop: SizeInt);
begin
  if AToCurrent then begin
    FBasePos.Left := Left;
    FBasePos.Top := Top;

    exit;
  end;

  FBasePos.Left := ALeft;
  FBasePos.Top := ATop;
end;

end.

