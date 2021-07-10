unit Pospolite.View.HTML.Basics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.CSS.Declaration;

type

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TInterfacedObject, IPLHTMLObject)
  private
    FParent: TPLHTMLBasicObject;
    FRenderer: TPLDrawingRenderer;
    FJSObject: IPLJSBasicObject;
    FState: TPLCSSElementState;
    FZoom: TPLFloat;
    FStates: array[TPLCSSElementState] of TPLCSSDeclarations;
    FAttributes: IPLHTMLObjectAttributes;
    FChildren: IPLHTMLObjects;
    FName: TPLString;
    FText: TPLString;
    FPosition: SizeInt;
    function GetAttributes: IPLHTMLObjectAttributes;
    function GetChild(const AName: TPLString): IPLHTMLObject;
    function GetChildren: IPLHTMLObjects;
    function GetJSObject: IPLJSBasicObject;
    function GetName: TPLString;
    function GetParent: TPLHTMLBasicObject;
    function GetPosition: SizeInt;
    function GetState: TPLCSSElementState;
    function GetText: TPLString;
    function GetZoom: TPLFloat;
    procedure SetName(AValue: TPLString);
    procedure SetParent(AValue: TPLHTMLBasicObject);
    procedure SetPosition(AValue: SizeInt);
    procedure SetState(AValue: TPLCSSElementState);
    procedure SetText(AValue: TPLString);
    procedure SetZoom(AValue: TPLFloat);
    procedure InitStates;
    procedure DoneStates;
  protected
    procedure DoDraw;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
    destructor Destroy; override;

    function Clone: IPLHTMLObject; virtual;

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_Get(APropName: TPLString): Pointer;
    procedure CSS_Set(APropName: TPLString; const APropValue);

    procedure UpdateScrollbars;
    procedure Draw;
    function ToHTML: TPLString; virtual;

    function IsVisible: TPLBool;
    function Display: TPLString;

    property Renderer: TPLDrawingRenderer read FRenderer;
    property Zoom: TPLFloat read GetZoom write SetZoom;
    property State: TPLCSSElementState read GetState write SetState;
    property JSObject: IPLJSBasicObject read GetJSObject;
    property Attributes: IPLHTMLObjectAttributes read GetAttributes;
    property Parent: TPLHTMLBasicObject read GetParent write SetParent;
    property Children: IPLHTMLObjects read GetChildren;
    property Child[const AName: TPLString]: IPLHTMLObject read GetChild;
    property Name: TPLString read GetName write SetName;
    property Text: TPLString read GetText write SetText;
    property Position: SizeInt read GetPosition write SetPosition;
  end;

implementation

{ TPLHTMLBasicObject }

function TPLHTMLBasicObject.GetAttributes: IPLHTMLObjectAttributes;
begin
  Result := FAttributes;
end;

function TPLHTMLBasicObject.GetChild(const AName: TPLString): IPLHTMLObject;
begin

end;

function TPLHTMLBasicObject.GetChildren: IPLHTMLObjects;
begin
  Result := FChildren;
end;

function TPLHTMLBasicObject.GetJSObject: IPLJSBasicObject;
begin
  Result := FJSObject;
end;

function TPLHTMLBasicObject.GetName: TPLString;
begin
  Result := FName;
end;

function TPLHTMLBasicObject.GetParent: TPLHTMLBasicObject;
begin
  Result := FParent;
end;

function TPLHTMLBasicObject.GetPosition: SizeInt;
begin
  Result := FPosition;
end;

function TPLHTMLBasicObject.GetState: TPLCSSElementState;
begin
  Result := FState;
end;

function TPLHTMLBasicObject.GetText: TPLString;
begin
  Result := FText;
end;

function TPLHTMLBasicObject.GetZoom: TPLFloat;
begin
  Result := FZoom;
end;

procedure TPLHTMLBasicObject.SetName(AValue: TPLString);
begin
  AValue := AValue.Trim.ToLower;
  if (FName = AValue) or (AValue.IsEmpty) then exit;
  FName := AValue;
end;

procedure TPLHTMLBasicObject.SetParent(AValue: TPLHTMLBasicObject);
begin
  FParent := AValue;
end;

procedure TPLHTMLBasicObject.SetPosition(AValue: SizeInt);
begin
  FPosition := AValue;
end;

procedure TPLHTMLBasicObject.SetState(AValue: TPLCSSElementState);
begin
  FState := AValue;
end;

procedure TPLHTMLBasicObject.SetText(AValue: TPLString);
begin
  FText := AValue;
end;

procedure TPLHTMLBasicObject.SetZoom(AValue: TPLFloat);
begin
  if (AValue = FZoom) or (AValue <= 0) or (AValue > 10) then exit;

  FZoom := AValue;
end;

procedure TPLHTMLBasicObject.InitStates;
var
  s: TPLCSSElementState;
begin
  for s in TPLCSSElementState do begin
    FStates[s] := TPLCSSDeclarations.Create();
    FStates[s].FreeObjects := false;
  end;
end;

procedure TPLHTMLBasicObject.DoneStates;
var
  s: TPLCSSElementState;
begin
  for s in TPLCSSElementState do
    FStates[s].Free;
end;

procedure TPLHTMLBasicObject.DoDraw;
begin
  // ...
end;

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create;

  FName := 'base_object';
  FText := '';
  FState := esNormal;
  FZoom := 1;
  FPosition := 0;

  FParent := AParent;
  FRenderer := ARenderer;
  //FJSObject := jsbasicobject.create(self);
  FAttributes := TPLHTMLObjectAttributes.Create;
  FChildren := TPLHTMLObjects.Create;

  InitStates;
end;

destructor TPLHTMLBasicObject.Destroy;
begin
  DoneStates;

  inherited Destroy;
end;

function TPLHTMLBasicObject.Clone: IPLHTMLObject;
begin
  Result := TPLHTMLBasicObject.Create(FParent, FRenderer);
end;

function TPLHTMLBasicObject.CSS_InheritValueOf(APropName: TPLString; AId: TPLInt
  ): TPLString;
begin

end;

function TPLHTMLBasicObject.CSS_InitialValueOf(APropName: TPLString; AId: TPLInt
  ): TPLString;
begin

end;

function TPLHTMLBasicObject.CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt
  ): TPLString;
begin

end;

function TPLHTMLBasicObject.CSS_RevertValueOf(APropName: TPLString; AId: TPLInt
  ): TPLString;
begin

end;

function TPLHTMLBasicObject.CSS_Get(APropName: TPLString): Pointer;
begin
  if not Assigned(FStates[FState]) then ;
end;

procedure TPLHTMLBasicObject.CSS_Set(APropName: TPLString; const APropValue);
begin

end;

procedure TPLHTMLBasicObject.UpdateScrollbars;
begin

end;

procedure TPLHTMLBasicObject.Draw;
begin
  if not Assigned(FRenderer) or not isVisible or (Display = 'none') then exit;

  DoDraw;
end;

function TPLHTMLBasicObject.ToHTML: TPLString;
begin
  Result := '';
end;

function TPLHTMLBasicObject.IsVisible: TPLBool;
var
  p: Pointer;
begin
  p := CSS_Get('visibility');
  if not Assigned(p) then exit(true);

  Result := TPLCSSProperty(@p).AsString.ToLower <> 'hidden';
end;

function TPLHTMLBasicObject.Display: TPLString;
var
  p: Pointer;
begin
  p := CSS_Get('display');
  if not Assigned(p) then exit('block');

  Result := TPLCSSProperty(@p).AsString.ToLower;
end;

end.

