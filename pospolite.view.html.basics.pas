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
    FParent: IPLHTMLObject;
    FRenderer: TPLDrawingRenderer;
    FJSObject: IPLJSBasicObject;
    FState: TPLCSSElementState;
    FZoom: TPLFloat;
    FStates: array[TPLCSSElementState] of TPLCSSDeclarations;
    FAttributes: IPLHTMLObjectAttributes;
    FChildren: IPLHTMLObjects;
    FName: TPLString;
    FText: TPLString;
    FNodeType: TPLHTMLObjectNodeType;
    function GetAttributes: IPLHTMLObjectAttributes;
    function GetChildren: IPLHTMLObjects;
    function GetJSObject: IPLJSBasicObject;
    function GetName: TPLString;
    function GetNodeType: TPLHTMLObjectNodeType;
    function GetParent: IPLHTMLObject;
    function GetState: TPLCSSElementState;
    function GetText: TPLString;
    function GetZoom: TPLFloat;
    procedure SetName(AValue: TPLString);
    procedure SetParent(AValue: IPLHTMLObject);
    procedure SetState(AValue: TPLCSSElementState);
    procedure SetText(AValue: TPLString);
    procedure SetZoom(AValue: TPLFloat);
    procedure InitStates;
    procedure DoneStates;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer;
      ANodeType: TPLHTMLObjectNodeType = ontElementNode);
    destructor Destroy; override;

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString;
    function CSS_Get(APropName: TPLString): Pointer;
    procedure CSS_Set(APropName: TPLString; const APropValue);

    procedure UpdateScrollbars;
    procedure Draw; virtual;

    function IsVisible: TPLBool;
    function Display: TPLString;

    property Renderer: TPLDrawingRenderer read FRenderer;
    property Zoom: TPLFloat read GetZoom write SetZoom;
    property State: TPLCSSElementState read GetState write SetState;
    property JSObject: IPLJSBasicObject read GetJSObject;
    property Attributes: IPLHTMLObjectAttributes read GetAttributes;
    property Parent: IPLHTMLObject read GetParent write SetParent;
    property Children: IPLHTMLObjects read GetChildren;
    property Name: TPLString read GetName write SetName;
    property Text: TPLString read GetText write SetText;
    property NodeType: TPLHTMLObjectNodeType read GetNodeType;
  end;

implementation

{ TPLHTMLBasicObject }

function TPLHTMLBasicObject.GetAttributes: IPLHTMLObjectAttributes;
begin
  Result := FAttributes;
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

function TPLHTMLBasicObject.GetNodeType: TPLHTMLObjectNodeType;
begin
  Result := FNodeType;
end;

function TPLHTMLBasicObject.GetParent: IPLHTMLObject;
begin
  Result := FParent;
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

procedure TPLHTMLBasicObject.SetParent(AValue: IPLHTMLObject);
begin
  FParent := AValue;
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

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer; ANodeType: TPLHTMLObjectNodeType);
begin
  inherited Create;

  FName := 'base_object';
  FText := '';
  FState := esNormal;
  FZoom := 1;
  FNodeType := ANodeType;

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
  if not isVisible or (Display = 'none') then exit;

  // ...
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

