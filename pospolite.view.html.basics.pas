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
    FPosition: SizeInt;
    function GetAttributes: IPLHTMLObjectAttributes;
    function GetChild(const AName: TPLString): IPLHTMLObject;
    function GetChildren: IPLHTMLObjects;
    function GetJSObject: IPLJSBasicObject;
    function GetName: TPLString;
    function GetParent: IPLHTMLObject;
    function GetPosition: SizeInt;
    function GetState: TPLCSSElementState;
    function GetText: TPLString;
    function GetZoom: TPLFloat;
    procedure SetName(AValue: TPLString);
    procedure SetParent(AValue: IPLHTMLObject);
    procedure SetPosition(AValue: SizeInt);
    procedure SetState(AValue: TPLCSSElementState);
    procedure SetText(AValue: TPLString);
    procedure SetZoom(AValue: TPLFloat);
    procedure InitStates;
    procedure DoneStates;
  protected
    procedure DoDraw; virtual;
    function DoToHTMLChildren: TPLString;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer); virtual;
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
    property Parent: IPLHTMLObject read GetParent write SetParent;
    property Children: IPLHTMLObjects read GetChildren;
    property Child[const AName: TPLString]: IPLHTMLObject read GetChild;
    property Name: TPLString read GetName write SetName;
    property Text: TPLString read GetText write SetText;
    property Position: SizeInt read GetPosition write SetPosition;
  end;

  { TPLHTMLRootObject }

  TPLHTMLRootObject = class(TPLHTMLBasicObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
      override;
  end;

  { TPLHTMLVoidObject }

  TPLHTMLVoidObject = class(TPLHTMLBasicObject)
  public
    function ToHTML: TPLString; override;
  end;

  { TPLHTMLNormalObject }

  TPLHTMLNormalObject = class(TPLHTMLBasicObject)
  public
    function ToHTML: TPLString; override;
  end;

  { TPLHTMLObjectDIV }

  TPLHTMLObjectDIV = class(TPLHTMLNormalObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
      override;
  end;

  { TPLHTMLObjectFactory }

  TPLHTMLObjectFactory = packed class sealed
  public const
    VoidElements: array[0..16] of string = (
      '!', 'area', 'base', 'br', 'col', 'command', 'embed', 'hr', 'img',
      'input', 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr'
    );
  public
    class function CreateObjectByTagName(const ATagName: TPLString; AParent: TPLHTMLBasicObject;
      ARenderer: TPLDrawingRenderer = nil): TPLHTMLBasicObject;
  end;

implementation

{ TPLHTMLBasicObject }

function TPLHTMLBasicObject.GetAttributes: IPLHTMLObjectAttributes;
begin
  Result := FAttributes;
end;

function TPLHTMLBasicObject.GetChild(const AName: TPLString): IPLHTMLObject;
var
  c: IPLHTMLObject;
begin
  for c in FChildren do begin
    if c.Name.ToLower = AName.ToLower then exit(c);
  end;

  Result := nil;
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

function TPLHTMLBasicObject.GetParent: IPLHTMLObject;
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

procedure TPLHTMLBasicObject.SetParent(AValue: IPLHTMLObject);
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

function TPLHTMLBasicObject.DoToHTMLChildren: TPLString;
var
  obj: IPLHTMLObject;
begin
  for obj in FChildren do
    Result += obj.ToHTML + LineEnding;
end;

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create;

  FName := 'basic_object';
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
  Result := TPLHTMLBasicObject.Create(FParent as TPLHTMLBasicObject, FRenderer);
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
  Result := DoToHTMLChildren;
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

{ TPLHTMLRootObject }

constructor TPLHTMLRootObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'root_object';
end;

{ TPLHTMLVoidObject }

function TPLHTMLVoidObject.ToHTML: TPLString;
var
  ts, te: TPLString;
begin
  ts := '';
  te := '';

  case Name of
    'comment': begin
      ts := '--';
      te := '--';
    end;
    'DOCTYPE': begin
      ts := 'DOCTYPE';
      te := '';
    end;
    'CDATA': begin
      ts := 'CDATA[';
      te := ']]';
    end;
  end;

  Result := '<!' + ts + Text + te + '>';
end;

{ TPLHTMLObjectDIV }

constructor TPLHTMLObjectDIV.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'div';
end;

{ TPLHTMLNormalObject }

function TPLHTMLNormalObject.ToHTML: TPLString;
begin
  Result := '<' + Name + ' ' + FAttributes.ToString + '>' + LineEnding;

  if not (Name in TPLHTMLObjectFactory.VoidElements) then
    Result += DoToHTMLChildren + LineEnding + '<' + Name + '/>' + LineEnding;
end;

{ TPLHTMLObjectFactory }

class function TPLHTMLObjectFactory.CreateObjectByTagName(const ATagName: TPLString;
  AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer
  ): TPLHTMLBasicObject;
begin
  if not Assigned(ARenderer) then ARenderer := AParent.Renderer;

  case ATagName.ToLower of
    'div': Result := TPLHTMLObjectDIV.Create(AParent, ARenderer);
    else begin
      Result := TPLHTMLBasicObject.Create(AParent, ARenderer);
      Result.Text := ATagName;
    end;
  end;
end;

end.

