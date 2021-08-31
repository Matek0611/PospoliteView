unit Pospolite.View.HTML.Basics;

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.Drawing.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.HTML.Scrolling, Pospolite.View.HTML.Events;

type

  { TPLCSSSelectorBind }

  TPLCSSSelectorBind = specialize TPLList<TPLCSSDeclarations>; // redef - avoiding circular unit reference

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TPLHTMLObject)
  private
    FEventTarget: TPLHTMLEventTarget;
    FRealPos: TPLPointF;
    FRenderer: TPLDrawingRenderer;
    FScrolling: TPLHTMLScrolling;
    FSize: TPLRectF;
  protected
    procedure InitStates; override;
    procedure DoneStates; override;
  protected
    // https://developer.mozilla.org/en-US/docs/Web/Events
    procedure InitEventTarget; virtual;
    procedure EventOnClick(const {%H-}AArguments: array of const); virtual;
    procedure EventOnDblClick(const {%H-}AArguments: array of const); virtual;
    procedure EventOnTripleClick(const {%H-}AArguments: array of const); virtual;
    procedure EventOnQuadClick(const {%H-}AArguments: array of const); virtual;
    procedure EventOnContextMenu(const {%H-}AArguments: array of const); virtual;
    procedure EventOnKeyDown(const {%H-}AArguments: array of const); virtual;
    procedure EventOnKeyPress(const {%H-}AArguments: array of const); virtual;
    procedure EventOnKeyUp(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseWheel(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseEnter(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseLeave(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseUp(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseDown(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseOver(const {%H-}AArguments: array of const); virtual;
    procedure EventOnMouseOut(const {%H-}AArguments: array of const); virtual;
    procedure EventOnFocus(const {%H-}AArguments: array of const); virtual;
    procedure EventOnBlur(const {%H-}AArguments: array of const); virtual;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer); virtual; reintroduce;
    destructor Destroy; override;

    function Clone: IPLHTMLObject; override;

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_Get(APropName: TPLString): Pointer; override;
    procedure CSS_Set(APropName: TPLString; const APropValue); override;

    function GetHeight: TPLFloat; override;
    function GetWidth: TPLFloat; override;
    function GetTop: TPLFloat; override;
    function GetLeft: TPLFloat; override;
    function GetRealTop: TPLFloat; override;
    function GetRealLeft: TPLFloat; override;
    function GetElementTarget: Pointer; override;
    function GetArgsFor(const AType: TPLString): TArrayOfConst;

    procedure Draw; reintroduce;

    function IsVisible: TPLBool; override;
    function Display: TPLString; override;
    function PositionType: TPLString; override;

    property Renderer: TPLDrawingRenderer read FRenderer;
    property Scrolling: TPLHTMLScrolling read FScrolling;
    property Size: TPLRectF read FSize write FSize;
    property RealPos: TPLPointF read FRealPos write FRealPos;
    property EventTarget: TPLHTMLEventTarget read FEventTarget;
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
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
      override;

    function ToHTML: TPLString; override;
  end;

  { TPLHTMLNormalObject }

  TPLHTMLNormalObject = class(TPLHTMLBasicObject)
  private
    FAnimationOverrides: TPLCSSDeclarations;
    FBounds: TPLRectF;
    FBindings: TPLCSSSelectorBind;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
      override;
    destructor Destroy; override;

    function ToHTML: TPLString; override;

    property Bounds: TPLRectF read FBounds write FBounds;
    property AnimationOverrides: TPLCSSDeclarations read FAnimationOverrides;
    property Bindings: TPLCSSSelectorBind read FBindings;
  end;

  { TPLHTMLTextObject }

  TPLHTMLTextObject = class(TPLHTMLBasicObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer);
      override;

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
    class function GetTextNodes(AObject: TPLHTMLObject): TPLHTMLObjects;
    class function GetTextFromTextNodes(AObject: TPLHTMLObject): TPLString;
    class function GetTextNodesCount(AObject: TPLHTMLObject): SizeInt;
  end;

implementation

uses Controls, Variants, Pospolite.View.Threads, Pospolite.View.CSS.Binder;

{ TPLHTMLBasicObject }

procedure TPLHTMLBasicObject.InitStates;
var
  s: TPLCSSElementState;
begin
  for s in TPLCSSElementState do begin
    FStates[s] := TPLCSSDeclarations.Create();
    TPLCSSDeclarations(FStates[s]).FreeObjects := false;
  end;
end;

procedure TPLHTMLBasicObject.DoneStates;
var
  s: TPLCSSElementState;
begin
  for s in TPLCSSElementState do
    TPLCSSDeclarations(FStates[s]).Free;
end;

procedure TPLHTMLBasicObject.InitEventTarget;

  procedure AddEventListener(const AType: TPLString; const AEvent: TPLAsyncProc); inline;
  begin
    FEventTarget.AddEventListener(AType, TPLHTMLEventListener.Create(
      TPLHTMLEvent.Create(AEvent, self, AType, GetDefaultEventProperties(AType)))
    );
  end;

begin
  // basic events (more in the future)
  AddEventListener('click', @EventOnClick);
  AddEventListener('dblclick', @EventOnDblClick);
  AddEventListener('tripleclick', @EventOnTripleClick);
  AddEventListener('quadclick', @EventOnQuadClick);
  AddEventListener('contextmenu', @EventOnContextMenu);
  AddEventListener('keydown', @EventOnKeyDown);
  AddEventListener('keypress', @EventOnKeyPress);
  AddEventListener('keyup', @EventOnKeyUp);
  AddEventListener('mousewheel', @EventOnMouseWheel);
  AddEventListener('mouseenter', @EventOnMouseEnter);
  AddEventListener('mouseleave', @EventOnMouseLeave);
  AddEventListener('mouseup', @EventOnMouseUp);
  AddEventListener('mousedown', @EventOnMouseDown);
  AddEventListener('mouseover', @EventOnMouseOver);
  AddEventListener('mouseout', @EventOnMouseOut);
  AddEventListener('focus', @EventOnFocus);
  AddEventListener('blur', @EventOnBlur);
end;

procedure TPLHTMLBasicObject.EventOnClick(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnDblClick(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnTripleClick(
  const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnQuadClick(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnContextMenu(
  const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnKeyDown(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnKeyPress(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnKeyUp(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseWheel(
  const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseEnter(
  const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseLeave(
  const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseUp(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseDown(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseOver(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnMouseOut(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnFocus(const AArguments: array of const);
begin

end;

procedure TPLHTMLBasicObject.EventOnBlur(const AArguments: array of const);
begin

end;

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent);

  FRenderer := ARenderer;
  FNodeType := ontDocumentFragmentNode;
  FScrolling := TPLHTMLScrolling.Create(self, FRenderer);

  FEventTarget := TPLHTMLEventTarget.Create(self);
  InitEventTarget;
end;

destructor TPLHTMLBasicObject.Destroy;
begin
  FreeAndNil(FEventTarget);
  FreeAndNil(FScrolling);

  inherited Destroy;
end;

function TPLHTMLBasicObject.Clone: IPLHTMLObject;
begin
  Result := TPLHTMLBasicObject.Create(Parent as TPLHTMLBasicObject, FRenderer);
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

end;

procedure TPLHTMLBasicObject.CSS_Set(APropName: TPLString; const APropValue);
begin

end;

function TPLHTMLBasicObject.GetHeight: TPLFloat;
begin
  Result := FSize.Height;
end;

function TPLHTMLBasicObject.GetWidth: TPLFloat;
begin
  Result := FSize.Width;
end;

function TPLHTMLBasicObject.GetTop: TPLFloat;
begin
  Result := FSize.Top;
end;

function TPLHTMLBasicObject.GetLeft: TPLFloat;
begin
  Result := FSize.Left;
end;

function TPLHTMLBasicObject.GetRealTop: TPLFloat;
begin
  Result := FRealPos.Y;
end;

function TPLHTMLBasicObject.GetRealLeft: TPLFloat;
begin
  Result := FRealPos.X;
end;

function TPLHTMLBasicObject.GetElementTarget: Pointer;
begin
  Result := FEventTarget;
end;

function TPLHTMLBasicObject.GetArgsFor(const AType: TPLString): TArrayOfConst;
begin
  // for mouse (temporary version)
  SetLength(Result, 2);
  Result[0].VType := vtInt64;
  New(Result[0].VInt64);
  Result[0].VInt64^ := Mouse.CursorPos.x;
  New(Result[1].VInt64);
  Result[1].VInt64^ := Mouse.CursorPos.y;
end;

procedure TPLHTMLBasicObject.Draw;
begin
  if not Assigned(FRenderer) then exit;

  inherited Draw;
  FScrolling.Draw;
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

function TPLHTMLBasicObject.PositionType: TPLString;
var
  p: Pointer;
begin
  p := CSS_Get('position');
  if not Assigned(p) then exit('static');

  Result := TPLCSSProperty(@p).AsString.ToLower;
end;

{ TPLHTMLRootObject }

constructor TPLHTMLRootObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'internal_root_object';
  FNodeType := ontDocumentNode;
end;

{ TPLHTMLVoidObject }

constructor TPLHTMLVoidObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'internal_void_object';
  FNodeType := ontDocumentFragmentNode;
end;

function TPLHTMLVoidObject.ToHTML: TPLString;
var
  ts, te: TPLString;
begin
  ts := '';
  te := '';

  case Name of
    'comment': begin
      ts := '-- ';
      te := ' --';
    end;
    'DOCTYPE': begin
      ts := 'DOCTYPE ';
      te := '';
    end;
    'CDATA': begin
      ts := 'CDATA[ ';
      te := ' ]]';
    end;
  end;

  Result := '<!' + ts + Text + te + '>';
end;

{ TPLHTMLTextObject }

constructor TPLHTMLTextObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'internal_text_object';
  FNodeType := ontTextNode;
end;

function TPLHTMLTextObject.ToHTML: TPLString;
begin
  Result := Text;
end;

{ TPLHTMLNormalObject }

constructor TPLHTMLNormalObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'internal_normal_object';
  FNodeType := ontElementNode;
  FBounds := TPLRectF.Create(0, 0, 0, 0);
  FAnimationOverrides := TPLCSSDeclarations.Create();
  FBindings := TPLCSSSelectorBind.Create;
end;

destructor TPLHTMLNormalObject.Destroy;
begin
  FAnimationOverrides.Free;
  FBindings.Free;

  inherited Destroy;
end;

function TPLHTMLNormalObject.ToHTML: TPLString;
begin
  Result := '<' + Trim(Name + ' ' + FAttributes.ToString) + '>' + LineEnding;

  if not (Name in TPLHTMLObjectFactory.VoidElements) then
    Result += Text + DoToHTMLChildren + '</' + Name + '>';
end;

{ TPLHTMLObjectDIV }

constructor TPLHTMLObjectDIV.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'div';
end;

{ TPLHTMLObjectFactory }

class function TPLHTMLObjectFactory.CreateObjectByTagName(const ATagName: TPLString;
  AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer
  ): TPLHTMLBasicObject;
begin
  if not Assigned(ARenderer) then ARenderer := AParent.Renderer;

  case ATagName.ToLower of
    'internal_text_object': Result := TPLHTMLTextObject.Create(AParent, ARenderer);
    'div': Result := TPLHTMLObjectDIV.Create(AParent, ARenderer);
    else begin
      Result := TPLHTMLNormalObject.Create(AParent, ARenderer);
      Result.Name := ATagName;
    end;
  end;
end;

class function TPLHTMLObjectFactory.GetTextNodes(AObject: TPLHTMLObject
  ): TPLHTMLObjects;
var
  obj: TPLHTMLObject;
begin
  Result := TPLHTMLObjects.Create(false);
  for obj in AObject.Children do
    if obj is TPLHTMLTextObject then Result.Add(obj);
end;

class function TPLHTMLObjectFactory.GetTextFromTextNodes(AObject: TPLHTMLObject
  ): TPLString;
var
  nodes: IPLHTMLObjects;
  obj: TPLHTMLObject;
begin
  nodes := GetTextNodes(AObject);
  Result := '';
  for obj in nodes do Result += obj.Text;
end;

class function TPLHTMLObjectFactory.GetTextNodesCount(AObject: TPLHTMLObject
  ): SizeInt;
var
  nodes: IPLHTMLObjects;
begin
  nodes := GetTextNodes(AObject);
  Result := nodes.Count;
end;

end.

