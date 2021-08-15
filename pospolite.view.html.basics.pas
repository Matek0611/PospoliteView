unit Pospolite.View.HTML.Basics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.Drawing.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.CSS.Binder;

type

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TPLHTMLObject)
  private
    FRenderer: TPLDrawingRenderer;
  protected
    procedure InitStates; override;
    procedure DoneStates; override;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer); virtual; reintroduce;

    function Clone: IPLHTMLObject; override;

    function CSS_InheritValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_InitialValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_UnsetValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_RevertValueOf(APropName: TPLString; AId: TPLInt = 0): TPLString; override;
    function CSS_Get(APropName: TPLString): Pointer; override;
    procedure CSS_Set(APropName: TPLString; const APropValue); override;

    procedure Draw; reintroduce;

    function IsVisible: TPLBool; override;
    function Display: TPLString; override;

    property Renderer: TPLDrawingRenderer read FRenderer;
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

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent);

  FRenderer := ARenderer;
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

procedure TPLHTMLBasicObject.Draw;
begin
  if not Assigned(FRenderer) then exit;

  inherited Draw;
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

  FName := 'internal_root_object';
end;

{ TPLHTMLVoidObject }

constructor TPLHTMLVoidObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'internal_void_object';
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

