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
  Pospolite.View.HTML.Scrolling, Pospolite.View.HTML.Events,
  Pospolite.View.CSS.Binder, math;

type

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TPLHTMLObject)
  private
    FEventTarget: TPLHTMLEventTarget;
    FFocused: TPLBool;
    FScrolling: TPLHTMLScrolling;
    FSize: TPLRectF;

    procedure SetFocused(AValue: TPLBool);
  protected
    FCustomProps: TPLCSSDeclarations;
    FSizeTransitionData: TPLVector4F;

    procedure InitStates; override;
    procedure DoneStates; override;
    procedure DoDraw(ADrawer: Pointer); override;
    procedure Render(ARenderer: TPLDrawingRenderer); virtual;
    procedure {%H-}ApplyInlineStyles; override;
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
    constructor Create(AParent: TPLHTMLBasicObject); virtual; reintroduce;
    destructor Destroy; override;

    function Clone: IPLHTMLObject; override;

    function GetCSSProperty(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true): Pointer; override;
    function GetCSSPropertyValue(const AName: TPLString; AState: TPLCSSElementState;
      AUseCommonPrefixes: TPLBool = true; AIndex: SizeInt = 0): Pointer; override;
    procedure SetCSSPropertyValue(const AName: TPLString; const AValue: Pointer;
      AState: TPLCSSElementState; AIndex: SizeInt = 0); override;

    function GetHeight: TPLFloat; override;
    function GetWidth: TPLFloat; override;
    function GetTop: TPLFloat; override;
    function GetLeft: TPLFloat; override;
    function GetElementTarget: Pointer; override;

    function IsVisible: TPLBool; override;
    function Display: TPLString; override;
    function PositionType: TPLString; override;

    property Scrolling: TPLHTMLScrolling read FScrolling;
    property Size: TPLRectF read FSize write FSize;
    property EventTarget: TPLHTMLEventTarget read FEventTarget;
    property Focused: TPLBool read FFocused write SetFocused;
    property CustomProperties: TPLCSSDeclarations read FCustomProps;
  end;

  { TPLHTMLRootObject }

  TPLHTMLRootObject = class(TPLHTMLBasicObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject); override;

    function CoordsInObject(const AX, AY: TPLFloat): TPLBool; override;
  end;

  { TPLHTMLVoidObject }

  TPLHTMLVoidObject = class(TPLHTMLBasicObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject); override;

    function ToHTML: TPLString; override;
    function CoordsInObject(const AX, AY: TPLFloat): TPLBool; override;
  end;

  { TPLHTMLNormalObject }

  TPLHTMLNormalObject = class(TPLHTMLBasicObject)
  private
    FBounds: TPLRectF;
    FBindings: TPLCSSStyleBind;
  public
    constructor Create(AParent: TPLHTMLBasicObject);
      override;
    destructor Destroy; override;

    function ToHTML: TPLString; override;

    function GetDefaultBindings: TPLCSSStyleBind; virtual;
    procedure RefreshStyles(const AParentStyles); override;
    procedure UpdateOwnLayout; override;

    property Bounds: TPLRectF read FBounds write FBounds;
    property Bindings: TPLCSSStyleBind read FBindings;
  end;

  { TPLHTMLTextObject }

  TPLHTMLTextObject = class(TPLHTMLBasicObject)
  public
    constructor Create(AParent: TPLHTMLBasicObject); override;

    function ToHTML: TPLString; override;
    function CoordsInObject(const AX, AY: TPLFloat): TPLBool; override;
  end;

  { TPLHTMLObjectDIV }

  TPLHTMLObjectDIV = class(TPLHTMLNormalObject)
  protected
    procedure Render(ARenderer: TPLDrawingRenderer); override;
  public
    constructor Create(AParent: TPLHTMLBasicObject); override;
  end;

  { TPLHTMLObjectFactory }

  TPLHTMLObjectFactory = packed class sealed
  public const
    VoidElements: array[0..16] of string = (
      '!', 'area', 'base', 'br', 'col', 'command', 'embed', 'hr', 'img',
      'input', 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr'
    );
  public
    class function CreateObjectByTagName(const ATagName: TPLString;
      AParent: TPLHTMLBasicObject): TPLHTMLBasicObject;
    class function GetTextNodes(AObject: TPLHTMLObject): TPLHTMLObjects;
    class function GetTextFromTextNodes(AObject: TPLHTMLObject): TPLString;
    class function GetTextNodesCount(AObject: TPLHTMLObject): SizeInt;
  end;

  operator :=(p: TPLCSSPropertyValuePart) r: TPLCSSSimpleUnit;

implementation

uses Controls, Variants, Pospolite.View.Threads, Pospolite.View.CSS.Basics;

operator :=(p: TPLCSSPropertyValuePart) r: TPLCSSSimpleUnit;
begin
  if p is TPLCSSPropertyValuePartNumber then
    r := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(TPLCSSPropertyValuePartNumber(p).Value, ''))
  else if p is TPLCSSPropertyValuePartDimension then
    r := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(TPLCSSPropertyValuePartDimension(p).Value, TPLCSSPropertyValuePartDimension(p).&Unit))
  else if p.AsString.Trim.ToLower = 'auto' then
    r := TPLCSSSimpleUnit.Auto
  else r := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, p.AsString.Trim.ToLower));
end;

{ TPLHTMLBasicObject }

procedure TPLHTMLBasicObject.SetFocused(AValue: TPLBool);
begin
  if FFocused = AValue then exit;
  FFocused := AValue;
end;

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

procedure TPLHTMLBasicObject.DoDraw(ADrawer: Pointer);
begin
  inherited DoDraw(ADrawer);
  if (GetLeft + GetWidth < 0) or (GetTop + GetHeight < 0) or not IsVisible then exit;

  Render(TPLDrawingRenderer(ADrawer));
  FScrolling.Draw(TPLDrawingRenderer(ADrawer));
end;

procedure TPLHTMLBasicObject.Render(ARenderer: TPLDrawingRenderer);
begin
  ARenderer.DrawHTMLObject(self);
end;

procedure TPLHTMLBasicObject.ApplyInlineStyles;
var
  d: TPLCSSDeclarations;
begin
  d := TPLCSSDeclarations.Create(Attributes.Style.Value);
  try
    TPLCSSDeclarations(FStates[esNormal]).Merge(d);
  finally
    d.Free;
  end;
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
  AddEventListener('tripleclick', @EventOnTripleClick); // fpc+
  AddEventListener('quadclick', @EventOnQuadClick); // fpc+
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

constructor TPLHTMLBasicObject.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

  FNodeType := ontDocumentFragmentNode;
  FScrolling := TPLHTMLScrolling.Create(self);
  FFocused := false;

  FEventTarget := TPLHTMLEventTarget.Create(self);
  InitEventTarget;

  FCustomProps := TPLCSSDeclarations.Create();

  FSizeTransitionData.Reset;
end;

destructor TPLHTMLBasicObject.Destroy;
begin
  FreeAndNil(FEventTarget);
  FreeAndNil(FScrolling);

  FCustomProps.Free;

  inherited Destroy;
end;

function TPLHTMLBasicObject.Clone: IPLHTMLObject;
begin
  Result := TPLHTMLBasicObject.Create(Parent as TPLHTMLBasicObject);
end;

function TPLHTMLBasicObject.GetCSSProperty(const AName: TPLString;
  AState: TPLCSSElementState; AUseCommonPrefixes: TPLBool): Pointer;
var
  i: SizeInt;
  prop: TPLCSSProperty;
begin
  Result := nil;

  if TPLCSSDeclarations(FStates[AState]).Exists(AName, prop) then
    Result := prop;

  if AUseCommonPrefixes and not Assigned(Result) then begin
    i := 0;
    while (i < Length(PLCSSCommonPrefixes)) do begin
      Result := GetCSSProperty(PLCSSCommonPrefixes[i] + AName, AState, false);
      if Assigned(Result) then break;

      Inc(i);
    end;
  end;
end;

function TPLHTMLBasicObject.GetCSSPropertyValue(const AName: TPLString;
  AState: TPLCSSElementState; AUseCommonPrefixes: TPLBool; AIndex: SizeInt
  ): Pointer;
var
  pv: TPLCSSPropertyValue;
begin
  if AIndex < 0 then exit(nil);
  Result := GetCSSProperty(AName, AState, AUseCommonPrefixes);

  if Assigned(Result) then begin
    pv := TPLCSSProperty(Result).Value;
    if AIndex < pv.Count then Result := pv[AIndex]
    else Result := nil;
  end;
end;

procedure TPLHTMLBasicObject.SetCSSPropertyValue(const AName: TPLString;
  const AValue: Pointer; AState: TPLCSSElementState; AIndex: SizeInt);
var
  pv: TPLCSSPropertyValue;
  p: Pointer;
begin
  if (AIndex < 0) or not Assigned(AValue) then exit;
  p := GetCSSProperty(AName, AState, false);

  if Assigned(p) then begin
    pv := TPLCSSProperty(p).Value;
    if AIndex < pv.Count then begin
      pv[AIndex].Free;
      pv[AIndex] := TPLCSSPropertyValuePart(AValue);
    end else pv.Add(TPLCSSPropertyValuePart(AValue));
  end;
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

function TPLHTMLBasicObject.GetElementTarget: Pointer;
begin
  Result := FEventTarget;
end;

function TPLHTMLBasicObject.IsVisible: TPLBool;
var
  p: Pointer;
begin
  p := GetCSSPropertyValue('visibility', FState, false);
  if not Assigned(p) then exit(true);

  Result := TPLCSSProperty(p).AsString.ToLower <> 'hidden';
end;

function TPLHTMLBasicObject.Display: TPLString;
var
  p: Pointer;
begin
  p := GetCSSPropertyValue('display', FState, false);
  if not Assigned(p) then exit('block');

  Result := TPLCSSProperty(p).AsString.ToLower;
end;

function TPLHTMLBasicObject.PositionType: TPLString;
var
  p: Pointer;
begin
  p := GetCSSPropertyValue('position', FState, false);
  if not Assigned(p) then exit('static');

  Result := TPLCSSProperty(p).AsString.ToLower;
end;

{ TPLHTMLRootObject }

constructor TPLHTMLRootObject.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

  FName := 'internal_root_object';
  FNodeType := ontDocumentNode;
end;

function TPLHTMLRootObject.CoordsInObject(const AX, AY: TPLFloat): TPLBool;
begin
  Result := false;
end;

{ TPLHTMLVoidObject }

constructor TPLHTMLVoidObject.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

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

function TPLHTMLVoidObject.CoordsInObject(const AX, AY: TPLFloat): TPLBool;
begin
  Result := false;
end;

{ TPLHTMLTextObject }

constructor TPLHTMLTextObject.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

  FName := 'internal_text_object';
  FNodeType := ontTextNode;
end;

function TPLHTMLTextObject.ToHTML: TPLString;
begin
  Result := Text;
end;

function TPLHTMLTextObject.CoordsInObject(const AX, AY: TPLFloat): TPLBool;
begin
  Result := false;
end;

{ TPLHTMLNormalObject }

constructor TPLHTMLNormalObject.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

  FName := 'internal_normal_object';
  FNodeType := ontElementNode;
  FBounds := TPLRectF.Create(0, 0, 0, 0);
  FBindings := GetDefaultBindings;
end;

destructor TPLHTMLNormalObject.Destroy;
begin
  inherited Destroy;
end;

function TPLHTMLNormalObject.ToHTML: TPLString;
begin
  Result := '<' + Trim(Name + ' ' + FAttributes.ToString) + '>' + LineEnding;

  if not (Name in TPLHTMLObjectFactory.VoidElements) then
    Result += Text + DoToHTMLChildren + '</' + Name + '>';
end;

function TPLHTMLNormalObject.GetDefaultBindings: TPLCSSStyleBind;
begin
  Result.RestoreDefault;
end;

procedure TPLHTMLNormalObject.RefreshStyles(const AParentStyles);
var
  obj: TPLHTMLObject;
  st: TPLCSSElementState;
  dcl: TPLCSSDeclarations;
  p: TPLCSSProperty;

  function ParentCurrentBinding: TPLCSSBindingProperties; inline;
  begin
    Result := TPLCSSStyleBind(AParentStyles).Properties[st];
  end;

begin
  FCustomProps.Clear;
  FBindings := GetDefaultBindings;

  ApplyInlineStyles;

  for st in TPLCSSElementState do begin
    dcl := TPLCSSDeclarations(FStates[st]);

    for p in dcl do begin
      if p.Name.StartsWith('--') then FCustomProps.Add(p.Clone) else
      case WithoutCommonPrefix(p.Name).ToLower of
        'align-content': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Align.Content := GetDefaultBindings.Properties[st].Align.Content;
            'inherit', 'unset': FBindings.Properties[st].Align.Content := ParentCurrentBinding.Align.Content;
            else FBindings.Properties[st].Align.Content := p.Value[0].AsString;
          end;
        end;
        // ... dodać resztę
        'background': ;
        // ...
        'position': ;
        'width': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Width := GetDefaultBindings.Properties[st].Width;
            'inherit', 'unset': FBindings.Properties[st].Width := ParentCurrentBinding.Width;
            else FBindings.Properties[st].Width := p.Value[0];
          end;
        end;
        'height': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Height := GetDefaultBindings.Properties[st].Height;
            'inherit', 'unset': FBindings.Properties[st].Height := ParentCurrentBinding.Height;
            else FBindings.Properties[st].Height := p.Value[0];
          end;
        end;
        'max-width': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Max.Width := GetDefaultBindings.Properties[st].Max.Width;
            'inherit', 'unset': FBindings.Properties[st].Max.Width := ParentCurrentBinding.Max.Width;
            else FBindings.Properties[st].Max.Width := p.Value[0];
          end;
        end;
        'max-height': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Max.Height := GetDefaultBindings.Properties[st].Max.Height;
            'inherit', 'unset': FBindings.Properties[st].Max.Height := ParentCurrentBinding.Max.Height;
            else FBindings.Properties[st].Max.Height := p.Value[0];
          end;
        end;
        'min-width': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Min.Width := GetDefaultBindings.Properties[st].Min.Width;
            'inherit', 'unset': FBindings.Properties[st].Min.Width := ParentCurrentBinding.Min.Width;
            else FBindings.Properties[st].Min.Width := p.Value[0];
          end;
        end;
        'min-height': if p.Value.Count = 1 then begin
          case p.Value[0].AsString.ToLower of
            'initial', 'revert': FBindings.Properties[st].Min.Height := GetDefaultBindings.Properties[st].Min.Height;
            'inherit', 'unset': FBindings.Properties[st].Min.Height := ParentCurrentBinding.Min.Height;
            else FBindings.Properties[st].Min.Height := p.Value[0];
          end;
        end;
        '': ;
      end;
    end;
  end;

  for obj in Children do
    obj.RefreshStyles(FBindings);
end;

procedure TPLHTMLNormalObject.UpdateOwnLayout;
begin
  if not IsVisible or (Display = 'none') then exit;

  FSize := TPLRectF.Create(0, 0, 0, 0);

  //
end;

{ TPLHTMLObjectDIV }

procedure TPLHTMLObjectDIV.Render(ARenderer: TPLDrawingRenderer);
begin
  inherited Render(ARenderer);
end;

constructor TPLHTMLObjectDIV.Create(AParent: TPLHTMLBasicObject);
begin
  inherited Create(AParent);

  FName := 'div';
end;

{ TPLHTMLObjectFactory }

class function TPLHTMLObjectFactory.CreateObjectByTagName(const ATagName: TPLString;
  AParent: TPLHTMLBasicObject): TPLHTMLBasicObject;
begin
  case ATagName.ToLower of
    'internal_text_object': Result := TPLHTMLTextObject.Create(AParent);
    'div': Result := TPLHTMLObjectDIV.Create(AParent);
    else begin
      Result := TPLHTMLNormalObject.Create(AParent);
      Result.Name := ATagName.ToLower;
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

