unit Pospolite.View.HTML.Basics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.Drawing.Renderer,
  Pospolite.View.CSS.Declaration;

type

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TPLHTMLObject)
  private
    FRenderer: TPLDrawingRenderer;
  protected
    procedure InitStates; override;
    procedure DoneStates; override;
  public
    constructor Create(AParent: TPLHTMLBasicObject; ARenderer: TPLDrawingRenderer); virtual;

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

  FName := 'root_object';
end;

{ TPLHTMLVoidObject }

constructor TPLHTMLVoidObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'void_object';
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

{ TPLHTMLObjectDIV }

constructor TPLHTMLObjectDIV.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'div';
end;

{ TPLHTMLNormalObject }

constructor TPLHTMLNormalObject.Create(AParent: TPLHTMLBasicObject;
  ARenderer: TPLDrawingRenderer);
begin
  inherited Create(AParent, ARenderer);

  FName := 'normal_object';
end;

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

