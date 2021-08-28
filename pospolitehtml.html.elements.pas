unit PospoLiteHTML.HTML.Elements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, math, Forms,
  LCLType, LCLProc, LCLIntf, strutils, BGRABitmap, BGRABitmapTypes, DOM,
  PospoLiteHTML.CSS.Classes, PospoLiteHTML.HTML.Scrolling,
  PospoLiteHTML.CSS.Animations, PospoLiteHTML.CSS.Basics,
  PospoLiteHTML.CSS.Stylesheet, PospoLiteHTML.CSS.Values;

type

  TPLHTMLBasicObject = class;
  TPLHTMLElementWithLayout = class;

  { TPLHTMLElementLayouter }

  TPLHTMLElementLayouter = class
  private
    FObject: TPLHTMLElementWithLayout;
  public
    constructor Create(AFor: TPLHTMLElementWithLayout);

    procedure UpdateLayout;
  end;

  { TPLHTMLBasicObject }

  TPLHTMLBasicObject = class(TPLHTMLScrollingControl, IPLHTMLBaseObject)
  private
    FClasses: TStringList;
    FDestParent: TPLCSSCustomControl;
    FInlineStyles: TPLCSSString;
    FInnerHTML: TPLCSSString;
    FNode: TDOMNode;
    FTransitions: TPLCSSTransitions;
    FAnim: boolean;
    function GetGlobalStyles: TPLCSSStyleSheet;
    function GetId: TPLCSSString;
    function GetInnerHTML: TPLCSSString;
    function GetOuterHTML: TPLCSSString;
    procedure SetId(AValue: TPLCSSString);
    procedure SetInnerHTML(AValue: TPLCSSString);
    procedure SetNode(AValue: TDOMNode);
    procedure SetOuterHTML(AValue: TPLCSSString);
  protected
    FObjectType: TPLCSSString;
    FMousePos: TPoint;
    FMouseIn, FMouseDown: boolean;
    FEntered: boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    function GetPseudo: TPLCSSString; virtual;
    function GetNode: TDOMNode;

    procedure Paint; override;
    procedure InternalDraw(var {%H-}ABitmap: TBGRABitmap); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateAllStyles; override;

    // get right style value (without animation)
    function Style(AProperty: TPLCSSString; APseudo: TPLCSSString = ''): TPLCSSString; override;

    procedure DrawTo(var ABitmap: TBGRABitmap); override; // for screenshot

    property InnerHTML: TPLCSSString read GetInnerHTML write SetInnerHTML;
    property OuterHTML: TPLCSSString read GetOuterHTML write SetOuterHTML;
    property InlineStyles: TPLCSSString read FInlineStyles write FInlineStyles; // overrides global styles
    property GlobalStyles: TPLCSSStyleSheet read GetGlobalStyles;
    property Classes: TStringList read FClasses write FClasses;
    property Id: TPLCSSString read GetId write SetId;
    property DestParent: TPLCSSCustomControl read FDestParent write FDestParent;

    property Node: TDOMNode read FNode write SetNode;

    property Transitions: TPLCSSTransitions read FTransitions write FTransitions;
  end;

  { TPLHTMLElementWithLayout }

  TPLHTMLElementWithLayout = class(TPLHTMLBasicObject)
  private
    FLayouter: TPLHTMLElementLayouter;
    FPadding: TPLCSSPaddings;
  protected
    FIsUpdating: boolean;

    property Padding: TPLCSSPaddings read FPadding write FPadding;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateAllStyles; override;
    procedure UpdateLayout;

    property Layouter: TPLHTMLElementLayouter read FLayouter;
  end;

  { TPLHTMLElementHtml }

  TPLHTMLElementHtml = class(TPLHTMLElementWithLayout)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TPLHTMLElementBody }

  TPLHTMLElementBody = class(TPLHTMLElementWithLayout)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TPLHTMLElementDiv }

  TPLHTMLElementDiv = class(TPLHTMLElementWithLayout)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TPLHTMLElementLayouter }

constructor TPLHTMLElementLayouter.Create(AFor: TPLHTMLElementWithLayout);
begin
  inherited Create;

  FObject := AFor;
end;

procedure TPLHTMLElementLayouter.UpdateLayout;
var
  size_w, size_h, poz_l, poz_t, poz_r, poz_b: TPLCSSLength;
  margin: TPLCSSMargins;
  w: TPLCSSString;
  i: integer;
begin
  w := FObject.Style('display', FObject.GetPseudo).ToLower;
  if w = 'none' then begin
    FObject.Visible := false;
    exit;
  end else FObject.Visible := true;

  if Assigned(FObject.Browser) then begin
    if FObject.Style('position', FObject.GetPseudo).Trim.ToLower = 'absolute' then
      FObject.Parent := FObject.Browser
    else
      FObject.Parent := FObject.DestParent;

    size_w := FObject.Transitions.TryGetTransitionActual('width', FObject.Style('width', FObject.GetPseudo));
    size_h := FObject.Transitions.TryGetTransitionActual('height', FObject.Style('height', FObject.GetPseudo));
    poz_l := FObject.Transitions.TryGetTransitionActual('left', FObject.Style('left', FObject.GetPseudo));
    poz_t := FObject.Transitions.TryGetTransitionActual('top', FObject.Style('top', FObject.GetPseudo));
    poz_r := FObject.Transitions.TryGetTransitionActual('right', FObject.Style('right', FObject.GetPseudo));
    poz_b := FObject.Transitions.TryGetTransitionActual('bottom', FObject.Style('bottom', FObject.GetPseudo));

    margin := TPLCSSMargins.Create(FObject.Transitions.TryGetTransitionActual('margin', FObject.Style('margin', FObject.GetPseudo)));
    w := FObject.Transitions.TryGetTransitionActual('margin-left', FObject.Style('margin-left', FObject.GetPseudo));
    if not IsInitValue(w) then margin.Left := w;
    w := FObject.Transitions.TryGetTransitionActual('margin-top', FObject.Style('margin-top', FObject.GetPseudo));
    if not IsInitValue(w) then margin.Top := w;
    w := FObject.Transitions.TryGetTransitionActual('margin-right', FObject.Style('margin-right', FObject.GetPseudo));
    if not IsInitValue(w) then margin.Right := w;
    w := FObject.Transitions.TryGetTransitionActual('margin-bottom', FObject.Style('margin-bottom', FObject.GetPseudo));
    if not IsInitValue(w) then margin.Bottom := w;

    FObject.Padding := TPLCSSMargins.Create(FObject.Transitions.TryGetTransitionActual('padding', FObject.Style('padding', FObject.GetPseudo)));
    w := FObject.Transitions.TryGetTransitionActual('padding-left', FObject.Style('padding-left', FObject.GetPseudo));
    if not IsInitValue(w) then FObject.Padding.Left := w;
    w := FObject.Transitions.TryGetTransitionActual('padding-top', FObject.Style('padding-top', FObject.GetPseudo));
    if not IsInitValue(w) then FObject.Padding.Top := w;
    w := FObject.Transitions.TryGetTransitionActual('padding-right', FObject.Style('padding-right', FObject.GetPseudo));
    if not IsInitValue(w) then FObject.Padding.Right := w;
    w := FObject.Transitions.TryGetTransitionActual('padding-bottom', FObject.Style('padding-bottom', FObject.GetPseudo));
    if not IsInitValue(w) then FObject.Padding.Bottom := w;

    FObject.UpdateBasePos(false, CSSLengthToScreen(poz_l, FObject, 'left') + CSSLengthToScreen(margin.Left, FObject, 'margin-left'), CSSLengthToScreen(poz_t, FObject, 'top') + CSSLengthToScreen(margin.Top, FObject, 'margin-top'));
    FObject.Width := CSSLengthToScreen(size_w, FObject, 'width') + CSSLengthToScreen(FObject.Padding.Left, FObject, 'padding-left') + CSSLengthToScreen(FObject.Padding.Right, FObject, 'padding-right');
    FObject.Height := CSSLengthToScreen(size_h, FObject, 'height') + CSSLengthToScreen(FObject.Padding.Top, FObject, 'padding-top') + CSSLengthToScreen(FObject.Padding.Bottom, FObject, 'padding-bottom');
  end;

  for i := 0 to FObject.ControlCount-1 do begin
    if FObject.Controls[i] is TPLHTMLElementWithLayout then
      TPLHTMLElementWithLayout(FObject.Controls[i]).Layouter.UpdateLayout;
  end;
end;

{ TPLHTMLElementWithLayout }

constructor TPLHTMLElementWithLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLayouter := TPLHTMLElementLayouter.Create(Self);
  FIsUpdating := false;
end;

destructor TPLHTMLElementWithLayout.Destroy;
begin
  FLayouter.Free;

  inherited Destroy;
end;

procedure TPLHTMLElementWithLayout.UpdateAllStyles;
//var
  //prop: TPLCSSProperties;
begin
  //prop := Styles['::self::'].Properties;
  //
  //// size and position
  //prop['position'] := Style('position', GetPseudo);
  //prop['left'] := Style('left', GetPseudo);
  //prop['right'] := Style('right', GetPseudo);
  //prop['top'] := Style('top', GetPseudo);
  //prop['bottom'] := Style('bottom', GetPseudo);
  //prop['float'] := Style('float', GetPseudo);
  //prop['clear'] := Style('clear', GetPseudo);
  //prop['width'] := Style('width', GetPseudo);
  //prop['height'] := Style('height', GetPseudo);
  //prop['display'] := Style('display', GetPseudo);
  //
  //prop['margin'] := Style('margin', GetPseudo);
  //prop['margin-left'] := Style('margin-left', GetPseudo);
  //prop['margin-right'] := Style('margin-right', GetPseudo);
  //prop['margin-top'] := Style('margin-top', GetPseudo);
  //prop['margin-bottom'] := Style('margin-bottom', GetPseudo);
  //
  //prop['padding'] := Style('padding', GetPseudo);
  //prop['padding-left'] := Style('padding-left', GetPseudo);
  //prop['padding-right'] := Style('padding-right', GetPseudo);
  //prop['padding-top'] := Style('padding-top', GetPseudo);
  //prop['padding-bottom'] := Style('padding-bottom', GetPseudo);

  inherited UpdateAllStyles;
end;

procedure TPLHTMLElementWithLayout.UpdateLayout;
begin
  if not FIsUpdating then begin
    FIsUpdating := true;
    if Assigned(FLayouter) then FLayouter.UpdateLayout;
    FIsUpdating := false;
  end;
end;

{ TPLHTMLElementHtml }

constructor TPLHTMLElementHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FObjectType := 'html';
end;

{ TPLHTMLElementBody }

constructor TPLHTMLElementBody.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FObjectType := 'body';
end;

{ TPLHTMLElementDiv }

constructor TPLHTMLElementDiv.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FObjectType := 'div';
end;

{ TPLHTMLBasicObject }

procedure TPLHTMLBasicObject.SetInnerHTML(AValue: TPLCSSString);
begin
  if FInnerHTML = AValue then exit;
  FInnerHTML := AValue;
  if Assigned(Browser) then Browser.FastUpdate(self);
end;

procedure TPLHTMLBasicObject.SetNode(AValue: TDOMNode);
var
  s: TPLCSSString = '';
  n: TDOMNode;
begin
  if FNode = AValue then exit;
  FNode := AValue;

  FClasses.Clear;

  if Assigned(AValue) then begin
    n := AValue.Attributes.GetNamedItem('class');
    if Assigned(n) then s := n.NodeValue;
    FClasses.DelimitedText := s;
  end;

  UpdateStyles;
end;

procedure TPLHTMLBasicObject.SetOuterHTML(AValue: TPLCSSString);
begin
  // not implemented - need rebuilt
end;

procedure TPLHTMLBasicObject.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseDown := true;

  //if FAnim then FTransitions.Stop;
  //SetStates(':hover', ':active');
  //FTransitions.Start;
  //FAnim := true;

  Invalidate;
end;

procedure TPLHTMLBasicObject.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMouseDown := false;

  //if FAnim then FTransitions.Stop;
  //SetStates(':active', ':hover');
  //FTransitions.Start;
  //FAnim := true;

  Invalidate;
end;

procedure TPLHTMLBasicObject.MouseEnter;
begin
  inherited MouseEnter;

  if PtInRect(TRect.Create(0, 0, Width, Height), ScreenToClient(Mouse.CursorPos)) and not (csDesigning in ComponentState) then begin
    SetStates('', ':hover');
    FTransitions.Start;
    FAnim := true;
    FEntered := true;
  end;

  Invalidate;
end;

procedure TPLHTMLBasicObject.MouseLeave;
begin
  inherited MouseLeave;

  FMousePos := TPoint.Create(-1, -1);
  FMouseIn := false;

  if FEntered and not (csDesigning in ComponentState) then begin
    FEntered := false;
    SetStates(':hover', '');
    FTransitions.Start;
    FAnim := true;
  end;

  Invalidate;
end;

procedure TPLHTMLBasicObject.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FMousePos := TPoint.Create(X, Y);
  FMouseIn := PtInRect(ClientRect, FMousePos);

  Invalidate;
end;

function TPLHTMLBasicObject.GetPseudo: TPLCSSString;
begin
  Result := '';

  if FMouseIn and FAnim then begin
    if FMouseDown then Result := ':active' else Result := ':hover';
  end;
end;

function TPLHTMLBasicObject.GetNode: TDOMNode;
begin
  Result := FNode;
end;

procedure TPLHTMLBasicObject.Paint;
var
  bmp: TBGRABitmap;
begin
  inherited Paint;

  if (csDesigning in ComponentState) or not Visible then exit;

  UpdateAllStyles;

  bmp := TBGRABitmap.Create(Width, Height);
  try
    InternalDraw(bmp);
    //bmp.TextOut(0, 0, ifthen(FAnim, 'tak', 'nie'), BGRABlack); // debug for mouse
    bmp.Draw(Canvas, 0, 0, false);
  finally
    bmp.Free;
  end;
end;

procedure TPLHTMLBasicObject.InternalDraw(var ABitmap: TBGRABitmap);
var
  bg: TPLCSSBackground;
begin
  // draw bg
  //bg := TPLCSSBackground.Create(Styles['::self::']);
  bg.Location := ExtractFilePath(FFilename);
  if FAnim and not FMouseDown then begin
    bg.Color := FTransitions.TryGetTransitionActual('background-color', bg.Color.AsHex);
  end;
  bg.Draw(ABitmap, ClientRect, self);
  //ABitmap.TextOut(0, 0, bg.Color.AsRGBA, BGRABlack); // color test
end;

function TPLHTMLBasicObject.GetGlobalStyles: TPLCSSStyleSheet;
begin
  if Assigned(Browser) then Result := Browser.Styles else Result := nil;
end;

function TPLHTMLBasicObject.GetId: TPLCSSString;
begin
  if Assigned(FNode) and Assigned(FNode.Attributes.GetNamedItem('id')) then Result := FNode.Attributes.GetNamedItem('id').NodeValue else Result := '';
end;

function TPLHTMLBasicObject.GetInnerHTML: TPLCSSString;
begin
  if Assigned(FNode) then FInnerHTML := FNode.NodeValue else FInnerHTML := '';
  Result := FInnerHTML;
end;

function TPLHTMLBasicObject.GetOuterHTML: TPLCSSString;
begin
  Result := '';
  if Assigned(FNode) then Result := FNode.TextContent else Result := '';
end;

procedure TPLHTMLBasicObject.SetId(AValue: TPLCSSString);
begin
  if Assigned(FNode) and Assigned(FNode.Attributes.GetNamedItem('id')) then FNode.Attributes.GetNamedItem('id').NodeValue := AValue;
end;

constructor TPLHTMLBasicObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Overflow := 'none none';
  FObjectType := 'object';

  FMousePos := TPoint.Create(-1, -1);
  FMouseIn := false;
  FMouseDown := false;
  FAnim := false;
  FEntered := false;

  FInlineStyles := '';
  FClasses := TStringList.Create;
  FClasses.Duplicates := dupIgnore;
  FClasses.Delimiter := ' ';
  FClasses.StrictDelimiter := true;
  FClasses.SkipLastLineBreak := true;
  FNode := nil;

  FTransitions := TPLCSSTransitions.Create(self);
end;

destructor TPLHTMLBasicObject.Destroy;
var
  i: integer;
begin
  for i := ControlCount-1 downto 0 do begin
    if Controls[i] is TPLHTMLBasicObject then Controls[i].Free;
  end;

  FClasses.Free;
  FTransitions.Free;

  inherited Destroy;
end;

procedure TPLHTMLBasicObject.UpdateAllStyles;
//var
  //prop: TPLCSSProperties;
begin
  //prop := Styles['::self::'].Properties;
  //
  //prop['background'] := Style('background', GetPseudo);
  //prop['background-color'] := Style('background-color', GetPseudo);
  //prop['background-image'] := Style('background-image', GetPseudo);
  //prop['background-repeat'] := Style('background-repeat', GetPseudo);
  //prop['background-clip'] := Style('background-clip', GetPseudo);
  //prop['background-attachment'] := Style('background-attachment', GetPseudo);
  //prop['background-origin'] := Style('background-origin', GetPseudo);
  //prop['background-position'] := Style('background-position', GetPseudo);
  //prop['background-size'] := Style('background-size', GetPseudo);
  //prop['opacity'] := Style('opacity', GetPseudo);
  //prop['border'] := Style('border', GetPseudo);
  //prop['border-radius'] := Style('border-radius', GetPseudo);
  //prop['border-top-left-radius'] := Style('border-top-left-radius', GetPseudo);
  //prop['border-top-right-radius'] := Style('border-top-right-radius', GetPseudo);
  //prop['border-bottom-left-radius'] := Style('border-bottom-left-radius', GetPseudo);
  //prop['border-bottom-right-radius'] := Style('border-bottom-right-radius', GetPseudo);
  //prop['border-color'] := Style('border-color', GetPseudo);
  //prop['border-width'] := Style('border-width', GetPseudo);
  //prop['border-style'] := Style('border-style', GetPseudo);
  //prop['border-left-color'] := Style('border-left-color', GetPseudo);
  //prop['border-left-width'] := Style('border-left-width', GetPseudo);
  //prop['border-left-style'] := Style('border-left-style', GetPseudo);
  //prop['border-top-color'] := Style('border-top-color', GetPseudo);
  //prop['border-top-width'] := Style('border-top-width', GetPseudo);
  //prop['border-top-style'] := Style('border-top-style', GetPseudo);
  //prop['border-right-color'] := Style('border-right-color', GetPseudo);
  //prop['border-right-width'] := Style('border-right-width', GetPseudo);
  //prop['border-right-style'] := Style('border-right-style', GetPseudo);
  //prop['border-bottom-color'] := Style('border-bottom-color', GetPseudo);
  //prop['border-bottom-width'] := Style('border-bottom-width', GetPseudo);
  //prop['border-bottom-style'] := Style('border-bottom-style', GetPseudo);
  //
  //// transitions
  //prop['transition'] := Style('transition');
  //prop['transition-delay'] := Style('transition-delay');
  //prop['transition-duration'] := Style('transition-duration');
  //prop['transition-property'] := Style('transition-property');
  //prop['transition-timing-function'] := Style('transition-timing-function');
end;

function TPLHTMLBasicObject.Style(AProperty: TPLCSSString; APseudo: TPLCSSString): TPLCSSString;
var
  i: SizeInt;
  s, w, z: TPLCSSString;
begin
  Result := '';

  if Assigned(GlobalStyles) then begin
    //w := GlobalStyles.Selector['*'].Properties[AProperty]; // global selector
    //s := GlobalStyles.Selector['html'].Properties[AProperty];
    //z := GlobalStyles.Selector['body'].Properties[AProperty];
    if IsInitValue(z) then begin
      if not IsInitValue(s) then w := s;
    end else w := z;

    //s := GlobalStyles.Selector[FObjectType + APseudo].Properties[AProperty]; // object type selector
    if IsInitValue(s) then Result := w else Result := s;

    // inherit from parent
    if Assigned(Parent) and (Parent is TPLHTMLBasicObject) then begin
      w := TPLHTMLBasicObject(Parent).Style(AProperty, APseudo);
      if not IsInitValue(w) then Result := w;
    end;

    // class
    for i := 0 to FClasses.Count-1 do begin
      s := '.' + FClasses[i] + APseudo;
      //w := GlobalStyles.Selector[s].Properties[AProperty];
      if not IsInitValue(w) then Result := w else begin
        s := FObjectType + s;
        //w := GlobalStyles.Selector[s].Properties[AProperty];
        if not IsInitValue(w) then Result := w;
      end;
    end;

    // id
    if Id <> '' then begin
      s := '#' + Id + APseudo;
      //w := GlobalStyles.Selector[s].Properties[AProperty];
      //if IsInitValue(w) then w := GlobalStyles.Selector[FObjectType + s].Properties[AProperty];
      if not IsInitValue(w) then Result := w;
    end;
  end;

  //if not IsInitValue(FInlineStyles[AProperty]) and (APseudo = '') then Result := FInlineStyles[AProperty];

  ProcessImportant(Result);

  if (APseudo <> '') and IsInitValue(Result) then Result := Style(AProperty);
end;

procedure TPLHTMLBasicObject.DrawTo(var ABitmap: TBGRABitmap);
begin
  InternalDraw(ABitmap);
end;

end.

