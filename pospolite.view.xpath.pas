unit Pospolite.View.XPath;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, strutils, Pospolite.View.Basics;

type

  IPLXToken = interface;
  TPLXToken = class;
  TPLXExpression = class;

  TPLXTokenKind = (xtkEOF, xtkIdentifier, xtkText, xtkComment, xtkString,
    xtkInteger, xtkFloat, xtkBracketStart, xtkBracketEnd, xtkParenthesisStart,
    xtkParenthesisEnd, xtkDoubleColon, xtkDot, xtkDoubleDot, xtkSlash, xtkDoubleSlash,
    xtkDollar, xtkAt, xtkComma, xtkPlus, xtkMinus, xtkMultiply, xtkEquals,
    xtkNotEquals, xtkLess, xtkLessEqual, xtkGreater, xtkGreaterEqual, xtkOr,
    xtkAnd, xtkDivF, xtkDivI, xtkNamespace, xtkMod, xtkParent, xtkChild, xtkDescendant,
    xtkSelf, xtkAncestor, xtkFollowing, xtkPreceding, xtkPrecedingSibling,
    xtkFollowingSibling, xtkAncestorOrSelf, xtkDescendantOrSelf, xtkDocumentNode,
    xtkNode, xtkElement, xtkAttribute, xtkSchemaElement, xtkSchemaAttribute,
    xtkProcessingInstruction, xtkElementState);
  TPLXTokenKinds = set of TPLXTokenKind;

  operator :=(k: TPLXTokenKind) r: TPLString;
  operator :=(k: TPLString) r: TPLXTokenKind;
  function XTokenKindCategory(const ACategory: TPLString): TPLXTokenKinds;

type

  { IPLXToken }

  IPLXToken = interface(specialize IPLCloneable<IPLXToken>)
    ['{A9B47866-21E7-4579-BF6A-66EF05121F78}']
    function GetKind: TPLXTokenKind;
    function GetLine: SizeInt;
    function GetLineIndex: SizeInt;
    function GetText: TPLString;

    function ToInteger: TPLInt;
    function ToFloat: TPLFloat;

    property Kind: TPLXTokenKind read GetKind;
    property Line: SizeInt read GetLine;
    property LineIndex: SizeInt read GetLineIndex;
    property Text: TPLString read GetText;
  end;

  IPLXTokens = specialize IPLList<IPLXToken>;
  TPLXTokens = class(specialize TPLList<IPLXToken>, IPLXTokens);

  { TPLXToken }

  TPLXToken = class(TInterfacedObject, IPLXToken)
  private
    FKind: TPLXTokenKind;
    FLine, FIndex: SizeInt;
    FText: TPLString;
    function GetKind: TPLXTokenKind;
    function GetLine: SizeInt;
    function GetLineIndex: SizeInt;
    function GetText: TPLString;
  public
    constructor Create(AKind: TPLXTokenKind; AText: TPLString; ALine, AIndex: SizeInt);

    function ToInteger: TPLInt;
    function ToFloat: TPLFloat;
    function Clone: IPLXToken;

    property Kind: TPLXTokenKind read GetKind;
    property Line: SizeInt read GetLine;
    property LineIndex: SizeInt read GetLineIndex;
    property Text: TPLString read GetText;
  end;

  { TPLXLexer }

  TPLXLexer = class(TObject)
  private
    FLastError: TPLString;
    FToken: IPLXToken;
    FTokens: IPLXTokens;
    FIndex: SizeInt;
    function ParseSymbol(ASymbol: TPLString; AWhere: TPLXTokenKinds; ALine, AIndex: SizeInt): TPLBool;
    procedure LexExpression(AExpression: TPLString);
  public
    constructor Create(const AExpression: TPLString);

    function IsEOF: TPLBool;
    function Consume(AKind: TPLXTokenKind): TPLBool;
    function Peek: IPLXToken;
    procedure MoveForward;

    property LastError: TPLString read FLastError;
    property Token: IPLXToken read FToken;
  end;

  TPLXValueKind = (xvkInteger, xvkFloat, xvkString, xvkBoolean, xvkHTMLObjects);

  { TPLXValue }

  TPLXValue = packed record
  strict private
    FNumber: TPLString;
    FList: IPLHTMLObjects;
    FKind: TPLXValueKind;
  public
    constructor NewInteger(const AValue: TPLInt);
    constructor NewFloat(const AValue: TPLFloat);
    constructor NewString(const AValue: TPLString);
    constructor NewBoolean(const AValue: TPLBool);
    constructor NewHTMLObjects(const AValue: IPLHTMLObjects);

    function ToInteger: TPLInt;
    function ToFloat: TPLFloat;
    function ToString: TPLString;
    function ToBoolean: TPLBool;
    function ToHTMLObjects: IPLHTMLObjects;

    function CanCastTo(AValue: TPLXValue): TPLBool;
    function IsNull: TPLBool;
    class function Null: TPLXValue; static;
    class operator =(a, b: TPLXValue) r: TPLBool;

    property Kind: TPLXValueKind read FKind write FKind;
  end;

  IPLXValues = specialize IPLList<TPLXValue>;
  TPLXValues = class(specialize TPLList<TPLXValue>, IPLXValues);

  { TPLXContext }

  TPLXContext = packed record
  strict private
    FHTMLDocument: IPLHTMLDocument;
    FHTMLObject: TPLHTMLObject;
    FPosition: SizeInt;
  public
    constructor Create(ADoc: IPLHTMLDocument; AObj: TPLHTMLObject; APos: SizeInt);

    property HTMLDocument: IPLHTMLDocument read FHTMLDocument write FHTMLDocument;
    property HTMLObject: TPLHTMLObject read FHTMLObject write FHTMLObject;
    property Position: SizeInt read FPosition write FPosition;
  end;

  { IPLXParameters }

  IPLXParameters = interface(specialize IPLObjectList<TPLXExpression>)
    ['{C84230A2-9C82-459B-8076-852C64FE0F8F}']
    function Evaluate(AContext: TPLXContext): IPLXValues;
  end;

  { TPLXParameters }

  TPLXParameters = class(specialize TPLObjectList<TPLXExpression>, IPLXParameters)
  public
    function Evaluate(AContext: TPLXContext): IPLXValues;
  end;

  TPLXOperator = (xoNone, xoPlus, xoMinus, xoMultiply, xoFloatDivide,
    xoIntegerDivide, xoModulo, xoOr, xoAnd, xoNegate, xoEqual, xoNotEqual,
    xoLess, xoGreater, xoLessOrEqual, xoGreaterOrEqual, xoStep);

  { IPLXEvaluation }

  IPLXEvaluation = interface
    ['{98D8D25B-231F-41E5-A1F7-3ABA958D33D9}']
    function Operation(AOperator: TPLXOperator; const A, B: TPLXValue): TPLXValue;
    function Evaluate(AContext: TPLXContext): TPLXValue;
  end;

  { TPLXEvaluation }

  TPLXEvaluation = class(TInterfacedObject, IPLXEvaluation)
  public
    function Operation(AOperator: TPLXOperator; const A, B: TPLXValue
      ): TPLXValue;
    function Evaluate({%H-}AContext: TPLXContext): TPLXValue; virtual;
  end;

  IPLXEvaluations = specialize IPLList<IPLXEvaluation>;
  TPLXEvaluations = class(specialize TPLList<IPLXEvaluation>, IPLXEvaluations);

  TPLXExpressionKind = (xekPath, xekExpression, xekNegation, xekRoot);
  TPLXExpressionElement = specialize TPLParameter<TPLXOperator, IPLXEvaluation>;
  IPLXExpressionElements = specialize IPLList<TPLXExpressionElement>;
  TPLXExpressionElements = class(specialize TPLList<TPLXExpressionElement>, IPLXExpressionElements);

  { TPLXExpression }

  TPLXExpression = class(TPLXEvaluation)
  private
    FKind: TPLXExpressionKind;
    FOperand: IPLXEvaluation;
    FElements: IPLXExpressionElements;
  public
    constructor Create(const AKind: TPLXExpressionKind; AOperand: IPLXEvaluation);

    function Evaluate(AContext: TPLXContext): TPLXValue; override;
    procedure AddElement(const AOperator: TPLXOperator; AOperand: IPLXEvaluation);

    property Kind: TPLXExpressionKind read FKind write FKind;
  end;

  IPLXExpressions = specialize IPLObjectList<TPLXExpression>;
  TPLXExpressions = class(specialize TPLObjectList<TPLXExpression>, IPLXExpressions);

  TPLXPrimaryExpressionKind = (xpekExpression, xpekContext, xpekValue, xpekVariable,
    xpekFunction);

  { TPLXPrimaryExpression }

  TPLXPrimaryExpression = class(TPLXEvaluation)
  private
    FKind: TPLXPrimaryExpressionKind;
    FExpression: TPLXExpression;
    FParameters: IPLXParameters;
    FToken: IPLXToken;
  public
    constructor Create(AToken: IPLXToken);
    constructor Create(AToken: IPLXToken; AParameters: IPLXParameters);
    constructor Create(AExpression: TPLXExpression);
    destructor Destroy; override;

    function Evaluate(AContext: TPLXContext): TPLXValue; override;

    property Kind: TPLXPrimaryExpressionKind read FKind write FKind;
  end;

  TPLXNodeTestKind = (xntIdentifier, xntKind, xntAny);

  { TPLXNodeTest }

  TPLXNodeTest = class(TPLXEvaluation)
  private
    FKind: TPLXNodeTestKind;
    FToken: IPLXToken;
    FTokenKind: TPLXTokenKind;
  public
    constructor CreateIdentifierTest(AToken: IPLXToken);
    constructor CreateKindTest(AKind: TPLXTokenKind);
    constructor CreateAnyTest;

    function Evaluate(AContext: TPLXContext): TPLXValue; override;
    function EvaluateAxis(AAxis: TPLXTokenKind; AContext: TPLXContext): TPLXValue;

    property Kind: TPLXNodeTestKind read FKind write FKind;
  end;

  { TPLXStep }

  TPLXStep = class(TPLXEvaluation)
  private
    FAxis: TPLXTokenKind;
    FTest: TPLXNodeTest;
  public
    constructor Create(ATest: TPLXNodeTest; AAxis: TPLXTokenKind = xtkEOF);
    destructor Destroy; override;

    function Evaluate(AContext: TPLXContext): TPLXValue; override;
  end;

  { TPLXStepExpression }

  TPLXStepExpression = class(TPLXEvaluation)
  private
    FStep: IPLXEvaluation;
    FPredicates: IPLXExpressions;
  public
    constructor Create(AStep: IPLXEvaluation);

    function Evaluate(AContext: TPLXContext): TPLXValue; override;
    procedure AddPredicate(APredicate: TPLXExpression);
  end;

  { TPLXPath }

  TPLXPath = packed class sealed
  private
    class function ConvertCSSToXPath(ACSS: TPLString): TPLString;
    class function ParseXPath(AXPath: TPLString): TPLXEvaluation;
    class function ParseExpression(ALexer: TPLXLexer): TPLXExpression;
    class function ParseAnd(ALexer: TPLXLexer): TPLXExpression;
    class function ParseComparison(ALexer: TPLXLexer): TPLXExpression;
    class function ParsePlusMinus(ALexer: TPLXLexer): TPLXExpression;
    class function ParseMultiplicative(ALexer: TPLXLexer): TPLXExpression;
    class function ParseUnary(ALexer: TPLXLexer): TPLXExpression;
    class function ParsePath(ALexer: TPLXLexer): TPLXExpression;
    class function ParseRelativePath(ALexer: TPLXLexer): TPLXExpression;
    class function ParseStep(ALexer: TPLXLexer): TPLXEvaluation;
    class function ParseStepExpression(ALexer: TPLXLexer): TPLXEvaluation;
    class function ParsePrimary(ALexer: TPLXLexer): TPLXPrimaryExpression;
    class function ParseNodeTest(ALexer: TPLXLexer): TPLXNodeTest;
    class function ParseParameters(ALexer: TPLXLexer): TPLXParameters;
  public
    class function EvaluateXPath(ADocument: IPLHTMLDocument; AXPath: TPLString): TPLXValue;
    class function EvaluateCSS(ADocument: IPLHTMLDocument; ACSS: TPLString): TPLXValue; inline;
  end;

implementation

operator :=(k: TPLXTokenKind) r: TPLString;
begin
  case k of
    xtkEOF: r := 'EOF';
    xtkIdentifier: r := 'Identifier';
    xtkText: r := 'text';
    xtkComment: r := 'comment';
    xtkString: r := 'String';
    xtkInteger: r := 'Integer';
    xtkFloat: r := 'Float';
    xtkBracketStart: r := '[';
    xtkBracketEnd: r:= ']';
    xtkParenthesisStart: r := '(';
    xtkParenthesisEnd: r := ')';
    xtkDoubleColon: r := '::';
    xtkDot: r := '.';
    xtkDoubleDot: r := '..';
    xtkSlash: r := '/';
    xtkDoubleSlash: r := '//';
    xtkDollar: r := '$';
    xtkAt: r := '@';
    xtkComma: r := ',';
    xtkPlus: r := '+';
    xtkMinus: r := '-';
    xtkMultiply: r := '*';
    xtkEquals: r := '=';
    xtkNotEquals: r := '!=';
    xtkLess: r := '<';
    xtkLessEqual: r := '<=';
    xtkGreater: r := '>';
    xtkGreaterEqual: r := '>=';
    xtkOr: r := 'or';
    xtkAnd: r := 'and';
    xtkDivF: r := 'div';
    xtkDivI: r := 'idiv';
    xtkMod: r := 'mod';
    xtkNamespace: r := 'namespace';
    xtkParent: r := 'parent';
    xtkChild: r := 'child';
    xtkDescendant: r := 'descendant';
    xtkSelf: r := 'self';
    xtkAncestor: r := 'ancestor';
    xtkFollowing: r := 'following';
    xtkPreceding: r := 'preceding';
    xtkPrecedingSibling: r := 'preceding-sibling';
    xtkFollowingSibling: r := 'following-sibling';
    xtkAncestorOrSelf: r := 'ancestor-or-self';
    xtkDescendantOrSelf: r := 'descendant-or-self';
    xtkDocumentNode: r := 'document-node';
    xtkNode: r := 'node';
    xtkElement: r := 'element';
    xtkAttribute: r := 'attribute';
    xtkSchemaElement: r := 'schema-element';
    xtkSchemaAttribute: r := 'schema-attribute';
    xtkProcessingInstruction: r := 'processing-instruction';
    xtkElementState: r := 'state';
  end;
end;

operator :=(k: TPLString) r: TPLXTokenKind;
begin
  case k of
    'EOF': r := xtkEOF;
    'Identifier': r := xtkIdentifier;
    'text': r := xtkText;
    'comment': r := xtkComment;
    'String': r := xtkString;
    'Integer': r := xtkInteger;
    'Float': r := xtkFloat;
    '[': r := xtkBracketStart;
    ']': r:= xtkBracketEnd;
    '(': r := xtkParenthesisStart;
    ')': r := xtkParenthesisEnd;
    '::': r := xtkDoubleColon;
    '.': r := xtkDot;
    '..': r := xtkDoubleDot;
    '/': r := xtkSlash;
    '//': r := xtkDoubleSlash;
    '$': r := xtkDollar;
    '@': r := xtkAt;
    ',': r := xtkComma;
    '+': r := xtkPlus;
    '-': r := xtkMinus;
    '*': r := xtkMultiply;
    '=': r := xtkEquals;
    '!=': r := xtkNotEquals;
    '<': r := xtkLess;
    '<=': r := xtkLessEqual;
    '>': r := xtkGreater;
    '>=': r := xtkGreaterEqual;
    'or': r := xtkOr;
    'and': r := xtkAnd;
    'div': r := xtkDivF;
    'idiv': r := xtkDivI;
    'mod': r := xtkMod;
    'namespace': r := xtkNamespace;
    'parent': r := xtkParent;
    'child': r := xtkChild;
    'descendant': r := xtkDescendant;
    'self': r := xtkSelf;
    'ancestor': r := xtkAncestor;
    'following': r := xtkFollowing;
    'preceding': r := xtkPreceding;
    'preceding-sibling': r := xtkPrecedingSibling;
    'following-sibling': r := xtkFollowingSibling;
    'ancestor-or-self': r := xtkAncestorOrSelf;
    'descendant-or-self': r := xtkDescendantOrSelf;
    'document-node': r := xtkDocumentNode;
    'node': r := xtkNode;
    'element': r := xtkElement;
    'attribute': r := xtkAttribute;
    'schema-element': r := xtkSchemaElement;
    'schema-attribute': r := xtkSchemaAttribute;
    'processing-instruction': r := xtkProcessingInstruction;
    'state': r := xtkElementState;
    else r := xtkEOF;
  end;
end;

function XTokenKindCategory(const ACategory: TPLString): TPLXTokenKinds;
begin
  case ACategory of
    'single': Result := [xtkBracketStart..xtkParenthesisEnd, xtkDot, xtkSlash, xtkDollar..xtkEquals, xtkLess, xtkGreater];
    'double': Result := [xtkDoubleColon, xtkDoubleDot, xtkDoubleSlash, xtkLessEqual, xtkGreaterEqual];
    'word': Result := [xtkText, xtkComment, xtkOr..xtkElementState];
    'compare': Result := [xtkEquals..xtkGreaterEqual];
    '+/-': Result := [xtkPlus, xtkMinus];
    'slashes': Result := [xtkSlash, xtkDoubleSlash];
    'multi': Result := [xtkMultiply, xtkDivF, xtkDivI, xtkMod];
    'other': Result := [xtkEOF, xtkIdentifier, xtkString..xtkFloat];
    'all': Result := [xtkEOF..xtkElementState];
    else Result := [];
  end;
end;

{ TPLXToken }

function TPLXToken.GetKind: TPLXTokenKind;
begin
  Result := FKind;
end;

function TPLXToken.GetLine: SizeInt;
begin
  Result := FLine;
end;

function TPLXToken.GetLineIndex: SizeInt;
begin
  Result := FIndex;
end;

function TPLXToken.GetText: TPLString;
begin
  Result := FText;
end;

constructor TPLXToken.Create(AKind: TPLXTokenKind; AText: TPLString; ALine,
  AIndex: SizeInt);
begin
  inherited Create;

  FKind := AKind;
  FText := AText;
  FLine := ALine;
  FIndex := AIndex;
end;

function TPLXToken.ToInteger: TPLInt;
begin
  Result := FText;
end;

function TPLXToken.ToFloat: TPLFloat;
begin
  Result := FText;
end;

function TPLXToken.Clone: IPLXToken;
begin
  Result := TPLXToken.Create(FKind, FText, FLine, FIndex);
end;

{ TPLXLexer }

function TPLXLexer.ParseSymbol(ASymbol: TPLString; AWhere: TPLXTokenKinds;
  ALine, AIndex: SizeInt): TPLBool;
var
  k: TPLXTokenKind;
begin
  k := TPLXTokenKind(ASymbol);
  Result := (k <> xtkEOF) and (k in AWhere);
  if Result then FTokens.Add(TPLXToken.Create(k, ASymbol, ALine, AIndex));
end;

procedure TPLXLexer.LexExpression(AExpression: TPLString);
var
  ei: TPLBool;
  line, index, len, p, ps: SizeInt;
  cc, pom, pomw: TPLString;

  function IsNotEnd: TPLBool; inline;
  begin
    Result := p <= len;
  end;

  procedure AddEOF;
  begin
    FTokens.Add(TPLXToken.Create(xtkEOF, xtkEOF, line, len+1));
    FToken := FTokens.First;
  end;

begin
  line := 1;
  index := 1;
  p := 1;
  len := AExpression.Length;

  FToken := nil;
  FTokens.Clear;
  FIndex := 0;
  FLastError := '';

  if AExpression.IsEmpty then begin
    AddEOF;
    exit;
  end;

  while IsNotEnd do begin
    pom := '';

    while IsNotEnd and AExpression[p].IsWhiteSpace do begin
      if AExpression[p] = #13 then begin
        index := p + 1;
        line += 1;
      end;
      p += 1;
    end;

    cc := '';
    if p <= len-1 then cc := AExpression.SubStr(p, 2);

    if IsNotEnd then begin
      if cc = '(:' then begin
        while IsNotEnd and (cc <> ':)') do begin
          if AExpression[p] = #13 then begin
            index := p + 1;
            line += 1;
          end;
          p += 1;
          cc := AExpression.SubStr(p, 2);
        end;
        p += 2;
      end else if IsNotEnd then begin
        if AExpression[p] in ['(', ')', '[', ']', '/', '$', '@', '*', '+', '-', '=', '<', '>', ':', '.', ',', '!'] then begin
          if ParseSymbol(cc, XTokenKindCategory('double'), line, p - index) then p += 2
          else if ParseSymbol(cc, XTokenKindCategory('single'), line, p - index) then p += 1
          else begin
            FLastError := '(%d, %d) Illegal symbol: "%s"'.Format([line, p - index, AExpression[p]]);
            AddEOF;
            exit;
          end;
        end else if AExpression[p] in ['0'..'9'] then begin
          ps := p;

          while IsNotEnd and (AExpression[p] in ['0'..'9']) do begin
            pom += AExpression[p];
            p += 1;
          end;

          if (p+1 <= len) and (AExpression[p] = '.') and (AExpression[p+1] in ['0'..'9']) then begin
            pom += '.';
            p += 1;

            while IsNotEnd and (AExpression[p] in ['0'..'9']) do begin
              pom += AExpression[p];
              p += 1;
            end;
            FTokens.Add(TPLXToken.Create(xtkFloat, pom, line, ps - index));
          end else FTokens.Add(TPLXToken.Create(xtkInteger, pom, line, ps - index));
        end else if lowerCase(AExpression[p]) in ['_', 'a'..'z'] then begin
          ps := p;
          pom += AExpression[p];
          p += 1;

          while IsNotEnd and (lowerCase(AExpression[p]) in ['_', 'a'..'z']) do begin
            pom += AExpression[p];
            p += 1;
          end;

          pomw := pom.ToLower;
          ei := false;
          if FTokens.Count > 0 then begin
            ei := ((FTokens.Last.Kind = xtkDoubleColon) and not
              ((pomw in TPLStringFuncs.NewArray(['text', 'node'])) and
              (AExpression[p] = '('))) or ((FTokens.Last.Kind in [xtkSlash, xtkDoubleSlash]) and not
              ((AExpression.SubStr(p, 2) = '::') and (TPLXTokenKind(pomw) in [xtkChild, xtkParent,
              xtkSelf, xtkDescendant, xtkFollowing, xtkPreceding, xtkNamespace, xtkAncestor,
              xtkFollowingSibling, xtkPrecedingSibling, xtkDescendantOrSelf, xtkAncestorOrSelf])));
          end;

          if ei or not ParseSymbol(pomw, XTokenKindCategory('word'), line, ps - index) then
            FTokens.Add(TPLXToken.Create(xtkIdentifier, pom, line, ps - index));
        end else if AExpression[p] = #39 then begin
          ps := p;
          p += 1;

          while IsNotEnd and not (AExpression[p] in [#13, #39]) do begin
            pom += AExpression[p];
            p += 1;
          end;

          if IsNotEnd and (AExpression[p] = #39) then begin
            FTokens.Add(TPLXToken.Create(xtkString, pom, line, ps - index));
          end else begin
            FLastError := '(%d, %d) Unexpected ending of "%s" string'.Format([line, ps - index, pom]);
            AddEOF;
            exit;
          end;
        end else begin
          FLastError := '(%d, %d) Illegal character: "%s"'.Format([line, p - index, AExpression[p]]);
          AddEOF;
          exit;
        end;
      end;
    end;
  end;

  AddEOF;
end;

function TPLXLexer.IsEOF: TPLBool;
begin
  Result := (FTokens.Count <= FIndex) or (FToken.Kind = xtkEOF);
end;

function TPLXLexer.Consume(AKind: TPLXTokenKind): TPLBool;
begin
  Result := AKind = FToken.Kind;
  if Result then MoveForward;
end;

function TPLXLexer.Peek: IPLXToken;
begin
  if FIndex+1 < FTokens.Count then Result := FTokens[FIndex+1]
  else Result := FTokens.Last;
end;

procedure TPLXLexer.MoveForward;
begin
  FIndex += 1;
  if not IsEOF then FToken := FTokens[FIndex] else FToken := nil;
end;

constructor TPLXLexer.Create(const AExpression: TPLString);
begin
  inherited Create;

  FTokens := TPLXTokens.Create;
  FIndex := 0;
  LexExpression(AExpression);
end;

{ TPLXValue }

constructor TPLXValue.NewInteger(const AValue: TPLInt);
begin
  FKind := xvkInteger;
  FNumber := AValue;
  FList := nil;
end;

constructor TPLXValue.NewFloat(const AValue: TPLFloat);
begin
  FKind := xvkFloat;
  FNumber := AValue;
  FList := nil;
end;

constructor TPLXValue.NewString(const AValue: TPLString);
begin
  FKind := xvkString;
  FNumber := AValue;
  FList := nil;
end;

constructor TPLXValue.NewBoolean(const AValue: TPLBool);
begin
  FKind := xvkBoolean;
  FNumber := ifthen(AValue, '1', '0');
  FList := nil;
end;

constructor TPLXValue.NewHTMLObjects(const AValue: IPLHTMLObjects);
begin
  FKind := xvkHTMLObjects;
  FNumber := '';
  FList := AValue;
end;

function TPLXValue.ToInteger: TPLInt;
begin
  Result := FNumber;
end;

function TPLXValue.ToFloat: TPLFloat;
begin
  Result := FNumber;
end;

function TPLXValue.ToString: TPLString;
begin
  Result := FNumber;
end;

function TPLXValue.ToBoolean: TPLBool;
begin
  Result := (ToString <> '') or (ToFloat <> 0);
end;

function TPLXValue.ToHTMLObjects: IPLHTMLObjects;
begin
  Result := FList;
end;

function TPLXValue.CanCastTo(AValue: TPLXValue): TPLBool;
begin
  Result := false;

  case FKind of
    xvkInteger, xvkFloat, xvkBoolean: Result := AValue.Kind in [xvkInteger, xvkFloat, xvkBoolean];
    xvkString: Result := AValue.Kind = xvkString;
    xvkHTMLObjects: Result := AValue.Kind = xvkHTMLObjects;
  end;
end;

function TPLXValue.IsNull: TPLBool;
begin
  Result := self = Null;
end;

class function TPLXValue.Null: TPLXValue;
begin
  Result.FKind := xvkString;
  Result.FNumber := '<TPLXValue.Null:=nil>';
  Result.FList := nil;
end;

class operator TPLXValue.=(a, b: TPLXValue) r: TPLBool;
begin
  r := a.CanCastTo(b) and (a.FKind = b.FKind) and (a.FNumber = b.FNumber)
    and (a.FList = b.FList);
end;

{ TPLXContext }

constructor TPLXContext.Create(ADoc: IPLHTMLDocument; AObj: TPLHTMLObject;
  APos: SizeInt);
begin
  FHTMLDocument := ADoc;
  FHTMLObject := AObj;
  FPosition := APos;
end;

{ TPLXParameters }

function TPLXParameters.Evaluate(AContext: TPLXContext): IPLXValues;
var
  v: TPLXExpression;
begin
  Result := TPLXValues.Create;
  for v in self do Result.Add(v.Evaluate(AContext));
end;

{ TPLXEvaluation }

function TPLXEvaluation.Operation(AOperator: TPLXOperator; const A, B: TPLXValue
  ): TPLXValue;
begin
  Result := TPLXValue.Null;

  case AOperator of
    xoNone: Result := A;
    xoPlus: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewInteger(A.ToInteger + B.ToInteger);
        xvkFloat: Result := TPLXValue.NewFloat(A.ToFloat + B.ToFloat);
        xvkString: Result := TPLXValue.NewString(A.ToString + B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToFloat + B.ToFloat <> 0);
      end;
    end;
    xoMinus: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewInteger(A.ToInteger - B.ToInteger);
        xvkFloat: Result := TPLXValue.NewFloat(A.ToFloat - B.ToFloat);
        xvkString: Result := TPLXValue.NewString(A.ToString.Replace(B.ToString, ''));
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToFloat - B.ToFloat <> 0);
      end;
    end;
    xoMultiply: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewInteger(A.ToInteger * B.ToInteger);
        xvkFloat: Result := TPLXValue.NewFloat(A.ToFloat * B.ToFloat);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToFloat * B.ToFloat <> 0);
      end;
    end;
    xoFloatDivide: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewFloat(A.ToInteger / B.ToInteger);
        xvkFloat, xvkBoolean: Result := TPLXValue.NewFloat(A.ToFloat / B.ToFloat);
      end;
    end;
    xoIntegerDivide: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewInteger(A.ToInteger div B.ToInteger);
        xvkFloat, xvkBoolean: Result := TPLXValue.NewFloat(trunc(A.ToFloat / B.ToFloat));
      end;
    end;
    xoModulo: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewFloat(A.ToInteger mod B.ToInteger);
        xvkFloat, xvkBoolean: Result := TPLXValue.NewFloat(A.ToFloat mod B.ToFloat);
      end;
    end;
    xoOr: Result := TPLXValue.NewBoolean(A.ToBoolean or B.ToBoolean);
    xoAnd: Result := TPLXValue.NewBoolean(A.ToBoolean and B.ToBoolean);
    xoNegate: begin
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewInteger(-A.ToInteger);
        xvkFloat: Result := TPLXValue.NewFloat(-A.ToFloat);
        xvkString: Result := TPLXValue.NewString(ReverseString(A.ToString));
        xvkBoolean: Result := TPLXValue.NewBoolean(not A.ToBoolean);
      end;
    end;
    xoEqual: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(false) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger = B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat = B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString = B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean = B.ToBoolean);
      end;
    end;
    xoNotEqual: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(true) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger <> B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat <> B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString <> B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean <> B.ToBoolean);
      end;
    end;
    xoLess: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(false) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger < B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat < B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString < B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean < B.ToBoolean);
      end;
    end;
    xoGreater: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(false) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger > B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat > B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString > B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean > B.ToBoolean);
      end;
    end;
    xoLessOrEqual: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(false) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger <= B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat <= B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString <= B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean <= B.ToBoolean);
      end;
    end;
    xoGreaterOrEqual: begin
      if not A.CanCastTo(B) then Result := TPLXValue.NewBoolean(false) else
      case A.Kind of
        xvkInteger: Result := TPLXValue.NewBoolean(A.ToInteger >= B.ToInteger);
        xvkFloat: Result := TPLXValue.NewBoolean(A.ToFloat >= B.ToFloat);
        xvkString: Result := TPLXValue.NewBoolean(A.ToString >= B.ToString);
        xvkBoolean: Result := TPLXValue.NewBoolean(A.ToBoolean >= B.ToBoolean);
      end;
    end;
  end;
end;

function TPLXEvaluation.Evaluate(AContext: TPLXContext): TPLXValue;
begin
  Result := TPLXValue.Null;
end;

{ TPLXExpression }

constructor TPLXExpression.Create(const AKind: TPLXExpressionKind;
  AOperand: IPLXEvaluation);
begin
  inherited Create;

  FKind := AKind;
  FOperand := AOperand;
  FElements := TPLXExpressionElements.Create;
end;

function TPLXExpression.Evaluate(AContext: TPLXContext): TPLXValue;

  function Path: TPLXValue;
  var
    e: IPLXEvaluation;
    obj, x: TPLHTMLObject;
    objr: TPLXValue;
    lin, lout: IPLHTMLObjects;
    i: SizeInt = 0;
  begin
    lin := TPLHTMLObjects.Create;
    lin.Add(AContext.HTMLObject);
    lout := TPLHTMLObjects.Create;
    e := FOperand;

    while Assigned(e) do begin
      for obj in lin do begin
        objr := e.Evaluate(TPLXContext.Create(AContext.HTMLDocument, obj, 1));
        if objr.Kind = xvkHTMLObjects then
          for x in objr.ToHTMLObjects do lout.Add(x);
      end;

      e := nil;
      if i < FElements.Count then begin
        lin.Clear;
        for x in lout do lin.Add(x);
        lout.Clear;
        e := FElements[i].Value;
        i += 1;
      end;
    end;

    Result := TPLXValue.NewHTMLObjects(lout);
  end;

  function Expr: TPLXValue;
  var
    e: TPLXExpressionElement;
  begin
    Result := FOperand.Evaluate(AContext);

    for e in FElements do
      Result := Operation(e.Key, Result, e.Value.Evaluate(AContext));
  end;

begin
  case FKind of
    xekPath: Result := Path;
    xekExpression: Result := Expr;
    xekNegation: Result := Operation(xoNegate, FOperand.Evaluate(AContext), TPLXValue.Null);
    xekRoot: Result := FOperand.Evaluate(TPLXContext.Create(AContext.HTMLDocument, AContext.HTMLDocument.Root.ToObject, 1));
  end;
end;

procedure TPLXExpression.AddElement(const AOperator: TPLXOperator;
  AOperand: IPLXEvaluation);
begin
  FElements.Add(TPLXExpressionElement.Create(AOperator, AOperand));
end;

{ TPLXPrimaryExpression }

constructor TPLXPrimaryExpression.Create(AToken: IPLXToken);
begin
  inherited Create;

  case AToken.Kind of
    xtkIdentifier: FKind := xpekVariable;
    xtkDot: FKind := xpekContext;
    else FKind := xpekValue;
  end;
  FExpression := nil;
  FParameters := nil;
  FToken := AToken.Clone;
end;

constructor TPLXPrimaryExpression.Create(AToken: IPLXToken;
  AParameters: IPLXParameters);
begin
  inherited Create;

  FKind := xpekFunction;
  FExpression := nil;
  FParameters := AParameters;
  FToken := AToken.Clone;
end;

constructor TPLXPrimaryExpression.Create(AExpression: TPLXExpression);
begin
  inherited Create;

  FKind := xpekExpression;
  FExpression := AExpression;
  FParameters := nil;
  FToken := nil;
end;

destructor TPLXPrimaryExpression.Destroy;
begin
  if Assigned(FExpression) then FreeAndNil(FExpression);

  inherited Destroy;
end;

function TPLXPrimaryExpression.Evaluate(AContext: TPLXContext): TPLXValue;

  function Func: TPLXValue;
  var
    cp: IPLXValues;
  begin
    Result := TPLXValue.Null;
    cp := FParameters.Evaluate(AContext);

    case FToken.Text of
      'true': Result := TPLXValue.NewBoolean(true);
      'false': Result := TPLXValue.NewBoolean(false);
      'text': begin
        if Assigned(AContext.HTMLObject) then Result := TPLXValue.NewString(AContext.HTMLObject.Text)
        else Result := TPLXValue.NewString('');
      end;
      'position': Result := TPLXValue.NewInteger(AContext.Position);
      'not': begin
        if cp.Count > 0 then Result := TPLXValue.NewBoolean(not cp[0].ToBoolean);
      end;
      'count': begin
        if (cp.Count > 0) and (cp[0].Kind = xvkHTMLObjects) then Result := TPLXValue.NewInteger(cp[0].ToHTMLObjects.Count);
      end;
      'state': begin
        if Assigned(AContext.HTMLObject) then Result := TPLXValue.NewString(AContext.HTMLObject.State)
        else Result := TPLXValue.NewString(esNormal);
      end;
    end;
  end;

begin
  Result := TPLXValue.Null;

  case FKind of
    xpekExpression: Result := FExpression.Evaluate(AContext);
    xpekValue: begin
      case FToken.Kind of
        xtkInteger: Result := TPLXValue.NewInteger(FToken.ToInteger);
        xtkFloat: Result := TPLXValue.NewFloat(FToken.ToFloat);
        xtkString: Result := TPLXValue.NewString(FToken.Text);
      end;
    end;
    xpekFunction: Result := Func;
  end;
end;

{ TPLXNodeTest }

constructor TPLXNodeTest.CreateIdentifierTest(AToken: IPLXToken);
begin
  inherited Create;

  FKind := xntIdentifier;
  FToken := AToken.Clone;
  FTokenKind := xtkIdentifier;
end;

constructor TPLXNodeTest.CreateKindTest(AKind: TPLXTokenKind);
begin
  inherited Create;

  FKind := xntKind;
  FToken := nil;
  FTokenKind := AKind;
end;

constructor TPLXNodeTest.CreateAnyTest;
begin
  inherited Create;

  FKind := xntAny;
  FToken := nil;
  FTokenKind := xtkMultiply;
end;

function TPLXNodeTest.Evaluate(AContext: TPLXContext): TPLXValue;
var
  objs: IPLHTMLObjects;
begin
  Result := TPLXValue.Null;

  case FKind of
    xntKind: begin
      case FTokenKind of
        xtkNode: begin
          objs := TPLHTMLObjects.Create;
          objs.Add(AContext.HTMLObject);
          Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
        end;
        xtkText: Result := TPLXValue.NewString(AContext.HTMLObject.Text);
      end;
    end;
    xntAny: Result := TPLXValue.NewHTMLObjects(AContext.HTMLObject.Children.Duplicate);
  end;
end;

function TPLXNodeTest.EvaluateAxis(AAxis: TPLXTokenKind; AContext: TPLXContext
  ): TPLXValue;
var
  objs: IPLHTMLObjects;

  procedure ProcessNode(A: TPLHTMLObject; B: IPLHTMLObjects);
  begin
    case FKind of
      xntIdentifier: begin
        if ((AAxis = xtkAttribute) and (A.Attributes.Has(FToken.Text))) or (A.Name = FToken.Text) then
          B.Add(A);
      end;
      xntKind: begin
        case FTokenKind of
          xtkNode: B.Add(A);
        end;
      end;
      xntAny: B.Add(A);
    end;
  end;

  procedure ProcessAxis(A, B: IPLHTMLObjects);
  var
    obj: TPLHTMLObject;
  begin
    for obj in A do begin
      ProcessNode(obj, B);
      if AAxis in [xtkDescendant, xtkDescendantOrSelf] then
        ProcessAxis(obj.Children, B);
    end;
  end;

begin
  objs := TPLHTMLObjects.Create;

  case AAxis of
    xtkAttribute: begin
      if AContext.HTMLObject.Attributes.Has(FToken.Text) then
        Result := TPLXValue.NewString(AContext.HTMLObject.Attributes[FToken.Text].Value)
      else
        Result := TPLXValue.NewBoolean(false);
    end;
    xtkSelf: begin
      ProcessNode(AContext.HTMLObject, objs);
      Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
    end;
    xtkParent: begin
      ProcessAxis(AContext.HTMLObject.Parent.Children.Duplicate, objs);
      Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
    end;
    xtkChild, xtkDescendant: begin
      ProcessAxis(AContext.HTMLObject.Children.Duplicate, objs);
      Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
    end;
    xtkDescendantOrSelf: begin
      ProcessNode(AContext.HTMLObject, objs);
      ProcessAxis(AContext.HTMLObject.Children.Duplicate, objs);
      Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
    end;
  end;
end;

{ TPLXStep }

constructor TPLXStep.Create(ATest: TPLXNodeTest; AAxis: TPLXTokenKind);
begin
  inherited Create;

  FTest := ATest;
  FAxis := AAxis;
end;

destructor TPLXStep.Destroy;
begin
  if Assigned(FTest) then FreeAndNil(FTest);

  inherited Destroy;
end;

function TPLXStep.Evaluate(AContext: TPLXContext): TPLXValue;
begin
  if FAxis = xtkEOF then
    Result := FTest.Evaluate(AContext)
  else
    Result := FTest.EvaluateAxis(FAxis, AContext);
end;

{ TPLXStepExpression }

constructor TPLXStepExpression.Create(AStep: IPLXEvaluation);
begin
  inherited Create;

  FStep := AStep;
  FPredicates := TPLXExpressions.Create(true);
end;

function TPLXStepExpression.Evaluate(AContext: TPLXContext): TPLXValue;
var
  obj: TPLHTMLObject;
  objs: IPLHTMLObjects;
  pos: SizeInt = 1;
  inc: TPLBool;
  pr: TPLXExpression;
  prr: TPLXValue;
begin
  Result := FStep.Evaluate(AContext);

  if FPredicates.Count > 0 then begin
    objs := TPLHTMLObjects.Create;

    for obj in Result.ToHTMLObjects do begin
      inc := true;

      for pr in FPredicates do begin
        prr := pr.Evaluate(TPLXContext.Create(AContext.HTMLDocument, obj, pos));
        if prr.Kind = xvkInteger then
          inc := inc and (prr.ToInteger = pos)
        else
          inc := inc and prr.ToBoolean;
      end;

      pos += 1;
      if inc then objs.Add(obj);
    end;

    Result := TPLXValue.NewHTMLObjects(objs.Duplicate);
  end;
end;

procedure TPLXStepExpression.AddPredicate(APredicate: TPLXExpression);
begin
  FPredicates.Add(APredicate);
end;

{ TPLXPath }

class function TPLXPath.ConvertCSSToXPath(ACSS: TPLString): TPLString;
begin
  Result := '';
end;

class function TPLXPath.ParseXPath(AXPath: TPLString): TPLXEvaluation;
var
  lex: TPLXLexer;
begin
  lex := TPLXLexer.Create(AXPath);
  try
    Result := ParseExpression(lex);
  finally
    lex.Free;
  end;
end;

class function TPLXPath.ParseExpression(ALexer: TPLXLexer): TPLXExpression;
var
  op: TPLXEvaluation;
begin
  Result := ParseAnd(ALexer);

  if ALexer.Token.Kind = xtkOr then begin
    Result := TPLXExpression.Create(xekExpression, Result);

    while ALexer.Token.Kind = xtkOr do begin
      ALexer.MoveForward;
      op := ParseAnd(ALexer);
      Result.AddElement(xoOr, op);
    end;
  end;
end;

class function TPLXPath.ParseAnd(ALexer: TPLXLexer): TPLXExpression;
var
  op: TPLXEvaluation;
begin
  Result := ParseComparison(ALexer);

  if ALexer.Token.Kind = xtkAnd then begin
    Result := TPLXExpression.Create(xekExpression, Result);

    while ALexer.Token.Kind = xtkAnd do begin
      ALexer.MoveForward;
      op := ParseAnd(ALexer);
      Result.AddElement(xoAnd, op);
    end;
  end;
end;

class function TPLXPath.ParseComparison(ALexer: TPLXLexer): TPLXExpression;
var
  op: TPLXEvaluation;
  xop: TPLXOperator;
begin
  Result := ParsePlusMinus(ALexer);

  if ALexer.Token.Kind in XTokenKindCategory('compare') then begin
    Result := TPLXExpression.Create(xekExpression, Result);

    while ALexer.Token.Kind in XTokenKindCategory('compare') do begin
      case ALexer.Token.Kind of
        xtkEquals: xop := xoEqual;
        xtkNotEquals: xop := xoNotEqual;
        xtkLess: xop := xoLess;
        xtkGreater: xop := xoGreater;
        xtkLessEqual: xop := xoLessOrEqual;
        xtkGreaterEqual: xop := xoGreaterOrEqual;
      end;
      ALexer.MoveForward;
      op := ParsePlusMinus(ALexer);
      Result.AddElement(xop, op);
    end;
  end;
end;

class function TPLXPath.ParsePlusMinus(ALexer: TPLXLexer): TPLXExpression;
var
  op: TPLXEvaluation;
  xop: TPLXOperator;
begin
  Result := ParseMultiplicative(ALexer);

  if ALexer.Token.Kind in XTokenKindCategory('+/-') then begin
    Result := TPLXExpression.Create(xekExpression, Result);

    while ALexer.Token.Kind in XTokenKindCategory('+/-') do begin
      case ALexer.Token.Kind of
        xtkPlus: xop := xoPlus;
        xtkMinus: xop := xoMinus;
      end;
      ALexer.MoveForward;
      op := ParseMultiplicative(ALexer);
      Result.AddElement(xop, op);
    end;
  end;
end;

class function TPLXPath.ParseMultiplicative(ALexer: TPLXLexer): TPLXExpression;
var
  op: TPLXEvaluation;
  xop: TPLXOperator;
begin
  Result := ParseUnary(ALexer);

  if ALexer.Token.Kind in XTokenKindCategory('multi') then begin
    Result := TPLXExpression.Create(xekExpression, Result);

    while ALexer.Token.Kind in XTokenKindCategory('multi') do begin
      case ALexer.Token.Kind of
        xtkMultiply: xop := xoMultiply;
        xtkDivF: xop := xoFloatDivide;
        xtkDivI: xop := xoIntegerDivide;
        xtkMod: xop := xoModulo;
      end;
      ALexer.MoveForward;
      op := ParseUnary(ALexer);
      Result.AddElement(xop, op);
    end;
  end;
end;

class function TPLXPath.ParseUnary(ALexer: TPLXLexer): TPLXExpression;
var
  neg: TPLBool = false;
begin
  while ALexer.Token.Kind in XTokenKindCategory('+/-') do begin
    if ALexer.Token.Kind = xtkMinus then neg := not neg;
    ALexer.MoveForward;
  end;

  Result := ParsePath(ALexer);

  if neg then begin
    Result := TPLXExpression.Create(xekNegation, Result);
    Result.AddElement(xoNegate, nil);
  end;
end;

class function TPLXPath.ParsePath(ALexer: TPLXLexer): TPLXExpression;
var
  cs, p: TPLXExpression;
  s: TPLXStep;
begin
  case ALexer.Token.Kind of
    xtkSlash: begin
      ALexer.MoveForward;
      Result := TPLXExpression.Create(xekRoot, ParseRelativePath(ALexer));
    end;
    xtkDoubleSlash: begin
      ALexer.MoveForward;
      cs := ParseRelativePath(ALexer);
      s := TPLXStep.Create(TPLXNodeTest.CreateKindTest(xtkNode), xtkDescendantOrSelf);
      p := TPLXExpression.Create(xekPath, s);
      p.AddElement(xoStep, cs);
      Result := TPLXExpression.Create(xekRoot, p);
    end;
    else Result := ParseRelativePath(ALexer);
  end;
end;

class function TPLXPath.ParseRelativePath(ALexer: TPLXLexer): TPLXExpression;
begin
  Result := TPLXExpression.Create(xekExpression, ParseStepExpression(ALexer));

  while ALexer.Token.Kind in XTokenKindCategory('slashes') do begin
    Result.Kind := xekPath;
    ALexer.MoveForward;

    case ALexer.Token.Kind of
      xtkSlash: Result.AddElement(xoStep, ParseStepExpression(ALexer));
      xtkDoubleSlash: begin
        Result.AddElement(xoStep, TPLXExpression.Create(xekPath, TPLXStep.Create(TPLXNodeTest.CreateKindTest(xtkNode), xtkDescendantOrSelf)));
        Result.AddElement(xoStep, ParseRelativePath(ALexer));
      end;
    end;
  end;
end;

class function TPLXPath.ParseStep(ALexer: TPLXLexer): TPLXEvaluation;
var
  tk: IPLXToken;
  tkk: TPLXTokenKind;
  nt: TPLXNodeTest;
begin
  case ALexer.Token.Kind of
    xtkDoubleDot: begin
      ALexer.MoveForward;
      tk := TPLXToken.Create(xtkNode, 'node', -1, -1);
      Result := TPLXStep.Create(TPLXNodeTest.CreateKindTest(tk.Kind), xtkParent);
    end;
    xtkAt: begin
      ALexer.MoveForward;
      Result := TPLXStep.Create(ParseNodeTest(ALexer), xtkAttribute);
    end;
    xtkParent..xtkDescendantOrSelf: begin
      tkk := ALexer.Token.Kind;
      ALexer.MoveForward;
      ALexer.Consume(xtkDoubleColon);
      Result := TPLXStep.Create(ParseNodeTest(ALexer), tkk);
    end;
    else begin
      nt := ParseNodeTest(ALexer);
      if nt.Kind = xntIdentifier then
        Result := TPLXStep.Create(nt, xtkChild)
      else
        Result := TPLXStep.Create(nt);
    end;
  end;
end;

class function TPLXPath.ParseStepExpression(ALexer: TPLXLexer): TPLXEvaluation;
begin
  if ((ALexer.Token.Kind = xtkIdentifier) and (ALexer.Peek.Kind <> xtkParenthesisStart))
    or (ALexer.Token.Kind in [xtkText, xtkComment, xtkAt, xtkMultiply, xtkNamespace, xtkParent..xtkProcessingInstruction]) then
    Result := ParseStep(ALexer)
  else Result := ParsePrimary(ALexer);

  Result := TPLXStepExpression.Create(Result);
  while ALexer.Token.Kind = xtkBracketStart do begin
    ALexer.Consume(xtkBracketStart);
    TPLXStepExpression(Result).AddPredicate(ParseExpression(ALexer));
    ALexer.Consume(xtkBracketEnd);
  end;
end;

class function TPLXPath.ParsePrimary(ALexer: TPLXLexer): TPLXPrimaryExpression;
var
  e: TPLXExpression;
  t: IPLXToken;
  p: TPLXParameters;
  r: TPLBool = true;
begin
  case ALexer.Token.Kind of
    xtkInteger, xtkFloat, xtkString: begin
      Result := TPLXPrimaryExpression.Create(ALexer.Token);
      ALexer.MoveForward;
    end;
    xtkIdentifier: begin
      if ALexer.Peek.Kind = xtkParenthesisStart then begin
        t := ALexer.Token;
        ALexer.MoveForward;
        ALexer.Consume(xtkParenthesisStart);
        p := ParseParameters(ALexer);
        ALexer.Consume(xtkParenthesisEnd);
        Result := TPLXPrimaryExpression.Create(t, p);
      end else r := false;
    end;
    xtkDollar: begin
      ALexer.MoveForward;
      if ALexer.Token.Kind = xtkIdentifier then
        Result := TPLXPrimaryExpression.Create(ALexer.Token)
      else r := false;
      ALexer.MoveForward;
    end;
    xtkDot: begin
      ALexer.MoveForward;
      Result := TPLXPrimaryExpression.Create(TPLXExpression.Create(xekExpression, TPLXStep.Create(TPLXNodeTest.CreateKindTest(xtkNode), xtkSelf)));
    end;
    xtkParenthesisStart: begin
      ALexer.MoveForward;
      e := ParseExpression(ALexer);
      ALexer.Consume(xtkParenthesisEnd);
      Result := TPLXPrimaryExpression.Create(e);
    end;
    else ALexer.MoveForward;
  end;

  if not r then begin
    t := TPLXToken.Create(xtkEOF, ALexer.Token.Text, ALexer.Token.Line, ALexer.Token.LineIndex);
    Result := TPLXPrimaryExpression.Create(t);
  end;
end;

class function TPLXPath.ParseNodeTest(ALexer: TPLXLexer): TPLXNodeTest;
begin
  case ALexer.Token.Kind of
    xtkIdentifier: begin
      Result := TPLXNodeTest.CreateIdentifierTest(ALexer.Token);
      ALexer.MoveForward;
    end;
    xtkMultiply: begin
      Result := TPLXNodeTest.CreateAnyTest;
      ALexer.MoveForward;
    end;
    else begin
      Result := TPLXNodeTest.CreateKindTest(ALexer.Token.Kind);
      ALexer.MoveForward;
      ALexer.Consume(xtkParenthesisStart);
      ALexer.Consume(xtkParenthesisEnd);
    end;
  end;
end;

class function TPLXPath.ParseParameters(ALexer: TPLXLexer): TPLXParameters;
begin
  Result := TPLXParameters.Create;

  if not (ALexer.Token.Kind in [xtkParenthesisEnd, xtkEOF]) then
    Result.Add(ParseExpression(ALexer));

  while ALexer.Token.Kind = xtkComma do begin
    ALexer.Consume(xtkComma);
    Result.Add(ParseExpression(ALexer));
  end;
end;

class function TPLXPath.EvaluateXPath(ADocument: IPLHTMLDocument;
  AXPath: TPLString): TPLXValue;
var
  p: IPLXEvaluation;
begin
  p := ParseXPath(AXPath);
  Result := p.Evaluate(TPLXContext.Create(ADocument, ADocument.Root.ToObject, 1));
end;

class function TPLXPath.EvaluateCSS(ADocument: IPLHTMLDocument; ACSS: TPLString
  ): TPLXValue;
begin
  Result := EvaluateXPath(ADocument, ConvertCSSToXPath(ACSS));
end;

end.

