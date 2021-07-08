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
    class operator =(a, b: TPLXValue) r: TPLBool;

    property Kind: TPLXValueKind read FKind write FKind;
  end;

  IPLXValues = specialize IPLList<TPLXValue>;
  TPLXValues = class(specialize TPLList<TPLXValue>, IPLXValues);

  { TPLXContext }

  TPLXContext = packed record
  strict private
    FHTMLDocument: IPLHTMLDocument;
    FHTMLObject: IPLHTMLObject;
    FPosition: SizeInt;
  public
    constructor Create(ADoc: IPLHTMLDocument; AObj: IPLHTMLObject; APos: SizeInt);

    property HTMLDocument: IPLHTMLDocument read FHTMLDocument write FHTMLDocument;
    property HTMLObject: IPLHTMLObject read FHTMLObject write FHTMLObject;
    property Position: SizeInt read FPosition write FPosition;
  end;

  IPLXParameters = specialize IPLObjectList<TPLXExpression>;

  { TPLXParameters }

  TPLXParameters = class(specialize TPLObjectList<TPLXExpression>, IPLXParameters)
  public
    function Evaluate(AContext: TPLXContext): IPLXValues;
  end;

  TPLXOperator = (xoNone, xoPlus, xoMinus, xoMultiply, xoFloatDivide,
    xoIntegerDivide, xoModulo, xoOr, xoAnd, xoNegate, xoStep, xoEqual,
    xoNotEqual, xoLess, xoGreater, xoLessOrEqual, xoGreaterOrEqual);

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
    function Evaluate(AContext: TPLXContext): TPLXValue; virtual;
  end;

  IPLXEvaluations = specialize IPLList<IPLXEvaluation>;
  TPLXEvaluations = class(specialize TPLList<IPLXEvaluation>, IPLXEvaluations);

  { TPLXExpression }

  TPLXExpression = class(TPLXEvaluation)

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
    xtkElementState: r := 'element-state';
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
    'element-state': r := xtkElementState;
    else r := xtkEOF;
  end;
end;

function XTokenKindCategory(const ACategory: TPLString): TPLXTokenKinds;
begin
  case ACategory of
    'single': Result := [xtkBracketStart..xtkParenthesisEnd, xtkDot, xtkSlash, xtkDollar..xtkEquals, xtkLess, xtkGreater];
    'double': Result := [xtkDoubleColon, xtkDoubleDot, xtkDoubleSlash, xtkLessEqual, xtkGreaterEqual];
    'word': Result := [xtkText, xtkComment, xtkOr..xtkElementState];
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
  Result := ToFloat <> 0;
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

class operator TPLXValue.=(a, b: TPLXValue) r: TPLBool;
begin
  r := a.CanCastTo(b) and (a.FKind = b.FKind) and (a.FNumber = b.FNumber);
end;

{ TPLXContext }

constructor TPLXContext.Create(ADoc: IPLHTMLDocument; AObj: IPLHTMLObject;
  APos: SizeInt);
begin
  FHTMLDocument := ADoc;
  FHTMLObject := AObj;
  FPosition := APos;
end;

{ TPLXParameters }

function TPLXParameters.Evaluate(AContext: TPLXContext): IPLXValues;
begin

end;

{ TPLXEvaluation }

function TPLXEvaluation.Operation(AOperator: TPLXOperator; const A, B: TPLXValue
  ): TPLXValue;
begin

end;

function TPLXEvaluation.Evaluate(AContext: TPLXContext): TPLXValue;
begin
  //
end;

end.

