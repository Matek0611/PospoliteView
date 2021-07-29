unit Pospolite.View.CSS.Selector;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, math, Pospolite.View.Basics, Pospolite.View.CSS.Declaration;

type

  { TPLCSSSelectorContext }

  TPLCSSSelectorContext = packed record
  strict private
    FFinish: SizeInt;
    FSelector: TPLString;
    FStart: SizeInt;
    FValue: TPLString;
  public
    constructor Create(const ASelector, AValue: TPLString; const AStart, AFinish: SizeInt);

    property Selector: TPLString read FSelector write FSelector;
    property Value: TPLString read FValue write FValue;
    property Start: SizeInt read FStart write FStart;
    property Finish: SizeInt read FFinish write FFinish;
  end;

  { TPLCSSSelectorContextable }

  TPLCSSSelectorContextable = packed class
  protected
    FContext: TPLCSSSelectorContext;
  public
    constructor Create(const AContext: TPLCSSSelectorContext); virtual;

    property Context: TPLCSSSelectorContext read FContext write FContext;
  end;

  { TPLCSSSimpleSelector }

  TPLCSSSimpleSelector = packed class(TPLCSSSelectorContextable);
  TPLCSSSimpleSelectors = packed class(specialize TPLObjectList<TPLCSSSimpleSelector>);

  { TPLCSSSimpleSelectorPattern }

  TPLCSSSimpleSelectorPattern = packed class(TPLCSSSelectorContextable)
  private
    FSimpleSelectors: TPLCSSSimpleSelectors;
  public
    constructor Create(const AContext: TPLCSSSelectorContext); override;
    destructor Destroy; override;

    function AppliesTo(constref AObject: TPLHTMLObject): TPLBool;

    property SimpleSelectors: TPLCSSSimpleSelectors read FSimpleSelectors;
  end;

  TPLCSSSimpleSelectorPatterns = packed class(specialize TPLObjectList<TPLCSSSimpleSelectorPattern>);

  TPLCSSSelectorExpressionSignal = (sesPlus = '+', sesMinus = '-');
  TPLCSSSelectorExpressionKind = (sekIdentifier, sekNumber, sekString, sekSignal);
  TPLCSSSelectorExpressionItem = specialize TPLParameter<TPLCSSSelectorExpressionKind, TPLString>;
  TPLCSSSelectorExpressionItems = class(specialize TPLList<TPLCSSSelectorExpressionItem>);

  { TPLCSSSelectorExpression }

  TPLCSSSelectorExpression = packed record
  strict private
    FFirst, FSecond: TPLInt;
    FText: TPLString;
  public
    constructor Create(const AFirst, ASecond: TPLInt; const AText: TPLString);
    class function Null: TPLCSSSelectorExpression; static;
    class operator =(a, b: TPLCSSSelectorExpression) r: TPLBool;

    property First: TPLInt read FFirst;
    property Second: TPLInt read FSecond;
    property Text: TPLString read FText;
  end;

  { TPLCSSSelectorPseudoExpression }

  TPLCSSSelectorPseudoExpression = packed class(TPLCSSSelectorContextable)
  private
    FEvaluation: TPLCSSSelectorExpression;
    FItems: TPLCSSSelectorExpressionItems;
    FText: TPLString;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AText: TPLString;
      const AEvaluation: TPLCSSSelectorExpression); reintroduce;
    destructor Destroy; override;

    function IsValidAsGroup: TPLBool; inline;

    property Text: TPLString read FText write FText;
    property Evaluation: TPLCSSSelectorExpression read FEvaluation write FEvaluation;
    property Items: TPLCSSSelectorExpressionItems read FItems;
  end;

  { TPLCSSSelectorPseudoEvaluator }

  TPLCSSSelectorPseudoEvaluator = packed class sealed
  public
    class function Evaluate(AList: TPLCSSSelectorExpressionItems): TPLCSSSelectorExpression;
  end;

  TPLCSSSelectorPseudoKind = (pskClass, pskElement);

  TPLCSSSelectorPseudo = packed class(TPLCSSSimpleSelector)
  private
    FExpression: TPLCSSSelectorPseudoExpression;
    FKind: TPLCSSSelectorPseudoKind;
    FName: TPLString;
    FNamespaced: TPLBool;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AName: TPLString;
      const AExpression: TPLCSSSelectorPseudoExpression; const AKind: TPLCSSSelectorPseudoKind;
      const ANamespaced: TPLBool); reintroduce;
    destructor Destroy; override;

    property Name: TPLString read FName;
    property Expression: TPLCSSSelectorPseudoExpression read FExpression;
    property Kind: TPLCSSSelectorPseudoKind read FKind;
    property Namespaced: TPLBool read FNamespaced;
  end;

  { TPLCSSSelectorString }

  TPLCSSSelectorString = packed class(TPLCSSSelectorContextable)
  private
    FValue: TPLString;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AValue: TPLString); reintroduce;

    property Value: TPLString read FValue write FValue;
  end;

  { TPLCSSSelectorAttribute }

  TPLCSSSelectorAttribute = packed class(TPLCSSSimpleSelector)
  private
    FCaseSensitive: TPLBool;
    FComparator: TPLString;
    FName: TPLString;
    FValue: TPLCSSSelectorString;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AName,
      AComparator: TPLString; const AValue: TPLCSSSelectorString;
      const ACaseSensitive: TPLBool = false); reintroduce;
    destructor Destroy; override;

    property Name: TPLString read FName;
    property Comparator: TPLString read FComparator;
    property Value: TPLCSSSelectorString read FValue;
    property CaseSensitive: TPLBool read FCaseSensitive;
  end;

  { TPLCSSSelectorClass }

  TPLCSSSelectorClass = packed class(TPLCSSSimpleSelector)
  private
    FName: TPLString;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AName: TPLString); reintroduce;

    property Name: TPLString read FName;
  end;

  { TPLCSSSelectorType }

  TPLCSSSelectorType = packed class(TPLCSSSelectorClass)
  public
    function IsUniversal: TPLBool; inline;
  end;

  { TPLCSSSelectorHash }

  TPLCSSSelectorHash = packed class(TPLCSSSelectorClass);

  { TPLCSSSelectorNegation }

  TPLCSSSelectorNegation = packed class(TPLCSSSelectorClass);

  TPLCSSSelectorCombinator = (scUndefined = 0, scDescendant = ' ', scChild = '>',
    scGeneralSibling = '~', scAdjascentSibling = '+');
  TPLCSSSelectorCombinatorItem = specialize TPLParameter<SizeInt, TPLCSSSelectorCombinator>;
  TPLCSSSelectorCombinators = packed class(specialize TPLList<TPLCSSSelectorCombinatorItem>);

  { TPLCSSSelector }

  TPLCSSSelector = packed class(TPLCSSSelectorContextable)
  private
    FCombinators: TPLCSSSelectorCombinators;
    FSimpleSelectors: TPLCSSSimpleSelectorPatterns;
  public
    constructor Create(const AContext: TPLCSSSelectorContext); override;
    destructor Destroy; override;

    property SimpleSelectors: TPLCSSSimpleSelectorPatterns read FSimpleSelectors;
    property Combinators: TPLCSSSelectorCombinators read FCombinators;
  end;

  IPLCSSSelectors = specialize IPLObjectList<TPLCSSSelector>;
  TPLCSSSelectors = packed class(specialize TPLObjectList<TPLCSSSelector>, IPLCSSSelectors);

  { TPLCSSSelectorParser }

  TPLCSSSelectorParser = packed class sealed
  public
    class function ParseSelector(ASelector: TPLString): TPLCSSSelectors;
  end;

implementation

uses character, strutils, LazUTF8;

{ TPLCSSSelectorContext }

constructor TPLCSSSelectorContext.Create(const ASelector, AValue: TPLString;
  const AStart, AFinish: SizeInt);
begin
  FSelector := ASelector;
  FValue := AValue;
  FStart := AStart;
  FFinish := AFinish;
end;

{ TPLCSSSelectorContextable }

constructor TPLCSSSelectorContextable.Create(
  const AContext: TPLCSSSelectorContext);
begin
  inherited Create;

  FContext := AContext;
end;

{ TPLCSSSimpleSelectorPattern }

constructor TPLCSSSimpleSelectorPattern.Create(
  const AContext: TPLCSSSelectorContext);
begin
  inherited Create(AContext);

  FSimpleSelectors := TPLCSSSimpleSelectors.Create();
end;

destructor TPLCSSSimpleSelectorPattern.Destroy;
begin
  FSimpleSelectors.Free;

  inherited Destroy;
end;

function TPLCSSSimpleSelectorPattern.AppliesTo(constref AObject: TPLHTMLObject
  ): TPLBool;
var
  i: SizeInt = 0;
  attr: TPLHTMLObjectAttribute;
  p1, p2: TPLString;
begin
  Result := false;
  if FSimpleSelectors.Empty then exit;

  if (FSimpleSelectors.First is TPLCSSSelectorType and
    TPLCSSSelectorType(FSimpleSelectors.First).IsUniversal) then Inc(i);

  while i < FSimpleSelectors.Count do begin
    if FSimpleSelectors[i] is TPLCSSSelectorType then begin
      if AObject.Name.ToLower <> TPLCSSSelectorType(FSimpleSelectors[i]).Name.ToLower then exit;
    end else if FSimpleSelectors[i] is TPLCSSSelectorHash then begin
      if AObject.Attributes.Id = Default(TPLHTMLObjectAttribute) then exit;
      if AObject.Attributes.Id.Value.ToLower <> TPLCSSSelectorHash(FSimpleSelectors[i]).Name.ToLower then exit;
    end else if FSimpleSelectors[i] is TPLCSSSelectorAttribute then begin
      if AObject.Attributes.Empty then exit;
      attr := AObject.Attributes.Get(TPLCSSSelectorAttribute(FSimpleSelectors[i]).Name);
      if (attr = Default(TPLHTMLObjectAttribute)) or TPLCSSSelectorAttribute(FSimpleSelectors[i]).Name.IsEmpty then exit;

      if TPLCSSSelectorAttribute(FSimpleSelectors[i]).Comparator.IsEmpty
        or not Assigned(TPLCSSSelectorAttribute(FSimpleSelectors[i]).Value) then begin
        Inc(i);
        continue;
      end;

      p1 := TPLCSSSelectorAttribute(FSimpleSelectors[i]).Value.Value;
      p2 := attr.Value;
      if not TPLCSSSelectorAttribute(FSimpleSelectors[i]).CaseSensitive then begin
        p1 := p1.ToLower;
        p2 := p2.ToLower;
      end;

      case TPLCSSSelectorAttribute(FSimpleSelectors[i]).Comparator of
        '=': if p1 <> p2 then exit;
        '~=': if (p2 <> p1) and not (p1 in p2.Split(' ')) then exit;
        '|=': if not ((p2 = p1) or (p2.StartsWith(p1+'-'))) then exit;
        '^=': if not p2.StartsWith(p1) then exit;
        '$=': if not p2.EndsWith(p1) then exit;
        '*=': if not p2.Exists(p1) then exit;
      end;
    end else if FSimpleSelectors[i] is TPLCSSSelectorPseudo then begin
      //... zrobić pseudo-elementy typu :hover, :active..., ::-webkit-scrollbar...
    end else if FSimpleSelectors[i] is TPLCSSSelectorClass then begin
      if AObject.Attributes.&Class = Default(TPLHTMLObjectAttribute) then exit;
      if not (TPLCSSSelectorClass(FSimpleSelectors[i]).Name.ToLower in AObject.Attributes.&Class.Value.ToLower.Split(' ')) then exit;
    end;

    Inc(i);
  end;

  Result := true;
end;

{ TPLCSSSelectorExpression }

constructor TPLCSSSelectorExpression.Create(const AFirst, ASecond: TPLInt;
  const AText: TPLString);
begin
  FFirst := AFirst;
  FSecond := ASecond;
  FText := AText;
end;

class function TPLCSSSelectorExpression.Null: TPLCSSSelectorExpression;
begin
  Result := Default(TPLCSSSelectorExpression);
end;

class operator TPLCSSSelectorExpression.=(a, b: TPLCSSSelectorExpression)
  r: TPLBool;
begin
  r := (a.First = b.First) and (a.Second = b.Second) and (a.Text = b.Text);
end;

{ TPLCSSSelectorPseudoExpression }

constructor TPLCSSSelectorPseudoExpression.Create(
  const AContext: TPLCSSSelectorContext; const AText: TPLString;
  const AEvaluation: TPLCSSSelectorExpression);
begin
  inherited Create(AContext);

  FText := AText;
  FEvaluation := AEvaluation;
  FItems := TPLCSSSelectorExpressionItems.Create;
end;

destructor TPLCSSSelectorPseudoExpression.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

function TPLCSSSelectorPseudoExpression.IsValidAsGroup: TPLBool;
begin
  Result := FEvaluation <> TPLCSSSelectorExpression.Null;
end;

{ TPLCSSSelectorPseudoEvaluator }

class function TPLCSSSelectorPseudoEvaluator.Evaluate(
  AList: TPLCSSSelectorExpressionItems): TPLCSSSelectorExpression;
var
  i: SizeInt = 0;
  s: TPLCSSSelectorExpressionSignal = sesPlus;
  c: TPLBool = false;
  a: TPLInt = 0;
  b: TPLInt = 0;

  function Current: TPLCSSSelectorExpressionItem;
  begin
    Result := AList[i];
  end;

  function IsEOF: TPLBool;
  begin
    Result := i >= AList.Count;
  end;

begin
  Result := TPLCSSSelectorExpression.Null;
  if not Assigned(AList) or AList.Empty then exit;
  if Current.Value in TPLStringFuncs.NewArray(['odd', 'even']) then exit(TPLCSSSelectorExpression.Create(2, ifthen(Current.Value = 'odd', 1, 0), ''));
  if (Current.Key = sekIdentifier) and (Current.Value <> 'n') then exit;

  if Current.Key = sekSignal then begin
    if Current.Value = '+' then s := sesPlus else s := sesMinus;
    Inc(i);
    if IsEOF then exit;
  end;

  if Current.Key = sekNumber then begin
    TryStrToInt64(Current.Value, a);
    c := true;
    Inc(i);
  end;

  if s = sesMinus then a := -a;
  if IsEOF then exit(TPLCSSSelectorExpression.Create(0, a, ''));

  if (Current.Key = sekIdentifier) and (Current.Value = 'n') then begin
    c := true;
    Inc(i);
  end;

  if not c then exit;
  if IsEOF then exit(TPLCSSSelectorExpression.Create(a, 0, ''));

  s := sesPlus;
  if Current.Key = sekSignal then begin
    if Current.Value = '+' then s := sesPlus else s := sesMinus;
    Inc(i);
    if IsEOF then exit;
  end;

  if Current.Key = sekNumber then begin
    TryStrToInt64(Current.Value, b);
    if s = sesMinus then b := -b;
    Inc(i);
  end;

  if IsEOF then exit(TPLCSSSelectorExpression.Create(a, b, ''));
end;

constructor TPLCSSSelectorPseudo.Create(const AContext: TPLCSSSelectorContext;
  const AName: TPLString; const AExpression: TPLCSSSelectorPseudoExpression;
  const AKind: TPLCSSSelectorPseudoKind; const ANamespaced: TPLBool);
begin
  inherited Create(AContext);

  FName := AName;
  FExpression := AExpression;
  FKind := AKind;
  FNamespaced := ANamespaced;
end;

destructor TPLCSSSelectorPseudo.Destroy;
begin
  if Assigned(FExpression) then FExpression.Free;

  inherited Destroy;
end;

{ TPLCSSSelectorString }

constructor TPLCSSSelectorString.Create(const AContext: TPLCSSSelectorContext;
  const AValue: TPLString);
begin
  inherited Create(AContext);

  FValue := AValue;
end;

{ TPLCSSSelectorAttribute }

constructor TPLCSSSelectorAttribute.Create(
  const AContext: TPLCSSSelectorContext; const AName, AComparator: TPLString;
  const AValue: TPLCSSSelectorString; const ACaseSensitive: TPLBool);
begin
  inherited Create(Context);

  FName := AName;
  FComparator := AComparator;
  FValue := AValue;
  FCaseSensitive := ACaseSensitive;
end;

destructor TPLCSSSelectorAttribute.Destroy;
begin
  if Assigned(FValue) then FValue.Free;

  inherited Destroy;
end;

{ TPLCSSSelectorClass }

constructor TPLCSSSelectorClass.Create(const AContext: TPLCSSSelectorContext;
  const AName: TPLString);
begin
  inherited Create(AContext);

  FName := AName;
end;

{ TPLCSSSelectorType }

function TPLCSSSelectorType.IsUniversal: TPLBool;
begin
  Result := Name = '*';
end;

{ TPLCSSSelector }

constructor TPLCSSSelector.Create(const AContext: TPLCSSSelectorContext);
begin
  inherited Create(AContext);

  FCombinators := TPLCSSSelectorCombinators.Create;
  FSimpleSelectors := TPLCSSSimpleSelectorPatterns.Create(true);
end;

destructor TPLCSSSelector.Destroy;
begin
  FCombinators.Free;
  FSimpleSelectors.Free;

  inherited Destroy;
end;

{ TPLCSSSelectorParser }

class function TPLCSSSelectorParser.ParseSelector(ASelector: TPLString
  ): TPLCSSSelectors;
var
  pos: SizeInt = 1;

  function IsEOF: TPLBool; inline;
  begin
    Result := pos > Length(ASelector);
  end;

  function Current: TPLChar;
  begin
    if not IsEOF then Result := ASelector[pos] else Result := #0;
  end;

  procedure ConsumeWhitespace;
  begin
    while not IsEOF and Current.IsWhiteSpace do Inc(pos);
  end;

  function IsSpecialPseudo(const APseudo: TPLString): TPLBool; inline;
  begin
    Result := APseudo.ToLower in TPLStringFuncs.NewArray(['after', 'before',
      'first-letter', 'first-line', 'marker', 'placeholder', 'selection']);
  end;

  function NewIdentifier: TPLString;
  begin
    Result := '';
    if not IsEOF and (Current <> '_') and not TCharacter.IsLetter(Current) then exit;

    while not IsEOF and (TCharacter.IsLetterOrDigit(Current) or (Current in ['_', '-'])) do begin
      Result += Current;
      Inc(pos);
    end;
  end;

  function NewString: TPLCSSSelectorString;
  var
    q: TPLChar;
    esc: TPLBool = false;
    v, pom, pom2: TPLString;
    i, p: SizeInt;
  begin
    q := Current;
    v := '';
    pom := '';
    p := pos;
    Inc(pos);

    while not IsEOF do begin
      if esc then begin
        if Current in ['0'..'9', 'A'..'F', 'a'..'f'] then begin
          pom2 := '';
          for i := 0 to 5 do begin
            if not (Current in ['0'..'9', 'A'..'F', 'a'..'f']) then break;
            pom2 += Current;
            Inc(pos);
          end;
          pom2 := UnicodeToUTF8(pom2.FromHex);
          v += pom2;
          pom += pom2;
        end else begin
          v += Current;
          pom += Current;
        end;
        esc := false;
      end else begin
        if Current = q then begin
          Inc(pos);
          break;
        end;
        pom += Current;
        if Current = '\' then esc := true else v += Current;
      end;

      Inc(pos);
    end;

    Result := TPLCSSSelectorString.Create(TPLCSSSelectorContext.Create(ASelector, pom, p, pos), v);
  end;

  function NewAttribute: TPLCSSSelectorAttribute;
  var
    sub: TPLString = '[';
    pom, pom2, op: TPLString;
    cs: TPLBool = false;
    p, pp: SizeInt;
    v: TPLCSSSelectorString = nil;
    q: TPLChar;
  begin
    op := '';
    p := pos;
    ConsumeWhitespace;
    pom := '';
    if not IsEOF then pom := NewIdentifier;
    if pom.IsEmpty then pom := 'attr';
    sub += pom;
    ConsumeWhitespace;

    if not isEOF then begin
      if Current in ['~', '*', '$', '|', '^'] then begin
        op := Current + '=';
        Inc(pos);
        if IsEOF or (Current <> '=') then begin
          if not IsEOF and not (Current in ['''', '"']) then Inc(pos);
        end else Inc(pos);
      end else if Current <> ']' then begin
        if Current = '=' then op := '=';
        Inc(pos);
      end;

      ConsumeWhitespace;
      if not IsEOF then begin
        if op <> '' then begin
          sub += op;
          if Current in ['''', '"'] then begin
            q := Current;
            v := NewString;
            sub += q + v.Context.Value + q;
          end else begin
            pp := pos;
            pom2 := NewIdentifier;
            sub += pom2;
            v := TPLCSSSelectorString.Create(TPLCSSSelectorContext.Create(ASelector, pom2, pp, pos), pom2);
          end;
        end;

        ConsumeWhitespace;
        if LowerCase(Current) in ['i', 's'] then begin
          cs := LowerCase(Current) = 's';
          Inc(pos);
        end;
        ConsumeWhitespace;
        if not IsEOF then Inc(pos);
      end;
    end;

    Result := TPLCSSSelectorAttribute.Create(TPLCSSSelectorContext.Create(ASelector, sub + ']', p, pos), pom, op, v, cs);
  end;

  function NewPseudoExpression: TPLCSSSelectorPseudoExpression;
  begin
    //Result := TPLCSSSelectorPseudoExpression.Create(TPLCSSSelectorContext.Create(), );
  end;

  function NewPseudo(AName: TPLString; ANamespaced: TPLBool; AKind: TPLCSSSelectorPseudoKind): TPLCSSSelectorPseudo;
  var
    expr: TPLCSSSelectorPseudoExpression = nil;
    sub: TPLString = '';
    p: SizeInt;
  begin
    p := pos;
    sub += AName;

    ConsumeWhitespace;
    if not IsEOF and (Current = '(') then begin
      sub += Current;
      Inc(pos);
      ConsumeWhitespace;
      expr := NewPseudoExpression;
      sub += expr.Context.Value + ')';
      ConsumeWhitespace;
      if not IsEOF then Inc(pos);
    end;

    Result := TPLCSSSelectorPseudo.Create(TPLCSSSelectorContext.Create(ASelector, sub, p, pos), AName, expr, AKind, ANamespaced);
  end;

  function NewPattern: TPLCSSSimpleSelectorPattern;
  var
    p, pp, sc: SizeInt;
    sub: TPLString = '';
    ss: TPLBool = false;
    pom: TPLString;
    ps, dc: TPLBool;
  begin
    p := pos;
    Result := TPLCSSSimpleSelectorPattern.Create(TPLCSSSelectorContext.Create(ASelector, '', p, pos));

    if Current = '*' then begin
      ss := true;
      sub += Current;
      Result.SimpleSelectors.Add(TPLCSSSelectorType.Create(TPLCSSSelectorContext.Create(ASelector, '*', p, pos), '*'));
      Inc(pos);
    end else if TCharacter.IsLetter(Current) then begin
      ss := true;
      pom := NewIdentifier;
      sub += pom;
      Result.SimpleSelectors.Add(TPLCSSSelectorType.Create(TPLCSSSelectorContext.Create(ASelector, pom, p, pos), pom));
    end;

    sc := 0;
    ps := false;
    while not IsEOF do begin
      pp := pos;
      case Current of
        '.': begin
          Inc(pos);
          if not IsEOF then pom := NewIdentifier else pom := '';
          if not pom.IsEmpty then begin
            sub += '.' + pom;
            Result.SimpleSelectors.Add(TPLCSSSelectorClass.Create(TPLCSSSelectorContext.Create(ASelector, '.' + pom, pp, pos), pom));
          end;
        end;
        '#': begin
          Inc(pos);
          if not IsEOF then pom := NewIdentifier else pom := '';
          if not pom.IsEmpty then begin
            sub += '#' + pom;
            Result.SimpleSelectors.Add(TPLCSSSelectorHash.Create(TPLCSSSelectorContext.Create(ASelector, '#' + pom, pp, pos), pom));
          end;
        end;
        '[': begin
          pp := pos;
          Inc(pos);
          Result.SimpleSelectors.Add(NewAttribute);
          sub += ASelector.SubStr(pp, pos - pp + 1);
        end;
        ':': begin
          dc := false;
          Inc(pos);
          sub += ':';

          if IsEOF then break;
          if Current = ':' then begin
            dc := true;
            sub += ':';
            Inc(pos);
          end;

          if not IsEOF then pom := NewIdentifier.ToLower else pom := '';
          if not pom.IsEmpty then begin
            if IsSpecialPseudo(pom) or dc then begin
              sub += pom;
              Result.SimpleSelectors.Add(NewPseudo(pom, dc, pskElement));
            end else begin
              case pom of
                'not': begin
                  //...
                end;
                // zrobić :is, :where, :nth-...
                else begin
                  sub += pom;
                  Result.SimpleSelectors.Add(NewPseudo(pom, dc, pskClass));
                end;
              end;
            end;

            Inc(sc);
          end;
        end;
        else break;
      end;
    end;

    with Result.Context do begin
      Value := sub;
      Finish := pos;
    end;
  end;

  function NewSelector: TPLCSSSelector;
  var
    sub: TPLString = '';
    c: TPLCSSSelectorCombinator;
    wsp: TPLBool;
    p: TPLCSSSimpleSelectorPattern;
  begin
    Result := TPLCSSSelector.Create(TPLCSSSelectorContext.Create(ASelector, '', pos, pos));

    while not IsEOF do begin
      c := scUndefined;

      if not Result.SimpleSelectors.Empty then begin
        wsp := false;

        if not IsEOF and Current.IsWhiteSpace then begin
          wsp := true;
          ConsumeWhitespace;
        end;

        if not IsEOF then begin
          case Current of
            '>': c := scChild;
            '~': c := scGeneralSibling;
            '+': c := scAdjascentSibling;
            else if wsp then c := scDescendant else c := scChild;
          end;

          if (c = scUndefined) or (Current = ',') then break;

          if c = scDescendant then sub += ' ' else begin
            sub += Current;
            Inc(pos);
          end;

          ConsumeWhitespace;
          if IsEOF then break;
        end else break;

        //if c = scUndefined then begin
        //  Inc(pos);
        //  continue;
        //end;

        Result.Combinators.Add(TPLCSSSelectorCombinatorItem.Create(Result.SimpleSelectors.Count-1, c));
      end;

      p := NewPattern;
      sub += p.Context.Value;
      Result.SimpleSelectors.Add(p);
    end;

    with Result.Context do begin
      Value := sub;
      Finish := pos;
    end;
  end;

begin
  ASelector := ASelector.Trim;
  Result := TPLCSSSelectors.Create(true);

  while not IsEOF do begin
    ConsumeWhitespace;
    Result.Add(NewSelector);
    ConsumeWhitespace;
    if not IsEOF and (Current <> ',') then break;
    Inc(pos);
  end;
end;

end.

