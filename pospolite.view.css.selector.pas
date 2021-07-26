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

    property Context: TPLCSSSelectorContext read FContext;
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

    property Text: TPLString read FText;
    property Evaluation: TPLCSSSelectorExpression read FEvaluation;
    property Items: TPLCSSSelectorExpressionItems read FItems;
  end;

  { TPLCSSSelectorPseudoEvaluator }

  TPLCSSSelectorPseudoEvaluator = packed class
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

    property Value: TPLString read FValue;
  end;

  { TPLCSSSelectorAttribute }

  TPLCSSSelectorAttribute = packed class(TPLCSSSimpleSelector)
  private
    FComparator: TPLString;
    FName: TPLString;
    FValue: TPLCSSSelectorString;
  public
    constructor Create(const AContext: TPLCSSSelectorContext; const AName,
      AComparator: TPLString; const AValue: TPLCSSSelectorString); reintroduce;
    destructor Destroy; override;

    property Name: TPLString read FName;
    property Comparator: TPLString read FComparator;
    property Value: TPLCSSSelectorString read FValue;
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
  TPLCSSSelectorCombinators = packed class(specialize TPLList<TPLCSSSelectorCombinator>);

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
  const AValue: TPLCSSSelectorString);
begin
  inherited Create(Context);

  FName := AName;
  FComparator := AComparator;
  FValue := AValue;
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

  function NewPattern: TPLCSSSimpleSelectorPattern;
  var
    p: SizeInt;
    sub: TPLString = '';
  begin
    p := pos;
    Result := TPLCSSSimpleSelectorPattern.Create(TPLCSSSelectorContext.Create(ASelector, '', p, pos));
    //...

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
            else if wsp then c := scDescendant;
          end;

          if (c = scUndefined) or (Current = ',') then break;

          if c = scDescendant then sub += ' ' else begin
            sub += Current;
            Inc(pos);
          end;

          ConsumeWhitespace;
          if IsEOF then break;
        end else break;

        if c = scUndefined then begin
          Inc(pos);
          continue;
        end;

        Result.Combinators.Add(c);
        p := NewPattern;
        sub += p.Context.Value;
        Result.SimpleSelectors.Add(p);
      end;
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

