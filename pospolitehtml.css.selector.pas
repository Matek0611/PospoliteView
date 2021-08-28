unit PospoLiteHTML.CSS.Selector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, PospoLiteHTML.CSS.Basics;

type

  TPLCSSSimpleSelectorType = (
    sstElement, // element
    sstUniversal, // *
    sstExistAttribute, // [attr]
    sstExactAttribute, // [attr=val]
    sstOneOfAttribute, // [attr~=val]
    sstBeginHyphenAttribute, // [attr|=val]
    sstBeginWithAttribute, // [attr^=val]
    sstEndWithAttribute, // [attr$=val]
    sstSubstringAttribute, // [attr*=val]
    sstClass, // .class
    sstId, // #id
    sstPseudoClass, // class:pseudo
    sstLang // :lang(language)
  );

  { TPLCSSSimpleSelector }

  TPLCSSSimpleSelector = packed class(TObject)
  private
    FAttribute: TPLCSSString;
    FNode: TDOMNode;
    FType: TPLCSSSimpleSelectorType;
    FValue: TPLCSSString;
  protected
    {%H-}constructor Create(AType: TPLCSSSimpleSelectorType; const AValue,
      AAttribute: TPLCSSString; AHTMLNode: TDOMNode = nil);
  public
    constructor CreateElement(const AName: TPLCSSString; AHTMLNode: TDOMNode = nil);
    constructor CreateUniversal;
    constructor CreateExistAttribute(const AAttribute: TPLCSSString);
    constructor CreateBinaryAttribute(AType: TPLCSSSimpleSelectorType; const AValue,
      AAttribute: TPLCSSString);
    constructor CreateClass(const AClassName: TPLCSSString);
    constructor CreateId(const AId: TPLCSSString);
    constructor CreatePseudoClass(const APseudoClass, ASeparator: TPLCSSString);
    constructor CreateLang(const ALang: TPLCSSString);

    class function AttributeTypeFromOperator(AOperator: TPLCSSChar): TPLCSSSimpleSelectorType; static;
    function IsAttributeCondition: boolean;

    property &Type: TPLCSSSimpleSelectorType read FType;
    property Attribute: TPLCSSString read FAttribute;
    property Value: TPLCSSString read FValue;
    property PseudoClass: TPLCSSString read FValue;
    property PseudoClassSeparator: TPLCSSString read FAttribute;
    property Lang: TPLCSSString read FValue;

    property Node: TDOMNode read FNode write FNode;
  end;

  TPLCSSSimpleSelectorsCombinator = (
    sscNone, sscDescendant, sscChild, sscSibling
  );

  { TPLCSSSimpleSelectors }

  TPLCSSSimpleSelectors = class(specialize TPLObjectList<TPLCSSSimpleSelector>)
  private
    FCombinator: TPLCSSSimpleSelectorsCombinator;
  public
    constructor Create(ACombinator: TPLCSSSimpleSelectorsCombinator);

    property Combinator: TPLCSSSimpleSelectorsCombinator read FCombinator;
  end;

  { TPLCSSSelector }

  TPLCSSSelector = class(specialize TPLObjectList<TPLCSSSimpleSelectors>);

  { TPLCSSSelectors }

  TPLCSSSelectors = class(specialize TPLObjectList<TPLCSSSelector>);

implementation

{ TPLCSSSimpleSelector }

constructor TPLCSSSimpleSelector.Create(AType: TPLCSSSimpleSelectorType;
  const AValue, AAttribute: TPLCSSString; AHTMLNode: TDOMNode);
begin
  inherited Create;

  FType := AType;
  FValue := AValue;
  FAttribute := AAttribute;
  FNode := AHTMLNode;
end;

constructor TPLCSSSimpleSelector.CreateElement(const AName: TPLCSSString;
  AHTMLNode: TDOMNode);
begin
  Create(sstElement, AName, '', AHTMLNode);
end;

constructor TPLCSSSimpleSelector.CreateUniversal;
begin
  Create(sstUniversal, '', '');
end;

constructor TPLCSSSimpleSelector.CreateExistAttribute(
  const AAttribute: TPLCSSString);
begin
  Create(sstExistAttribute, '', AAttribute);
end;

constructor TPLCSSSimpleSelector.CreateBinaryAttribute(
  AType: TPLCSSSimpleSelectorType; const AValue, AAttribute: TPLCSSString);
begin
  Create(AType, AValue, AAttribute);
end;

constructor TPLCSSSimpleSelector.CreateClass(const AClassName: TPLCSSString);
begin
  Create(sstClass, AClassName, 'class');
end;

constructor TPLCSSSimpleSelector.CreateId(const AId: TPLCSSString);
begin
  Create(sstId, AId, 'id');
end;

constructor TPLCSSSimpleSelector.CreatePseudoClass(const APseudoClass,
  ASeparator: TPLCSSString);
begin
  Create(sstPseudoClass, APseudoClass, ASeparator);
end;

constructor TPLCSSSimpleSelector.CreateLang(const ALang: TPLCSSString);
begin
  Create(sstLang, ALang, '');
end;

class function TPLCSSSimpleSelector.AttributeTypeFromOperator(
  AOperator: TPLCSSChar): TPLCSSSimpleSelectorType;
begin
  case AOperator of
    '=': Result := sstExactAttribute;
    '~': Result := sstOneOfAttribute;
    '|': Result := sstBeginHyphenAttribute;
    '^': Result := sstBeginWithAttribute;
    '$': Result := sstEndWithAttribute;
    '*': Result := sstSubstringAttribute;
    else Result := sstSubstringAttribute;
  end;
end;

function TPLCSSSimpleSelector.IsAttributeCondition: boolean;
begin
  Result := FType in [sstExistAttribute, sstExactAttribute, sstOneOfAttribute,
    sstBeginHyphenAttribute, sstBeginWithAttribute, sstEndWithAttribute,
    sstSubstringAttribute, sstClass, sstId];
end;

{ TPLCSSSimpleSelectors }

constructor TPLCSSSimpleSelectors.Create(
  ACombinator: TPLCSSSimpleSelectorsCombinator);
begin
  inherited Create(true);

  FCombinator := ACombinator;
end;

end.

