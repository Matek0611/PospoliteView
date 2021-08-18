unit Pospolite.View.CSS.MediaQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

type

  TPLCSSMediaExpression = class;

  { TPLCSSMediaExpression }

  TPLCSSMediaExpression = class(TInterfacedObject, specialize IPLCloneable<TPLCSSMediaExpression>)
  private
    FName: TPLString;
    FValue: TPLString;
  public
    constructor Create(const AName: TPLString);
    constructor Create(const AName, AValue: TPLString);

    function Clone: TPLCSSMediaExpression; inline;
    function HasValue: TPLBool;

    property Name: TPLString read FName;
    property Value: TPLString read FValue;
  end;

  { TPLCSSMediaExpressions }

  TPLCSSMediaExpressions = class(specialize TPLObjectList<TPLCSSMediaExpression>);

  TPLCSSMediaQualifier = (mqOnly, mqNot, mqNone);

  { TPLCSSMediaQuery }

  TPLCSSMediaQuery = class(TObject)
  private
    FExpressions: TPLCSSMediaExpressions;
    FMediaType: TPLString;
    FQualifier: TPLCSSMediaQualifier;
  public
    constructor Create;
    destructor Destroy; override;

    property Qualifier: TPLCSSMediaQualifier read FQualifier write FQualifier;
    property MediaType: TPLString read FMediaType write FMediaType;
    property Expressions: TPLCSSMediaExpressions read FExpressions;
  end;

  { TPLCSSMediaQueries }

  TPLCSSMediaQueries = class(specialize TPLObjectList<TPLCSSMediaQuery>);

  { TPLCSSMediaQueryParser }

  TPLCSSMediaQueryParser = packed class sealed
  public
    class procedure ParseMediaQueries(ASource: TPLString; var AMediaQueries: TPLCSSMediaQueries); static;
  end;

implementation

{ TPLCSSMediaExpression }

constructor TPLCSSMediaExpression.Create(const AName: TPLString);
begin
  inherited Create;

  FName := AName.ToLower;
  FValue := '';
end;

constructor TPLCSSMediaExpression.Create(const AName, AValue: TPLString);
begin
  inherited Create;

  FName := AName.ToLower;
  FValue := AValue;
end;

function TPLCSSMediaExpression.Clone: TPLCSSMediaExpression;
begin
  Result := TPLCSSMediaExpression.Create(FName, FValue);
end;

function TPLCSSMediaExpression.HasValue: TPLBool;
begin
  Result := TPLString.IsNullOrEmpty(FValue);
end;

{ TPLCSSMediaQuery }

constructor TPLCSSMediaQuery.Create;
begin
  inherited Create;

  FExpressions := TPLCSSMediaExpressions.Create(true);
  FQualifier := mqNone;
end;

destructor TPLCSSMediaQuery.Destroy;
begin
  FExpressions.Free;

  inherited Destroy;
end;

{ TPLCSSMediaQueryParser }

class procedure TPLCSSMediaQueryParser.ParseMediaQueries(ASource: TPLString;
  var AMediaQueries: TPLCSSMediaQueries);
begin

end;

end.

