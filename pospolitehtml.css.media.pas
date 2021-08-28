unit PospoLiteHTML.CSS.Media;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PospoLiteHTML.CSS.Basics;

type

  { TPLCSSMediaExpression }

  TPLCSSMediaExpression = packed class(TObject)
  private
    FHasValue: boolean;
    FName: TPLCSSString;
    FValue: TPLCSSString;
    {%H-}constructor Create;
  public
    constructor Create(const AName: TPLCSSString);
    constructor Create(const AName, AValue: TPLCSSString);

    function Clone: TPLCSSMediaExpression;

    property Name: TPLCSSString read FName;
    property Value: TPLCSSString read FValue;
    property HasValue: boolean read FHasValue;
  end;

  { TPLCSSMediaExpressions }

  TPLCSSMediaExpressions = class(specialize TPLObjectList<TPLCSSMediaExpression>);

  TPLCSSMediaQualifier = (mqOnly, mqNot, mqNone);

  { TPLCSSMediaQuery }

  TPLCSSMediaQuery = class(TObject)
  private
    FExpressions: TPLCSSMediaExpressions;
    FMediaType: TPLCSSString;
    FQualifier: TPLCSSMediaQualifier;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    property Qualifier: TPLCSSMediaQualifier read FQualifier write FQualifier;
    property MediaType: TPLCSSString read FMediaType write FMediaType;
    property Expressions: TPLCSSMediaExpressions read FExpressions;
  end;

  { TPLCSSMediaQueries }

  TPLCSSMediaQueries = class(specialize TPLObjectList<TPLCSSMediaQuery>);

  function MediumAppliesToScreen(const AMedia: TPLCSSString): boolean;
  function MediaAppliesToScreen(const AMediaQueries: TPLCSSMediaQueries): boolean;

implementation

function MediumAppliesToScreen(const AMedia: TPLCSSString): boolean;
begin
  Result := (AMedia = 'all') or (AMedia = 'screen') or (AMedia = '');
end;

function MediaAppliesToScreen(const AMediaQueries: TPLCSSMediaQueries): boolean;
var
  i: SizeInt;
begin
  Result := false;

  for i := 0 to AMediaQueries.Count-1 do
    if (AMediaQueries[i].Qualifier = mqNone) and
      MediumAppliesToScreen(AMediaQueries[i].MediaType) then exit(true);
end;

{ TPLCSSMediaExpression }

constructor TPLCSSMediaExpression.Create;
begin
  inherited Create;
end;

constructor TPLCSSMediaExpression.Create(const AName: TPLCSSString);
begin
  inherited Create;

  FName := AName.ToLower;
  FValue := '';
  FHasValue := false;
end;

constructor TPLCSSMediaExpression.Create(const AName, AValue: TPLCSSString);
begin
  inherited Create;

  FName := AName;
  FValue := AValue;
  FHasValue := true;
end;

function TPLCSSMediaExpression.Clone: TPLCSSMediaExpression;
begin
  Result := TPLCSSMediaExpression.Create;
  Result.FName := FName;
  Result.FValue := FValue;
  Result.FHasValue := FHasValue;
end;

{ TPLCSSMediaQuery }

constructor TPLCSSMediaQuery.Create;
begin
  inherited Create;

  FExpressions := TPLCSSMediaExpressions.Create(true);
  FQualifier := TPLCSSMediaQualifier.mqNone;
end;

destructor TPLCSSMediaQuery.Destroy;
begin
  FExpressions.Free;

  inherited Destroy;
end;

procedure TPLCSSMediaQuery.Reset;
begin
  FExpressions.Clear;
  FMediaType := '';
  FQualifier := mqNone;
end;

end.

