unit Pospolite.View.CSS.MediaQuery;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.DOM.Screen;

type
  // - Media Queries Level 4 - //
  // https://www.w3.org/TR/mediaqueries-4/

  // actual values

  { TPLCSSMediaQueriesEnvironment }

  TPLCSSMediaQueriesEnvironment = packed record
  public
    function HasTouchScreen: TPLBool;

    function FeatureAnyHover: TPLString;
    function FeatureAnyPointer: TPLString;
    function FeatureAspectRatio: TPLFloat;
    function FeatureColor: TPLInt;
    function FeatureColorGamut: TPLString;
    function FeatureColorIndex: TPLInt;
    function FeatureDeviceAspectRatio: TPLFloat;
    function FeatureDeviceHeight: TPLInt;
    function FeatureDeviceWidth: TPLInt;
    function FeatureGrid: TPLBool;
    function FeatureHeight: TPLInt;
    function FeatureHover: TPLInt;
    function FeatureMonochrome: TPLInt;
    function FeatureOrientation: TPLString;
    function FeaturePointer: TPLString;
    function FeatureResolution: TPLFloat; // dpi
    function FeatureScan: TPLString;
    function FeatureUpdate: TPLString;
    function FeatureWidth: TPLInt;
  public
    Screen: TPLDOMScreen;
    Viewport: record // to write
      Width, Height: TPLInt;
    end;

    constructor Create(AVWidth, AVHeight: TPLInt);
  end;

  TPLCSSMediaExpressionBase = class;

  { TPLCSSMediaExpressionBase }

  TPLCSSMediaExpressionBase = class(TInterfacedObject, specialize IPLCloneable<TPLCSSMediaExpressionBase>)
  protected
    FFeature: TPLString;
  public
    function IsCorrect: TPLBool; virtual;
    function Clone: TPLCSSMediaExpressionBase; virtual;
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool; virtual;

    property Feature: TPLString read FFeature;
  end;

  { TPLCSSMediaExpression }

  TPLCSSMediaExpression = class(TPLCSSMediaExpressionBase)
  private
    FValue: TPLCSSPropertyValuePart;
  public
    constructor Create(const AFeature: TPLString);
    constructor Create(const AFeature: TPLString; const AValue: TPLCSSPropertyValuePart);
    destructor Destroy; override;

    function IsCorrect: TPLBool; override;
    function HasValue: TPLBool;
    function Clone: TPLCSSMediaExpressionBase; override;
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
      override;

    property Value: TPLCSSPropertyValuePart read FValue;
  end;

  TPLCSSMediaExpressionRangeOperator = (meroEqual, meroLess, meroLessEqual,
    meroGreater, meroGreaterEqual);

  { TPLCSSMediaExpressionRange }
  // https://www.w3.org/TR/mediaqueries-4/#mq-range-context
  TPLCSSMediaExpressionRange = class(TPLCSSMediaExpressionBase)
  private
    FOperatorFirst: TPLCSSMediaExpressionRangeOperator;
    FOperatorSecond: TPLCSSMediaExpressionRangeOperator;
    FValueFirst: TPLCSSPropertyValuePart;
    FValueSecond: TPLCSSPropertyValuePart;
  public
    constructor Create(const AFeature: TPLString; const AOperator: TPLCSSMediaExpressionRangeOperator;
      const AValue: TPLCSSPropertyValuePart);
    constructor Create( const AFeature: TPLString; const AOperator1, AOperator2: TPLCSSMediaExpressionRangeOperator;
      const AValue1, AValue2: TPLCSSPropertyValuePart);
    destructor Destroy; override;

    function IsCorrect: TPLBool; override;
    function HasOneSide: TPLBool; inline;
    function HasTwoSides: TPLBool; inline;
    function Clone: TPLCSSMediaExpressionBase; override;
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
      override;

    property ValueFirst: TPLCSSPropertyValuePart read FValueFirst;
    property ValueSecond: TPLCSSPropertyValuePart read FValueSecond;
    property OperatorFirst: TPLCSSMediaExpressionRangeOperator read FOperatorFirst;
    property OperatorSecond: TPLCSSMediaExpressionRangeOperator read FOperatorSecond;
  end;

  { TPLCSSMediaExpressions }

  TPLCSSMediaExpressions = class(specialize TPLObjectList<TPLCSSMediaExpressionBase>);

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

{$ifdef windows}
uses windows;
{$endif}

{ TPLCSSMediaQueriesEnvironment }

function TPLCSSMediaQueriesEnvironment.HasTouchScreen: TPLBool;
begin
  Result := GetSystemMetrics(SM_DIGITIZER) and ($00000080 or $00000001 or $00000002 or $00000004 or $00000008);
end;

function TPLCSSMediaQueriesEnvironment.FeatureAnyHover: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureAnyPointer: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureAspectRatio: TPLFloat;
begin
  Result := Viewport.Width / Viewport.Height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureColor: TPLInt;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureColorGamut: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureColorIndex: TPLInt;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceAspectRatio: TPLFloat;
begin
  Result := Screen.deviceScaleFactor;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceHeight: TPLInt;
begin
  Result := Screen.height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceWidth: TPLInt;
begin
  Result := Screen.width;
end;

function TPLCSSMediaQueriesEnvironment.FeatureGrid: TPLBool;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureHeight: TPLInt;
begin
  Result := Viewport.Height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureHover: TPLInt;
begin
  Result := not HasTouchScreen;
end;

function TPLCSSMediaQueriesEnvironment.FeatureMonochrome: TPLInt;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureOrientation: TPLString;
begin
  if Viewport.Height >= Viewport.Width then Result := 'portrait'
  else Result := 'landscape';
end;

function TPLCSSMediaQueriesEnvironment.FeaturePointer: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureResolution: TPLFloat;
begin
  Result := Screen.devicePPI;
end;

function TPLCSSMediaQueriesEnvironment.FeatureScan: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureUpdate: TPLString;
begin

end;

function TPLCSSMediaQueriesEnvironment.FeatureWidth: TPLInt;
begin
  Result := Viewport.Width;
end;

constructor TPLCSSMediaQueriesEnvironment.Create(AVWidth, AVHeight: TPLInt);
begin
  Viewport.Width := AVWidth;
  Viewport.Height := AVHeight;
end;

{ TPLCSSMediaExpressionBase }

function TPLCSSMediaExpressionBase.IsCorrect: TPLBool;
begin
  Result := not TPLString.IsNullOrEmpty(FFeature);
end;

function TPLCSSMediaExpressionBase.Clone: TPLCSSMediaExpressionBase;
begin
  Result := TPLCSSMediaExpressionBase.Create;
end;

function TPLCSSMediaExpressionBase.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
begin
  if not IsCorrect then exit(false);
  Result := true;
  // ...
end;

{ TPLCSSMediaExpression }

constructor TPLCSSMediaExpression.Create(const AFeature: TPLString);
begin
  inherited Create;

  FFeature := AFeature.ToLower;
  FValue := nil;
end;

constructor TPLCSSMediaExpression.Create(const AFeature: TPLString;
  const AValue: TPLCSSPropertyValuePart);
begin
  inherited Create;

  FFeature := AFeature.ToLower;
  FValue := AValue;
end;

destructor TPLCSSMediaExpression.Destroy;
begin
  if Assigned(FValue) then FValue.Free;

  inherited Destroy;
end;

function TPLCSSMediaExpression.IsCorrect: TPLBool;
begin
  Result := inherited IsCorrect;
end;

function TPLCSSMediaExpression.HasValue: TPLBool;
begin
  Result := Assigned(FValue);
end;

function TPLCSSMediaExpression.Clone: TPLCSSMediaExpressionBase;
begin
  Result := TPLCSSMediaExpression.Create(FFeature, FValue);
end;

function TPLCSSMediaExpression.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
begin
  Result := inherited Evaluate(AEnvironment);
  if not Result then exit;


end;

{ TPLCSSMediaExpressionRange }

constructor TPLCSSMediaExpressionRange.Create(const AFeature: TPLString;
  const AOperator: TPLCSSMediaExpressionRangeOperator;
  const AValue: TPLCSSPropertyValuePart);
begin

end;

constructor TPLCSSMediaExpressionRange.Create(const AFeature: TPLString;
  const AOperator1, AOperator2: TPLCSSMediaExpressionRangeOperator;
  const AValue1, AValue2: TPLCSSPropertyValuePart);
begin

end;

destructor TPLCSSMediaExpressionRange.Destroy;
begin
  if Assigned(FValueFirst) then FValueFirst.Free;
  if Assigned(FValueSecond) then FValueSecond.Free;

  inherited Destroy;
end;

function TPLCSSMediaExpressionRange.IsCorrect: TPLBool;
begin
  Result := inherited IsCorrect and (HasOneSide or HasTwoSides);
end;

function TPLCSSMediaExpressionRange.HasOneSide: TPLBool;
begin
  Result := Assigned(FValueFirst) and not Assigned(FValueSecond);
end;

function TPLCSSMediaExpressionRange.HasTwoSides: TPLBool;
begin
  Result := Assigned(FValueFirst) and Assigned(FValueSecond);
end;

function TPLCSSMediaExpressionRange.Clone: TPLCSSMediaExpressionBase;
begin
  Result := inherited Clone;
end;

function TPLCSSMediaExpressionRange.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
begin
  Result := inherited Evaluate(AEnvironment);
  if not Result then exit;


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

