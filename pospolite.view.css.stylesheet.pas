unit Pospolite.View.CSS.StyleSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Document,
  Pospolite.View.CSS.Declaration, Pospolite.View.CSS.Selector,
  Pospolite.View.CSS.MediaQuery;

type

  TPLCSSStyleSheetPartKind = (sspSelector, sspRule, sspUndefined);

  { TPLCSSStyleSheetPart }

  TPLCSSStyleSheetPart = packed class
  private
    FKind: TPLCSSStyleSheetPartKind;
    FName: TPLString;
  public
    constructor Create; virtual;

    property Name: TPLString read FName write FName;
    property Kind: TPLCSSStyleSheetPartKind read FKind;
  end;

  TPLCSSStyleSheetParts = class(specialize TPLObjectList<TPLCSSStyleSheetPart>);

  { TPLCSSStyleSheetPartSelector }

  TPLCSSStyleSheetPartSelector = packed class(TPLCSSStyleSheetPart)
  private
    FProperties: TPLCSSDeclarations;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Properties: TPLCSSDeclarations read FProperties;
  end;

  TPLCSSStyleSheetPartRule = class;
  TPLCSSStyleSheetPartRules = class(specialize TPLObjectList<TPLCSSStyleSheetPartRule>);

  { TPLCSSStyleSheetPartRule }

  TPLCSSStyleSheetPartRule = class(TPLCSSStyleSheetPart)
  private
    FMediaQueries: TPLCSSMediaQueries;
    FNestedRules: TPLCSSStyleSheetPartRules;
    FValue: TPLCSSPropertyValue;
  public
    constructor Create; override;
    destructor Destroy; override;

    property NestedRules: TPLCSSStyleSheetPartRules read FNestedRules;
    property MediaQueries: TPLCSSMediaQueries read FMediaQueries;
    property Value: TPLCSSPropertyValue read FValue;
  end;

  { TPLCSSStyleSheet }

  TPLCSSStyleSheet = packed class
  private
    FFileName: TPLString;
    FParts: TPLCSSStyleSheetParts;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(ASource: TPLString);
    procedure Import(const AURL: TPLString);
    procedure Merge(ASheet: TPLCSSStyleSheet);

    property Parts: TPLCSSStyleSheetParts read FParts;
    property FileName: TPLString read FFileName write FFileName;
  end;

  TPLCSSStyleSheetList = packed class(specialize TPLObjectList<TPLCSSStyleSheet>);

  { TPLCSSStyleSheetManager }

  TPLCSSStyleSheetManager = class
  private
    FExternals: TPLCSSStyleSheetList;
    FInternal: TPLCSSStyleSheet;
    FDocument: TPLHTMLDocument;

    procedure ParseStyleSheet(ASource: TPLString; var ASheet: TPLCSSStyleSheet);
  public
    constructor Create(ADocument: TPLHTMLDocument);
    destructor Destroy; override;

    procedure AddToInternal(AStyle: TPLString);
    procedure AddExternal(AURL: TPLString);

    procedure StartStyling;
    procedure StopStyling;
    procedure Rebuilt;

    property Internal: TPLCSSStyleSheet read FInternal;
    property Externals: TPLCSSStyleSheetList read FExternals;
  public
    Environment: TPLCSSMediaQueriesEnvironment;

    function EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
  end;

implementation

uses Pospolite.View.CSS.Basics;

{ TPLCSSStyleSheetPart }

constructor TPLCSSStyleSheetPart.Create;
begin
  inherited Create;

  FName := '';
  FKind := sspUndefined;
end;

{ TPLCSSStyleSheetPartSelector }

constructor TPLCSSStyleSheetPartSelector.Create;
begin
  inherited Create;

  FKind := sspSelector;
  FProperties := TPLCSSDeclarations.Create();
end;

destructor TPLCSSStyleSheetPartSelector.Destroy;
begin
  FProperties.Free;

  inherited Destroy;
end;

{ TPLCSSStyleSheetPartRule }

constructor TPLCSSStyleSheetPartRule.Create;
begin
  inherited Create;

  FKind := sspRule;
  FNestedRules := TPLCSSStyleSheetPartRules.Create(true);
  FMediaQueries := TPLCSSMediaQueries.Create(true);
  FValue := TPLCSSPropertyValue.Create(true);
end;

destructor TPLCSSStyleSheetPartRule.Destroy;
begin
  FValue.Free;
  FMediaQueries.Free;
  FNestedRules.Free;

  inherited Destroy;
end;

constructor TPLCSSStyleSheet.Create;
begin
  inherited Create;

  FParts := TPLCSSStyleSheetParts.Create(true);
  FFileName := '';
end;

destructor TPLCSSStyleSheet.Destroy;
begin
  FParts.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleSheet.Load(ASource: TPLString);
begin
  RemoveCSSComments(ASource);


end;

procedure TPLCSSStyleSheet.Import(const AURL: TPLString);
begin

end;

procedure TPLCSSStyleSheet.Merge(ASheet: TPLCSSStyleSheet);
begin

end;

{ TPLCSSStyleSheetManager }

procedure TPLCSSStyleSheetManager.ParseStyleSheet(ASource: TPLString;
  var ASheet: TPLCSSStyleSheet);
begin

end;

constructor TPLCSSStyleSheetManager.Create(ADocument: TPLHTMLDocument);
begin
  inherited Create;

  FInternal := TPLCSSStyleSheet.Create;
  FExternals := TPLCSSStyleSheetList.Create(true);
end;

destructor TPLCSSStyleSheetManager.Destroy;
begin
  FInternal.Free;
  FExternals.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleSheetManager.AddToInternal(AStyle: TPLString);
begin
  ParseStyleSheet(AStyle, FInternal);
end;

procedure TPLCSSStyleSheetManager.AddExternal(AURL: TPLString);
var
  ss: TPLCSSStyleSheet;
begin

end;

procedure TPLCSSStyleSheetManager.StartStyling;
begin

end;

procedure TPLCSSStyleSheetManager.StopStyling;
begin

end;

procedure TPLCSSStyleSheetManager.Rebuilt;
begin

end;

function TPLCSSStyleSheetManager.EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
begin
  Result := Environment;
  Result.UsePrinter := true;
end;

end.

