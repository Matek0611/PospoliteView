unit Pospolite.View.CSS.StyleSheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Document,
  Pospolite.View.CSS.Declaration, Pospolite.View.CSS.Selector,
  Pospolite.View.CSS.MediaQuery, Pospolite.View.CSS.Binder,
  Pospolite.View.Internet;

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

  TPLCSSStyleSheetPartSelectors = class(specialize TPLObjectList<TPLCSSStyleSheetPartSelector>);

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
    FCharset: TPLString;
    FFileName: TPLString;
    FFontFaces: TPLCSSStyleSheetPartSelectors;
    FImports: TPLCSSStyleSheetPartRules;
    FKeyframes: TPLCSSStyleSheetPartRules;
    FMedias: TPLCSSStyleSheetPartRules;
    FPages: TPLCSSStyleSheetPartSelectors;
    FSelectors: TPLCSSDeclarationsList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(ASource: TPLString; const AMerge: TPLBool = true);
    procedure Clean;

    property Charset: TPLString read FCharset write FCharset;
    property FontFaces: TPLCSSStyleSheetPartSelectors read FFontFaces;
    property Imports: TPLCSSStyleSheetPartRules read FImports;
    property Keyframes: TPLCSSStyleSheetPartRules read FKeyframes;
    property Medias: TPLCSSStyleSheetPartRules read FMedias;
    property Pages: TPLCSSStyleSheetPartSelectors read FPages;

    property Selectors: TPLCSSDeclarationsList read FSelectors;

    property FileName: TPLString read FFileName write FFileName;
  end;

  TPLFuncsOfClassCSSStyleSheet = specialize TPLFuncsOfClass<TPLCSSStyleSheet>;
  TPLCSSStyleSheetList = packed class(specialize TPLObjectList<TPLCSSStyleSheet>);

  TPLCSSStyleSheetManager = class;

  { TPLCSSStylingThread }

  TPLCSSStylingThread = class(TThread)
  private
    FEnabled: TPLBool;
    FManager: TPLCSSStyleSheetManager;

    procedure UpdateStyles;
  public
    constructor Create(AManager: TPLCSSStyleSheetManager);

    procedure Execute; override;

    property Enabled: TPLBool read FEnabled write FEnabled;
  end;

  { TPLCSSStyleSheetManager }

  TPLCSSStyleSheetManager = class
  private
    FBinder: TPLCSSStyleBinder;
    FExternals: TPLCSSStyleSheetList;
    FInternal: TPLCSSStyleSheet;
    FDocument: TPLHTMLDocument;
    FStyling: TPLCSSStylingThread;

    function ComparatorForSearch(const AObject: TPLCSSStyleSheet;
      const ACriteria: Variant): TPLSign;
    function ComparatorForSort(a, b: TPLCSSStyleSheet): TPLSign;
    procedure Rebind;
  public
    constructor Create(ADocument: TPLHTMLDocument);
    destructor Destroy; override;

    procedure AddToInternal(AStyle: TPLString); inline;
    procedure AddExternal(AURL: TPLString);

    procedure StartStyling;
    procedure StopStyling;
    procedure Rebuilt;

    property Internal: TPLCSSStyleSheet read FInternal;
    property Externals: TPLCSSStyleSheetList read FExternals;

    property Binder: TPLCSSStyleBinder read FBinder;
    property Styling: TPLCSSStylingThread read FStyling;
  public
    Environment: TPLCSSMediaQueriesEnvironment;

    function EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
  end;

implementation

uses Variants, Pospolite.View.CSS.Basics;

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

  FFileName := '';

  FSelectors := TPLCSSDeclarationsList.Create(true);
  FFontFaces := TPLCSSStyleSheetPartSelectors.Create(true);
  FImports := TPLCSSStyleSheetPartRules.Create(true);
  FKeyframes := TPLCSSStyleSheetPartRules.Create(true);
  FMedias := TPLCSSStyleSheetPartRules.Create(true);
  FPages := TPLCSSStyleSheetPartSelectors.Create(true);

  Clean;
end;

destructor TPLCSSStyleSheet.Destroy;
begin
  FSelectors.Free;
  FFontFaces.Free;
  FImports.Free;
  FKeyframes.Free;
  FMedias.Free;
  FPages.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleSheet.Load(ASource: TPLString; const AMerge: TPLBool);
begin
  RemoveCSSComments(ASource);


end;

procedure TPLCSSStyleSheet.Clean;
begin
  FCharset := '';

  FSelectors.Clear;
  FFontFaces.Clear;
  FImports.Clear;
  FKeyframes.Clear;
  FMedias.Clear;
  FPages.Clear;
end;

{ TPLCSSStylingThread }

procedure TPLCSSStylingThread.UpdateStyles;
begin

end;

constructor TPLCSSStylingThread.Create(AManager: TPLCSSStyleSheetManager);
begin
  inherited Create(true);

  FManager := AManager;
  FEnabled := false;
end;

procedure TPLCSSStylingThread.Execute;
begin

end;

{ TPLCSSStyleSheetManager }

function TPLCSSStyleSheetManager.ComparatorForSearch(
  const AObject: TPLCSSStyleSheet; const ACriteria: Variant): TPLSign;
var
  s: TPLString;
begin
  s := VarToStr(ACriteria).ToLower;

  if AObject.FFileName < s then Result := -1
  else if AObject.FFileName > s then Result := 1
  else Result := 0;
end;

function TPLCSSStyleSheetManager.ComparatorForSort(a, b: TPLCSSStyleSheet
  ): TPLSign;
begin
  if a.FFileName < b.FFileName then Result := -1
  else if a.FFileName > b.FFileName then Result := 1
  else Result := 0;
end;

procedure TPLCSSStyleSheetManager.Rebind;
begin
  FBinder.Annihilate;
  FBinder := TPLCSSStyleBinder.Create(FDocument);
end;

constructor TPLCSSStyleSheetManager.Create(ADocument: TPLHTMLDocument);
begin
  inherited Create;

  FInternal := TPLCSSStyleSheet.Create;
  FExternals := TPLCSSStyleSheetList.Create(true);

  FBinder := TPLCSSStyleBinder.Create(ADocument);
  FStyling := TPLCSSStylingThread.Create(self);
end;

destructor TPLCSSStyleSheetManager.Destroy;
begin
  FBinder.Free;
  FStyling.Free;

  FInternal.Free;
  FExternals.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleSheetManager.AddToInternal(AStyle: TPLString);
begin
  FInternal.Load(AStyle);
end;

procedure TPLCSSStyleSheetManager.AddExternal(AURL: TPLString);
var
  ss: TPLCSSStyleSheet = nil;
  s: TPLString;
begin
  if TPLFuncsOfClassCSSStyleSheet.FastSearch(FExternals, AURL,
    @ComparatorForSearch) > -1 then exit;

  try
    s := OnlineClient.Download(AURL);

    ss := TPLCSSStyleSheet.Create;
    ss.FileName := AURL.ToLower;
    ss.Load(s);

    FExternals.Add(ss);
    FExternals.Sort(@ComparatorForSort);
  except
    if Assigned(ss) then ss.Free;
  end;
end;

procedure TPLCSSStyleSheetManager.StartStyling;
begin
  FStyling.Enabled := true;
end;

procedure TPLCSSStyleSheetManager.StopStyling;
begin
  FStyling.Enabled := false;
  Rebind;
end;

procedure TPLCSSStyleSheetManager.Rebuilt;
begin
  FBinder.UpdateBindings;
end;

function TPLCSSStyleSheetManager.EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
begin
  Result := Environment;
  Result.UsePrinter := true;
end;

end.

