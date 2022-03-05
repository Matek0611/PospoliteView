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
    FMediaQuery: TPLString;
    FMedias: TPLCSSStyleSheetPartRules;
    FPages: TPLCSSStyleSheetPartSelectors;
    FSelectors: TPLCSSDeclarationsList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(ASource: TPLString; const AMerge: TPLBool = true);
    procedure CleanUp;
    function MediaAccepts(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;

    property Charset: TPLString read FCharset write FCharset;
    property FontFaces: TPLCSSStyleSheetPartSelectors read FFontFaces;
    property Imports: TPLCSSStyleSheetPartRules read FImports;
    property Keyframes: TPLCSSStyleSheetPartRules read FKeyframes;
    property Medias: TPLCSSStyleSheetPartRules read FMedias;
    property Pages: TPLCSSStyleSheetPartSelectors read FPages;

    property Selectors: TPLCSSDeclarationsList read FSelectors;

    property FileName: TPLString read FFileName write FFileName;
    property MediaQuery: TPLString read FMediaQuery write FMediaQuery;
  end;

  TPLFuncsOfClassCSSStyleSheet = specialize TPLFuncsOfClass<TPLCSSStyleSheet>;
  TPLCSSStyleSheetList = packed class(specialize TPLObjectList<TPLCSSStyleSheet>);

  TPLCSSStyleSheetManager = class;

  { TPLCSSStylingThread }

  TPLCSSStylingThread = class(TThread)
  private
    FManager: TPLCSSStyleSheetManager;

    procedure UpdateStyles;
    procedure LoadAllStyles;
  public
    constructor Create(AManager: TPLCSSStyleSheetManager);

    procedure Execute; override;

    procedure UpdateStyling;
    procedure Annihilate;
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
    procedure Restyle;
  public
    constructor Create(ADocument: TPLHTMLDocument);
    destructor Destroy; override;

    procedure AddToInternal(AStyle: TPLString); inline;
    procedure AddExternal(AURL: TPLString);

    procedure StartStyling;
    procedure StopStyling;
    procedure Rebuilt;

    procedure ClearStyles;

    property Internal: TPLCSSStyleSheet read FInternal;
    property Externals: TPLCSSStyleSheetList read FExternals;

    property Binder: TPLCSSStyleBinder read FBinder;
    property Styling: TPLCSSStylingThread read FStyling;
  public
    Environment: TPLCSSMediaQueriesEnvironment;

    function EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
  end;

implementation

uses Variants, Pospolite.View.CSS.Basics, Pospolite.View.HTML.Basics;

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
  FMediaQuery := '';

  FSelectors := TPLCSSDeclarationsList.Create(true);
  FFontFaces := TPLCSSStyleSheetPartSelectors.Create(true);
  FImports := TPLCSSStyleSheetPartRules.Create(true);
  FKeyframes := TPLCSSStyleSheetPartRules.Create(true);
  FMedias := TPLCSSStyleSheetPartRules.Create(true);
  FPages := TPLCSSStyleSheetPartSelectors.Create(true);

  CleanUp;
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
  if not AMerge then CleanUp;


end;

procedure TPLCSSStyleSheet.CleanUp;
begin
  FCharset := '';

  FSelectors.Clear;
  FFontFaces.Clear;
  FImports.Clear;
  FKeyframes.Clear;
  FMedias.Clear;
  FPages.Clear;
end;

function TPLCSSStyleSheet.MediaAccepts(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
var
  mq: TPLCSSMediaQueries;
begin
  if TPLString.IsNullOrEmpty(FMediaQuery) then exit(true);

  mq := TPLCSSMediaQueries.Create(true);
  try
    TPLCSSMediaQueryParser.ParseMediaQueries(FMediaQuery, mq);
    Result := mq.Evaluate(AEnvironment);
  finally
    mq.Free;
  end;
end;

{ TPLCSSStylingThread }

procedure TPLCSSStylingThread.UpdateStyles;

  procedure EnumStyleUpdates(obj: TPLHTMLObject);

    procedure ApplyStylesheet(css: TPLCSSStyleSheet);
    var
      pr: TPLCSSStyleSheetPartRule;
      ps: TPLCSSStyleSheetPartSelector;
      pd: TPLCSSDeclarations;
    begin
      if not css.MediaAccepts(FManager.Environment) then exit;

      for pd in css.Selectors do begin
        if not pd.IsFor(obj) then continue;


      end;
    end;

  var
    e: TPLHTMLObject;
    st: TPLCSSStyleSheet;
  begin
    if not Assigned(obj) then exit;

    ApplyStylesheet(FManager.Internal);
    for st in FManager.Externals do
      ApplyStylesheet(st);

    if Suspended then exit;

    for e in obj.Children do
      EnumStyleUpdates(e);
  end;

begin
  // aktualizacja styli - nie może kłócić się to z przejściami
  EnumStyleUpdates(FManager.FDocument.Body);
end;

procedure TPLCSSStylingThread.LoadAllStyles;

  procedure EnumStyles(obj: TPLHTMLObject);
  var
    e: TPLHTMLObject;
    m: TPLHTMLObjectAttribute;
    s: TPLCSSStyleSheet;
  begin
    if not Assigned(obj) then exit;

    if obj.Name = 'style' then
      // internal
      FManager.Internal.Load(obj.Text)
    else if (obj.Name = 'link') and (obj.Attributes.Rel <> Default(TPLHTMLObjectAttribute)) then begin
      // external
      FManager.Externals.Add(TPLCSSStyleSheet.Create);
      s := FManager.Externals.Last;
      s.FileName := obj.Attributes.Href.Value;
      m := obj.Attributes.Get('media');

      if (m <> Default(TPLHTMLObjectAttribute)) and (not TPLString.IsNullOrEmpty(m.Value))
        and (m.Value.ToLower <> 'all') then s.MediaQuery := m.Value;

      s.Load(OnlineClient.FileGetContents(s.FileName)); // może załadować przy wczytywaniu dokumentu?
    end else if obj.Attributes.Style <> Default(TPLHTMLObjectAttribute) then
      // inline
      obj.ApplyInlineStyles;

    for e in obj.Children do
      EnumStyles(e);
  end;

begin
  FManager.ClearStyles;
  EnumStyles(FManager.FDocument.Root); // faster than query selector
end;

constructor TPLCSSStylingThread.Create(AManager: TPLCSSStyleSheetManager);
begin
  inherited Create(true);

  FManager := AManager;
end;

procedure TPLCSSStylingThread.Execute;
var
  delay: Cardinal = 1000;
begin
  LoadAllStyles;
  FManager.Binder.UpdateBindings;

  while not Terminated and not Suspended do begin
    Synchronize(@UpdateStyles);

    Sleep(delay);
  end;
end;

procedure TPLCSSStylingThread.UpdateStyling;
begin
  Start;
end;

procedure TPLCSSStylingThread.Annihilate;
begin
  Suspended := true;
  Free;
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

procedure TPLCSSStyleSheetManager.Restyle;
begin
  FStyling.Annihilate;
  FStyling := TPLCSSStylingThread.Create(self);
end;

constructor TPLCSSStyleSheetManager.Create(ADocument: TPLHTMLDocument);
begin
  inherited Create;

  FDocument := ADocument;

  FInternal := TPLCSSStyleSheet.Create;
  FExternals := TPLCSSStyleSheetList.Create(true);

  FBinder := TPLCSSStyleBinder.Create(ADocument);
  FStyling := TPLCSSStylingThread.Create(self);
end;

destructor TPLCSSStyleSheetManager.Destroy;
begin
  FStyling.Annihilate;
  FBinder.Annihilate;

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
  FStyling.UpdateStyling;
end;

procedure TPLCSSStyleSheetManager.StopStyling;
begin
  Restyle;
  Rebind;
end;

procedure TPLCSSStyleSheetManager.Rebuilt;
begin
  if FStyling.Suspended then
    FBinder.UpdateBindings;
end;

procedure TPLCSSStyleSheetManager.ClearStyles;
begin
  FInternal.CleanUp;
  FExternals.Clear;
end;

function TPLCSSStyleSheetManager.EnvironmentPrinter: TPLCSSMediaQueriesEnvironment;
begin
  Result := Environment;
  Result.UsePrinter := true;
end;

end.

