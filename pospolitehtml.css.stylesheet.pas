unit PospoLiteHTML.CSS.Stylesheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Properties,
  PospoLiteHTML.CSS.Media, PospoLiteHTML.CSS.Rules, PospoLiteHTML.CSS.Selector;

type

  { TPLCSSStyleSheet }

  TPLCSSStyleSheet = class(TObject)
  private
    FCharsets: TPLCSSAtRuleCharsets;
    FFontFaces: TPLCSSAtRuleFontFaces;
    FImports: TPLCSSAtRuleImports;
    FNamespace: TPLCSSAtRuleNamespace;
    FRulesets: TPLCSSRulesets;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    property Namespace: TPLCSSAtRuleNamespace read FNamespace write FNamespace;
    property Charsets: TPLCSSAtRuleCharsets read FCharsets;
    property Imports: TPLCSSAtRuleImports read FImports;
    property FontFaces: TPLCSSAtRuleFontFaces read FFontFaces;
    property Rulesets: TPLCSSRulesets read FRulesets;
  end;

implementation

{ TPLCSSStyleSheet }

constructor TPLCSSStyleSheet.Create;
begin
  inherited Create;

  FNamespace := TPLCSSAtRuleNamespace.Create('');
  FCharsets := TPLCSSAtRuleCharsets.Create;
  FImports := TPLCSSAtRuleImports.Create(true);
  FFontFaces := TPLCSSAtRuleFontFaces.Create(true);
  FRulesets := TPLCSSRulesets.Create(true);
end;

destructor TPLCSSStyleSheet.Destroy;
begin
  FCharsets.Free;
  FImports.Free;
  FFontFaces.Free;
  FRulesets.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleSheet.Clear;
begin
  FNamespace := TPLCSSAtRuleNamespace.Create('');
  FCharsets.Clear;
  FImports.Clear;
  FFontFaces.Clear;
  FRulesets.Clear;
end;

end.

