unit PospoLiteHTML.CSS.Rules;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Media,
  PospoLiteHTML.CSS.Selector, PospoLiteHTML.CSS.Properties, DOM, Forms,
  Controls, PospoLiteHTML.CSS.Values, dateutils;

type

  { TPLCSSRuleset }

  TPLCSSRuleset = packed class(TObject)
  private
    FOwnLists: boolean;
    FDeclarations: TPLCSSProperties;
    FIsUnparsed: boolean;
    FMediaQueries: TPLCSSMediaQueries;
    FSelectors: TPLCSSSelectors;
    FUnparsedValue: TPLCSSString;
  public
    constructor Create;
    constructor Create(ASelectors: TPLCSSSelectors; AMediaQueries: TPLCSSMediaQueries;
      ADeclarations: TPLCSSProperties);
    constructor Create(const ARegion: TPLCSSString);
    destructor Destroy; override;

    function MediaAppliesTo(const AObject: IPLHTMLBaseObject): boolean;

    property IsUnparsed: boolean read FIsUnparsed;
    property UnparsedValue: TPLCSSString read FUnparsedValue;
    property MediaQueries: TPLCSSMediaQueries read FMediaQueries;
    property Selectors: TPLCSSSelectors read FSelectors;
    property Declarations: TPLCSSProperties read FDeclarations;
  end;

  { TPLCSSRulesets }

  TPLCSSRulesets = class(specialize TPLObjectList<TPLCSSRuleset>);

  { TPLCSSAtRuleCharsets }

  TPLCSSAtRuleCharsets = class(specialize TPLList<TPLCSSString>);

  { TPLCSSAtRuleNamespace }

  TPLCSSAtRuleNamespace = packed record
  strict private
    FLink: TPLCSSString;
    FPrefix: TPLCSSString;
    procedure SetLink(AValue: TPLCSSString);
  public
    constructor Create(AValue: TPLCSSString);

    class operator = (a, b: TPLCSSAtRuleNamespace) r: boolean;
    function IsEmpty: boolean;

    property Link: TPLCSSString read FLink;
    property Prefix: TPLCSSString read FPrefix;
  end;

  { TPLCSSAtRuleImport }

  TPLCSSAtRuleImport = packed class(TObject)
  private
    FLink: TPLCSSString;
    FMediaQueries: TPLCSSMediaQueries;
  public
    constructor Create;
    destructor Destroy; override;

    property Link: TPLCSSString read FLink write FLink;
    property MediaQueries: TPLCSSMediaQueries read FMediaQueries;
  end;

  { TPLCSSAtRuleImports }

  TPLCSSAtRuleImports = class(specialize TPLObjectList<TPLCSSAtRuleImport>);

  { TPLCSSAtRuleFontFace }

  TPLCSSAtRuleFontFace = packed class(TObject)
  private
    FDeclarations: TPLCSSProperties;
    FMediaQueries: TPLCSSMediaQueries;
  public
    constructor Create;
    destructor Destroy; override;

    property MediaQueries: TPLCSSMediaQueries read FMediaQueries;
    property Declarations: TPLCSSProperties read FDeclarations;
  end;

  { TPLCSSAtRuleFontFaces }

  TPLCSSAtRuleFontFaces = class(specialize TPLObjectList<TPLCSSAtRuleFontFace>);

implementation

{$IFDEF WINDOWS}
uses windows;
{$ENDIF}

{ TPLCSSRuleset }

constructor TPLCSSRuleset.Create;
begin
  inherited Create;

  FIsUnparsed := false;
  FUnparsedValue := '';
  FOwnLists := true;
  FDeclarations := TPLCSSProperties.Create;
  FSelectors := TPLCSSSelectors.Create(true);
  FMediaQueries := TPLCSSMediaQueries.Create(true);
end;

constructor TPLCSSRuleset.Create(ASelectors: TPLCSSSelectors;
  AMediaQueries: TPLCSSMediaQueries; ADeclarations: TPLCSSProperties);
begin
  inherited Create;

  FIsUnparsed := false;
  FUnparsedValue := '';
  FOwnLists := false;
  FDeclarations := ADeclarations;
  FSelectors := ASelectors;
  FMediaQueries := AMediaQueries;
end;

constructor TPLCSSRuleset.Create(const ARegion: TPLCSSString);
begin
  inherited Create;

  FIsUnparsed := true;
  FUnparsedValue := ARegion;
  FOwnLists := true;
  FDeclarations := nil;
  FSelectors := nil;
  FMediaQueries := TPLCSSMediaQueries.Create(true);
end;

destructor TPLCSSRuleset.Destroy;
begin
  if FOwnLists and Assigned(FMediaQueries) then FMediaQueries.Free;
  if FOwnLists and Assigned(FSelectors) then FSelectors.Free;
  if FOwnLists and Assigned(FDeclarations) then FDeclarations.Free;

  inherited Destroy;
end;

function TPLCSSRuleset.MediaAppliesTo(const AObject: IPLHTMLBaseObject
  ): boolean;

  function CalcRatio(s: TPLCSSString): TPLCSSFloat;
  var
    a, b, p: TPLCSSFloat;
  begin
    s := s.Trim;
    p := s.Find('/');
    a := s.SubStr(1, trunc(p) - 1);
    b := s.SubStr(trunc(p) + 1, s.Length);
    if b = 0 then b := 1;
    Result := a / b;
  end;

var
  i, j: SizeInt;
  mediatype, expr: boolean;
  sizex, sizey: SizeInt;
  godz: Word;
  {$IFDEF WINDOWS}
    dc: HDC;
  {$ENDIF}
begin
  Result := false;
  if IsUnparsed then exit;

  for i := 0 to FMediaQueries.Count-1 do begin
    mediatype := MediumAppliesToScreen(FMediaQueries[i].MediaType);

    expr := true;
    for j := 0 to FMediaQueries[i].Expressions.Count-1 do begin
      if FMediaQueries[i].Expressions[j].HasValue then begin
        case FMediaQueries[i].Expressions[j].Name of
          'hover', 'any-hover': begin
            expr := expr and (FMediaQueries[i].Expressions[j].Value = 'hover');
          end;
          'pointer', 'any-pointer': begin
            expr := expr and (FMediaQueries[i].Expressions[j].Value = 'fine');
          end;
          'aspect-ratio': begin
            if not Assigned(AObject) then expr := false else begin
              sizex := AObject.GetViewport.GetRealRect.Size.Width;
              sizey := AObject.GetViewport.GetRealRect.Size.Height;
              if sizey = 0 then sizey := 1;
              expr := expr and ((sizex / sizey) = CalcRatio(FMediaQueries[i].Expressions[j].Value));
            end;
          end;
          'min-aspect-ratio': begin
            if not Assigned(AObject) then expr := false else begin
              sizex := AObject.GetViewport.GetRealRect.Size.Width;
              sizey := AObject.GetViewport.GetRealRect.Size.Height;
              if sizey = 0 then sizey := 1;
              expr := expr and ((sizex / sizey) >= CalcRatio(FMediaQueries[i].Expressions[j].Value));
            end;
          end;
          'max-aspect-ratio': begin
            if not Assigned(AObject) then expr := false else begin
              sizex := AObject.GetViewport.GetRealRect.Size.Width;
              sizey := AObject.GetViewport.GetRealRect.Size.Height;
              if sizey = 0 then sizey := 1;
              expr := expr and ((sizex / sizey) <= CalcRatio(FMediaQueries[i].Expressions[j].Value));
            end;
          end;
          'display-mode': begin
            case FMediaQueries[i].Expressions[j].Value of
              'fullscreen': begin
                if Assigned(AObject) and Assigned(AObject.GetWindow) then begin
                  expr := expr and (AObject.GetWindow.WindowState = wsFullScreen);
                end else expr := false;
              end;
              'standalone', 'minimal-ui', 'browser': begin
                if Assigned(AObject) and Assigned(AObject.GetWindow) then begin
                  expr := expr and (AObject.GetWindow.WindowState <> wsFullScreen);
                end else expr := false;
              end;
              else expr := false;
            end;
          end;
          'grid': expr := expr and (FMediaQueries[i].Expressions[j].Value = '0');
          'height': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Height = CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'max-height': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Height <= CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'min-height': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Height >= CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'width': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Width = CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'max-width': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Width <= CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'min-width': begin
            if Assigned(AObject) then begin
              expr := expr and (AObject.GetViewport.GetRealRect.Size.Width >= CSSLengthToScreen(PLCSSLength(FMediaQueries[i].Expressions[j].Value), AObject));
            end else expr := false;
          end;
          'orientation': begin
            if not Assigned(AObject) then expr := false else
            case FMediaQueries[i].Expressions[j].Value of
              'portrait': expr := expr and (AObject.GetViewport.GetRealRect.Size.Height >= AObject.GetViewport.GetRealRect.Size.Width);
              'landscape': expr := expr and (AObject.GetViewport.GetRealRect.Size.Height < AObject.GetViewport.GetRealRect.Size.Width);
              else expr := false;
            end;
          end;
          'prefers-color-scheme': begin
            godz := HourOf(Now);
            if ((godz >= 18) and (godz <= 23)) or ((godz >= 0) and (godz <= 3)) then  // dark 18:00 - 3:00
              expr := expr and (FMediaQueries[i].Expressions[j].Value = 'dark')
            else
              expr := expr and (FMediaQueries[i].Expressions[j].Value = 'light');
          end;
          'resolution': begin

          end;
          else expr := false;
        end;
      end else begin
        case FMediaQueries[i].Expressions[j].Name of
          'color': begin
            {$IFDEF WINDOWS}
              dc := GetDC(0);
              expr := expr and (GetDeviceCaps(dc, BITSPIXEL) > 2);
              ReleaseDC(0, dc);
            {$ELSE}
              expr := expr and true;
            {$ENDIF}
          end;
          'monochrome': begin
            {$IFDEF WINDOWS}
              dc := GetDC(0);
              expr := expr and (GetDeviceCaps(dc, BITSPIXEL) <= 1);
              ReleaseDC(0, dc);
            {$ELSE}
              expr := false;
            {$ENDIF}
          end;
          else expr := false;
        end;
      end;
      if not expr then break;
    end;

    mediatype := mediatype and expr;
    if FMediaQueries[i].Qualifier = mqNot then mediatype := not mediatype;

    Result := Result or mediatype;
    if Result then exit(true);
  end;
end;

{ TPLCSSAtRuleNamespace }

procedure TPLCSSAtRuleNamespace.SetLink(AValue: TPLCSSString);
begin
  if FLink = AValue then exit;

  AValue := AValue.Trim;
  if not AValue.ToLower.StartsWith('url(') then AValue := 'url(' + AValue + ')';
  FLink := TPLCSSTypeUrl.Create(AValue).Link;
end;

constructor TPLCSSAtRuleNamespace.Create(AValue: TPLCSSString);
var
  tab: TStringArray;
begin
  FLink := '';
  FPrefix := '';
  tab := AValue.Trim.Split(' ');

  case Length(tab) of
    1: SetLink(tab[0]);
    2: begin
      FPrefix := tab[0].Trim;
      SetLink(tab[1]);
    end;
  end;
end;

class operator TPLCSSAtRuleNamespace.=(a, b: TPLCSSAtRuleNamespace) r: boolean;
begin
  r := (a.Link = b.Link) and (a.Prefix = b.Prefix);
end;

function TPLCSSAtRuleNamespace.IsEmpty: boolean;
begin
  Result := Self = TPLCSSAtRuleNamespace.Create('');
end;

{ TPLCSSAtRuleImport }

constructor TPLCSSAtRuleImport.Create;
begin
  inherited Create;

  FMediaQueries := TPLCSSMediaQueries.Create(true);
end;

destructor TPLCSSAtRuleImport.Destroy;
begin
  FMediaQueries.Free;

  inherited Destroy;
end;

{ TPLCSSAtRuleFontFace }

constructor TPLCSSAtRuleFontFace.Create;
begin
  inherited Create;

  FMediaQueries := TPLCSSMediaQueries.Create(true);
  FDeclarations := TPLCSSProperties.Create;
end;

destructor TPLCSSAtRuleFontFace.Destroy;
begin
  FMediaQueries.Free;
  FDeclarations.Free;

  inherited Destroy;
end;

end.

