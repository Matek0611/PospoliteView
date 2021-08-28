unit PospoLiteHTML.CSS.Parser;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Graphics, Controls, strutils, contnrs, character,
  LazUTF8, PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Stylesheet,
  PospoLiteHTML.CSS.Properties, PospoLiteHTML.CSS.Media, PospoLiteHTML.CSS.Rules;

type

  { IPLCSSParser }

  IPLCSSParser = interface
  ['{D1DB2FF4-390D-403B-979F-5E349AE65F67}']
    function GetCode: TPLCSSString;
    procedure SetCode(AValue: TPLCSSString);
    function GetErrors: TPLCSSString;

    procedure ParseAsStyleSheet(var AStyleSheet: TPLCSSStyleSheet); overload;
    procedure ParseAsDeclarations(var ADeclarations: TPLCSSProperties); overload;
    function ParseAsStyleSheet: TPLCSSStyleSheet; overload;
    function ParseAsDeclarations: TPLCSSProperties; overload;

    property Code: TPLCSSString read GetCode write SetCode;
    property Errors: TPLCSSString read GetErrors;
  end;

  { TPLCSSParserError }

  TPLCSSParserError = packed record
  public
    Message: TPLCSSString;
    Position: TPLCSSInt;

    constructor Create(APos: SizeInt; AMsg: TPLCSSString);
    class operator = (a, b: TPLCSSParserError) r: boolean;

    function ToString: TPLCSSString;
  end;

  { TPLCSSParserErrors }

  TPLCSSParserErrors = class(specialize TPLList<TPLCSSParserError>);

  { TPLCSSParser }

  TPLCSSParser = class(TInterfacedObject, IPLCSSParser)
  private
    FCode: TPLCSSString;
    FCodeStart, FCodePos, FCodeEnd: ^TPLCSSChar;
    FErrors: TPLCSSParserErrors;
    function GetCode: TPLCSSString;
    procedure SetCode(AValue: TPLCSSString);
    function GetErrors: TPLCSSString;
  protected
    procedure Reset;
    procedure ParseStyleSheet(var AStyleSheet: TPLCSSStyleSheet);
    procedure ParseDeclarations(var ADeclarations: TPLCSSProperties);
    procedure ReportError(const AMessage: TPLCSSString);
    function IsDone: boolean;
    function Position: SizeInt;

    procedure SkipWhitespace;
    procedure SkipComment;
    procedure SkipNextToken;
    function SkipAtRule: boolean;
    function SkipMatching: boolean;
    function SkipPreviousDelimiter(const ADelimiter: TPLCSSChar): boolean;
    procedure SkipToMediaQueryEnd;
    procedure SkipAny;

    procedure ParseStatement(const AMedia: TPLCSSMediaQueries; var AStyles: TPLCSSStyleSheet);
    function ParseIdent: TPLCSSString;
    function ParseEscape: TPLCSSString;
    function ParseString(const ADelimiter: TPLCSSChar): TPLCSSString;
    function ParseIdentOrString: TPLCSSString;
    function ParseImport: TPLCSSAtRuleImport;
    function ParseURL: TPLCSSString;
    procedure ParseMediaQueries(var AMedia: TPLCSSMediaQueries);
    function ParseMediaQuery: TPLCSSMediaQuery;
    function ParseCharset: TPLCSSString;
  public
    constructor Create(const ACode: TPLCSSString);
    destructor Destroy; override;

    procedure ParseAsStyleSheet(var AStyleSheet: TPLCSSStyleSheet); overload;
    procedure ParseAsDeclarations(var ADeclarations: TPLCSSProperties); overload;
    function ParseAsStyleSheet: TPLCSSStyleSheet; overload;
    function ParseAsDeclarations: TPLCSSProperties; overload;

    property Code: TPLCSSString read GetCode write SetCode;
    property Errors: TPLCSSString read GetErrors;
  end;

  { TPLCSSHelperForStylesheet }

  TPLCSSHelperForStylesheet = class helper for TPLCSSStyleSheet
  public
    procedure Add(AStyles: TPLCSSString);
  end;

  function NewParser(ACode: string): TPLCSSParser;

implementation

function NewParser(ACode: string): TPLCSSParser;
begin
  Result := TPLCSSParser.Create(ACode);
end;

{ TPLCSSHelperForStylesheet }

procedure TPLCSSHelperForStylesheet.Add(AStyles: TPLCSSString);
begin

end;

{ TPLCSSParserError }

constructor TPLCSSParserError.Create(APos: SizeInt; AMsg: TPLCSSString);
begin
  Position := APos;
  Message := AMsg;
end;

class operator TPLCSSParserError.=(a, b: TPLCSSParserError) r: boolean;
begin
  r := a.ToString = b.ToString;
end;

function TPLCSSParserError.ToString: TPLCSSString;
begin
  Result := 'Error at 0x%x: %s'.Format([Position, Message]);
end;

{ TPLCSSParser }

function TPLCSSParser.GetCode: TPLCSSString;
begin
  Result := FCode;
end;

procedure TPLCSSParser.SetCode(AValue: TPLCSSString);
begin
  if AValue = FCode then exit;

  FCode := AValue;
  FErrors.Clear;
end;

function TPLCSSParser.GetErrors: TPLCSSString;
var
  i: SizeInt;
begin
  Result := '';

  for i := 0 to FErrors.Count-1 do
    Result += FErrors[i].ToString + LineEnding;
end;

procedure TPLCSSParser.Reset;
begin
  FCodeStart := @FCode[1];
  FCodeEnd := @FCode[FCode.Length];
  FCodePos := FCodeStart;

  FErrors.Clear;
end;

procedure TPLCSSParser.ParseStyleSheet(var AStyleSheet: TPLCSSStyleSheet);
begin
  if FCode.Length = 0 then exit;
  Reset;

  SkipWhitespace;
  if IsDone then exit;

  while FCodePos < FCodeEnd do begin
    case FCodePos^ of
      '<': begin
        FCodePos := FCodePos + 1;
        if (FCodeEnd - FCodePos >= 3) and TPLCSSString(FCodePos).Exists('!--') then
          FCodePos := FCodePos + 3 else ReportError('Wrong beginning of the HTML comment.');
      end;
      '-': begin
        FCodePos := FCodePos + 1;
        if (FCodeEnd - FCodePos >= 2) and TPLCSSString(FCodePos).Exists('->') then
          FCodePos := FCodePos + 2 else ReportError('Wrong ending of the HTML comment.');
      end;
      else ParseStatement(nil, AStyleSheet);
    end;

    SkipWhitespace;
  end;

  if not IsDone then ReportError('Unknown error occured.');
end;

procedure TPLCSSParser.ParseDeclarations(var ADeclarations: TPLCSSProperties);
begin
  if FCode.Length = 0 then exit;
  Reset;


end;

procedure TPLCSSParser.ReportError(const AMessage: TPLCSSString);
begin
  FErrors.Add(TPLCSSParserError.Create(FCodePos, AMessage));
end;

function TPLCSSParser.IsDone: boolean;
begin
  Result := FCodePos >= FCodeEnd;
end;

function TPLCSSParser.Position: SizeInt;
begin
  Result := FCodeEnd - FCodePos;
end;

procedure TPLCSSParser.SkipWhitespace;
begin
  while FCodePos < FCodeEnd do begin
    if FCodePos^.IsWhiteSpace then FCodePos += 1
    else if (FCodePos + 1 < FCodeEnd) and (FCodePos[0] = '/') and (FCodePos[1] = '*') then SkipComment
    else break;
  end;
end;

procedure TPLCSSParser.SkipComment;
begin
  if (FCodePos + 2 <= FCodeEnd) and (FCodePos[0] = '/') and (FCodePos[1] = '*') then begin
    FCodePos += 2;
    exit;
  end;

  FCodePos := FCodePos + 2;
  while FCodePos + 1 < FCodeEnd do begin
    if (FCodePos[0] = '*') and (FCodePos[1] = '/') then begin
      FCodePos := FCodePos + 2;
      exit;
    end else FCodePos := FCodePos + 1;
  end;

  ReportError('Comment is not closed until EOF.');
  FCodePos := FCodeEnd;
end;

procedure TPLCSSParser.SkipNextToken;
begin
  SkipWhitespace;
  if IsDone then exit;

  case FCodePos^ of
    '''', '"': ParseString(FCodePos^);
    '\': ParseEscape;
    else FCodePos := FCodePos + 1;
  end;
end;

function TPLCSSParser.SkipAtRule: boolean;
begin
  SkipWhitespace;

  while FCodePos < FCodeEnd do begin
    case FCodePos^ of
      '}': exit(true);
      ';': begin
        FCodePos := FCodePos + 1;
        exit(true);
      end;
      '{': exit(SkipMatching);
      '[', '(': SkipMatching;
       else SkipNextToken;
    end;
  end;

  Result := false;
end;

function TPLCSSParser.SkipMatching: boolean;
var
  delims: TPLCSSString = '';
begin
  if not (FCodePos^ in ['{', '[', '(']) then exit(false);

  ReportError('Bracket blocks are ignored.');

  case FCodePos^ of
    '(': begin
      FCodePos := FCodePos + 1;
      delims += ')';
    end;
    '[': begin
      FCodePos := FCodePos + 1;
      delims += ']';
    end;
    '{': begin
      FCodePos := FCodePos + 1;
      delims += '}';
    end;
    else exit(false);
  end;

  SkipWhitespace;

  while FCodePos < FCodeEnd do begin
    if FCodePos^ = delims[delims.Length] then begin
      FCodePos := FCodePos + 1;
      delims := delims.SubStr(1, delims.Length - 1);
      if delims = '' then exit(true);
    end else begin
      case FCodePos^ of
        '(': begin
          FCodePos := FCodePos + 1;
          delims += ')';
        end;
        '[': begin
          FCodePos := FCodePos + 1;
          delims += ']';
        end;
        '{': begin
          FCodePos := FCodePos + 1;
          delims += '}';
        end;
        else SkipNextToken;
      end;
    end;

    SkipWhitespace;
  end;

  Result := false;
end;

procedure TPLCSSParser.ParseStatement(const AMedia: TPLCSSMediaQueries;
  var AStyles: TPLCSSStyleSheet);
var
  correct: boolean;
  ident, tmp: TPLCSSString;
  import: TPLCSSAtRuleImport;
  media: TPLCSSMediaQueries;
begin
  SkipWhitespace;
  if IsDone then exit;

  if FCodePos^ = '@' then begin
    correct := true;
    FCodePos := FCodePos + 1;
    ident := ParseIdent;

    case ident of
      'import': begin
        if Assigned(AMedia) then begin
          ReportError('@import is not allowed inside @media.');
          correct := SkipAtRule;
        end else if not AStyles.Rulesets.Empty or not AStyles.FontFaces.Empty then begin
          ReportError('"@import" at-rule found after rulesets.');
          correct := SkipAtRule;
        end else begin
          import := ParseImport;
          SkipWhitespace;

          if Assigned(import) then begin
            if IsDone then begin
              ReportError('Unexpected EOF found in @import.');
              correct := false;
              AStyles.Imports.Add(import);
            end else if FCodePos^ = ';' then begin
              FCodePos := FCodePos + 1;
              AStyles.Imports.Add(import);
            end else begin
              ReportError('Ignoring characters until the end of @import.');
              correct := SkipAtRule;
            end;
          end else begin
            ReportError('Parsing @import statement failed.');
            correct := SkipAtRule;
          end;
        end;
      end;
      'charset': begin
        if Assigned(AMedia) then begin
          ReportError('@charset is not allowed inside @media.');
          correct := SkipAtRule;
        end else if not AStyles.Rulesets.Empty or not AStyles.Imports.Empty or not AStyles.FontFaces.Empty then begin
          ReportError('@charset is not allowed after other rules.');
          correct := SkipAtRule;
        end else begin
          tmp := ParseCharset;
          SkipWhitespace;

          if IsDone then begin
            ReportError('Unexpected EOF found in @charset.');
            correct := false;
            AStyles.Charsets.Add(tmp);
          end else begin
            if FCodePos^ = ';' then begin
              FCodePos := FCodePos + 1;
              AStyles.Charsets.Add(tmp);
            end else begin
              ReportError('Ignoring characters until the end of @charset.');
              correct := SkipAtRule;
            end;
          end;
        end;
      end;
      'media': begin
        if Assigned(AMedia) then begin
          ReportError('@media cannot be nested inside @media.');
          correct := SkipAtRule;
        end else begin
          media := TPLCSSMediaQueries.Create(true);

        end;
      end;
      'font-face': begin

      end;
      'namespace': begin
        if Assigned(AMedia) then begin
          ReportError('@namespace is not allowed inside @media.');
          correct := SkipAtRule;
        end else if not AStyles.Namespace.IsEmpty then begin
          ReportError('@namespace must be only one.');
          correct := SkipAtRule;
        end else begin
          SkipWhitespace;
          tmp := '';

          while not IsDone and (FCodePos^ <> ';') do begin
            tmp += FCodePos^;
            FCodePos := FCodePos + 1;
          end;

          AStyles.Namespace := TPLCSSAtRuleNamespace.Create(tmp);
        end;
      end;
      //'keyframes': begin
      //  // to do
      //end;
      else begin
        ReportError('@%s is unknown.'.Format([ident]));
        correct := SkipAtRule;
      end;
    end;
  end;
end;

function TPLCSSParser.ParseIdent: TPLCSSString;
begin
  Result := '';

  while FCodePos <= FCodeEnd do begin
    if (FCodePos^ in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then begin
      Result += FCodePos^;
      FCodePos := FCodePos + 1;
    end else if (FCodePos^ = '\') then Result += ParseEscape
    else break;
  end;
end;

function TPLCSSParser.ParseEscape: TPLCSSString;
var
  dh, i: integer;
  codepoint: Cardinal;
  tmp: TPLCSSString = '';
begin
  Result := '';
  codepoint := 0;

  SkipWhitespace;
  if IsDone or (FCodePos^ <> '\') then exit;

  FCodePos := FCodePos + 1;
  if IsDone then exit('\');

  dh := FCodePos^.FromHex;
  if dh = -1 then begin
    tmp := FCodePos^.FromC;
    FCodePos := FCodePos + 1;
    if (tmp.Length <> 1) then begin
      Result := ' ';
      ReportError('UTF-32 to UTF-8 parsing error.');
    end else Result := tmp;
  end else begin
    for i := 0 to 5 do begin
      if IsDone then break;

      dh := FCodePos^.FromHex;
      if dh = -1 then break;

      FCodePos := FCodePos + 1;
      codepoint := (codepoint << 4) or dh;
    end;
    if (FCodeEnd - FCodePos >= 2) and TPLCSSString(FCodePos).Exists(#13#10) then
      FCodePos := FCodePos + 2
    else if (FCodePos <= FCodeEnd) and FCodePos^.IsWhiteSpace then
      FCodePos := FCodePos + 1;

    Result := UnicodeToUTF8(codepoint);
    if Result = #0 then begin
      Result := ' ';
      ReportError('Invalid Unicode value "0x%x" found.'.Format([codepoint]));
    end;
  end;
end;

function TPLCSSParser.ParseString(const ADelimiter: TPLCSSChar): TPLCSSString;
begin
  Result := '';

  SkipWhitespace;
  if IsDone or (FCodePos^ <> ADelimiter) then exit;

  FCodePos := FCodePos + 1;
  if IsDone then exit;

  while not IsDone do begin
    if FCodePos^ = ADelimiter then begin
      FCodePos := FCodePos + 1;
      exit;
    end else
    case FCodePos^ of
      #13: exit;
      '\': begin
        if (FCodePos + 1 < FCodeEnd) and (FCodePos[1] = #13) then
          FCodePos := FCodePos + 2
        else
          Result += ParseEscape;
      end;
      else begin
        Result += FCodePos^;
        FCodePos := FCodePos + 1;
      end;
    end;
  end;
end;

function TPLCSSParser.ParseIdentOrString: TPLCSSString;
begin
  Result := '';
  SkipWhitespace;
  if IsDone then exit;

  if FCodePos^ in ['''', '"'] then Result := ParseString(FCodePos^)
  else Result := ParseIdent;
end;

function TPLCSSParser.ParseImport: TPLCSSAtRuleImport;
var
  mq: TPLCSSMediaQueries;
begin
  SkipWhitespace;
  if IsDone then exit(nil);

  Result := TPLCSSAtRuleImport.Create;

  Result.Link := ParseURL;
  SkipWhitespace;

  if not (IsDone or (FCodePos^ = ';')) then begin
    mq := Result.MediaQueries;
    ParseMediaQueries(mq);
  end;
end;

function TPLCSSParser.ParseURL: TPLCSSString;
begin
  Result := '';

  SkipWhitespace;
  if IsDone then exit;

  if FCodePos^ in ['''', '"'] then Result := ParseString(FCodePos^) else begin
    while not IsDone do begin
      if FCodePos^.IsWhiteSpace or (FCodePos^ = ')') then break else
        if FCodePos^ = '\' then Result += ParseEscape else begin
          Result += FCodePos^;
          FCodePos := FCodePos + 1;
        end;
    end;
  end;

  SkipWhitespace;
  if not(not IsDone and (FCodePos^ = ')')) then Result := '';
end;

procedure TPLCSSParser.ParseMediaQueries(var AMedia: TPLCSSMediaQueries);
var
  query: TPLCSSMediaQuery;
begin
  SkipWhitespace;

  if IsDone or (FCodePos^ in [';', '{']) then exit;

  while not IsDone do begin
    query := ParseMediaQuery;

    if not Assigned(query) then begin
      query := TPLCSSMediaQuery.Create;
      query.Qualifier := mqNot;
      query.MediaType := 'all';
    end;

    AMedia.Add(query);

    SkipWhitespace;
    if IsDone then break;

    case FCodePos^ of
      ',': FCodePos := FCodePos + 1;
      '{', ';': break;
      else begin
        ReportError('Unexpected character "%s" found in media query.'.Format([FCodePos^]));
        break;
      end;
    end;
  end;
end;

function TPLCSSParser.ParseMediaQuery: TPLCSSMediaQuery;
var
  id, tmp: TPLCSSString;
  tmp_pos_s, tmp_pos_e: ^TPLCSSChar;
  and_needed, and_found, all_done: boolean;
begin
  SkipWhitespace;

  Result := TPLCSSMediaQuery.Create;
  id := ParseIdent;
  SkipWhitespace;

  if id = 'not' then begin
    Result.Qualifier := mqNot;
    id := ParseIdent;
  end else if id = 'only' then begin
    Result.Qualifier := mqOnly;
    id := ParseIdent;
  end;

  and_found := false;
  and_needed := false;
  all_done := false;

  if id = '' then begin
    Result.MediaType := id;
    and_needed := true;
  end;
  SkipWhitespace;

  while not (IsDone or all_done) do begin
    case FCodePos^ of
      ';', '{', ',': all_done := true;
      '(': begin
        if and_needed <> and_found then begin
          ReportError('Too much or less "and" in media query');
          Result.Free;
          exit(nil);
        end;

        and_needed := true;
        and_found := false;
        FCodePos := FCodePos + 1;
        SkipWhitespace;
        tmp := ParseIdent;
        SkipWhitespace;

        if IsDone then begin
          ReportError('EOF found instead of content in media query.');
          Result.Free;
          exit(nil);
        end;

        case FCodePos^ of
          ')': begin
            FCodePos := FCodePos + 1;
            SkipWhitespace;
            Result.Expressions.Add(TPLCSSMediaExpression.Create(tmp));
          end;
          ':': begin
            FCodePos := FCodePos + 1;
            SkipWhitespace;

            if IsDone then begin
              ReportError('EOF found instead of the content in media query.');
              Result.Free;
              exit(nil);
            end;

            tmp_pos_s := FCodePos;
            if SkipPreviousDelimiter(')') then begin
              tmp_pos_e := FCodeEnd - 1;
              Result.Expressions.Add(TPLCSSMediaExpression.Create(tmp, UTF8Copy(TPLCSSString(tmp_pos_s), 1, tmp_pos_e - tmp_pos_s + 1)));
            end else begin
              ReportError('Media query is not closed.');
              SkipToMediaQueryEnd;
              Result.Free;
              exit(nil);
            end;
          end;
          else begin
            ReportError('Unparsable media expression.');
            SkipPreviousDelimiter(')');
            SkipToMediaQueryEnd;
            Result.Free;
            exit(nil);
          end;
        end;
      end;
      else begin
        tmp := ParseIdent.ToLower;
        case tmp of
          'or': all_done := true; // or = ,
          'and': begin
            if and_found then begin
              ReportError('"and" found multiple times in a row.');
              SkipToMediaQueryEnd;
              Result.Free;
              exit(nil);
            end else if not IsDone and (FCodePos^ = '(') then begin
              ReportError('"(" found instead of space.');
              SkipToMediaQueryEnd;
              Result.Free;
              exit(nil);
            end else begin
              and_found := true;
            end;
          end;
          else begin
            if IsDone then ReportError('Unexpected EOF found.')
            else if tmp = '' then ReportError('Unexpected character "%s" found in media query.'.Format([FCodePos^]))
            else ReportError('Unexpected identifier separating media queries "%s" found.'.Format([tmp]));
            SkipToMediaQueryEnd;
            Result.Free;
            exit(nil);
          end;
        end;
      end;
    end;
    SkipWhitespace;
  end;

  if and_found then begin
    ReportError('"and" cannot end the media query.');
    SkipToMediaQueryEnd;
    Result.Free;
    exit(nil);
  end;

  if (Result.MediaType = '') and (Result.Expressions.Empty) then begin
    ReportError('Media query is empty.');
    Result.Reset;
  end;
end;

function TPLCSSParser.ParseCharset: TPLCSSString;
begin
  Result := '';
  SkipWhitespace;

  if IsDone then begin
    ReportError('Unexpected EOF found inside @charset.');
    exit;
  end;

  if FCodePos^ in [#39, '"'] then Result := ParseString(FCodePos^) else ReportError('@charset does not contain string.');
end;

function TPLCSSParser.SkipPreviousDelimiter(const ADelimiter: TPLCSSChar
  ): boolean;
begin
  Result := false;
  SkipWhitespace;

  while not IsDone do begin
    if FCodePos^ = ADelimiter then begin
      FCodePos := FCodePos + 1;
      exit(true);
    end else begin
      if FCodePos^ in ['(', '[', '{'] then SkipMatching else SkipNextToken;
    end;
    SkipWhitespace;
  end;
end;

procedure TPLCSSParser.SkipToMediaQueryEnd;
begin
  SkipWhitespace;

  while not IsDone do begin
    if FCodePos^ in [',', '{', ';', '}'] then exit;
    if FCodePos^ in ['(', '['] then SkipMatching else SkipAny;
    SkipWhitespace;
  end;
end;

procedure TPLCSSParser.SkipAny;
begin
  SkipWhitespace;

  while not IsDone do begin
    if FCodePos^ in [';', ' ', ':', '{', '}', '[', ']'] then begin
      FCodePos := FCodePos + 1;
      break;
    end;

    FCodePos := FCodePos + 1;
  end;
end;

constructor TPLCSSParser.Create(const ACode: TPLCSSString);
begin
  inherited Create;

  FErrors := TPLCSSParserErrors.Create;

  Code := ACode;
end;

destructor TPLCSSParser.Destroy;
begin
  FErrors.Free;

  inherited Destroy;
end;

procedure TPLCSSParser.ParseAsStyleSheet(var AStyleSheet: TPLCSSStyleSheet);
begin
  AStyleSheet.Clear;
  ParseStyleSheet(AStyleSheet);
end;

procedure TPLCSSParser.ParseAsDeclarations(var ADeclarations: TPLCSSProperties);
begin
  ADeclarations.Clear;
  ParseDeclarations(ADeclarations);
end;

function TPLCSSParser.ParseAsStyleSheet: TPLCSSStyleSheet;
begin
  Result := TPLCSSStyleSheet.Create;
  ParseStyleSheet(Result);
end;

function TPLCSSParser.ParseAsDeclarations: TPLCSSProperties;
begin
  Result := TPLCSSProperties.Create;
  ParseDeclarations(Result);
end;

end.

