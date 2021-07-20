unit Pospolite.View.HTML.Parser;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Basics;

type

  { TPLHTMLParserPosition }

  TPLHTMLParserPosition = record
  public
    Row, Column: SizeInt;

    constructor Create(const ARow, AColumn: SizeInt);
    class operator =(a, b: TPLHTMLParserPosition): TPLBool; inline;
  end;

  { TPLHTMLParserError }

  TPLHTMLParserError = record
  public
    Position: TPLHTMLParserPosition;
    Name: TPLString;

    constructor Create(const APosition: TPLHTMLParserPosition; const AName: TPLString);
    class operator =(a, b: TPLHTMLParserError): TPLBool; inline;
  end;

  { TPLHTMLParserErrors }

  TPLHTMLParserErrors = class(specialize TPLList<TPLHTMLParserError>)
  public
    function AllInOneString: TPLString;
  end;

  { IPLHTMLParser }

  IPLHTMLParser = interface
    ['{F939F08E-B228-44DD-AA3B-BEE2F82F4AF0}']
    function GetErrors: TPLHTMLParserErrors;
    function GetHasCriticalError: TPLBool;
    function GetStrict: TPLBool;
    procedure SetStrict(AValue: TPLBool);

    procedure Parse(const ASource: TPLString; var ARoot: TPLHTMLRootObject);
    procedure CleanUp;

    property Strict: TPLBool read GetStrict write SetStrict;
    property Errors: TPLHTMLParserErrors read GetErrors;
    property HasCriticalError: TPLBool read GetHasCriticalError;
  end;

  { TPLHTMLParser }

  TPLHTMLParser = class(TInterfacedObject, IPLHTMLParser)
  private
    FSource: TPLString;
    FCurrent, FEnd: ^TPLChar;
    FPos: TPLHTMLParserPosition;
    FStrict: TPLBool;
    FRoot: TPLHTMLRootObject;
    FErrors: TPLHTMLParserErrors;
    function GetErrors: TPLHTMLParserErrors;
    function GetHasCriticalError: TPLBool;
    function GetStrict: TPLBool;
    procedure SetStrict(AValue: TPLBool);
    procedure MovePosToNextRow;
    procedure MovePosForward;
    function IsVoidElement(AName: TPLString): TPLBool; inline;
    function IsEOF: TPLBool; inline;
    procedure Consume(AChar: TPLChar; AMoveNext: TPLBool = true);
    procedure ConsumeWhitespace;
    function Position: SizeInt; inline;
    function ReadObject(AParent: TPLHTMLBasicObject): TPLHTMLBasicObject;
    function ReadVoidObject(AParent: TPLHTMLBasicObject; AStart, AEnd, AName: TPLString): TPLHTMLVoidObject;
    function ReadNormalObject(AParent: TPLHTMLBasicObject): TPLHTMLBasicObject;
    function ReadText(AChars: array of TPLString): TPLString;
    procedure ReadObjectAttribute(var AObject: TPLHTMLBasicObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(const ASource: TPLString; var ARoot: TPLHTMLRootObject);
    procedure CleanUp;

    property Strict: TPLBool read GetStrict write SetStrict;
    property Errors: TPLHTMLParserErrors read GetErrors;
    property HasCriticalError: TPLBool read GetHasCriticalError;
  end;

implementation

{ TPLHTMLParserPosition }

constructor TPLHTMLParserPosition.Create(const ARow, AColumn: SizeInt);
begin
  Row := ARow;
  Column := AColumn;
end;

class operator TPLHTMLParserPosition.=(a, b: TPLHTMLParserPosition): TPLBool;
begin
  Result := (a.Row = b.Row) and (a.Column = b.Column);
end;

{ TPLHTMLParserError }

constructor TPLHTMLParserError.Create(const APosition: TPLHTMLParserPosition;
  const AName: TPLString);
begin
  Position := APosition;
  Name := AName;
end;

class operator TPLHTMLParserError.=(a, b: TPLHTMLParserError): TPLBool;
begin
  Result := (a.Name = b.Name) and (a.Position = b.Position);
end;

{ TPLHTMLParserErrors }

function TPLHTMLParserErrors.AllInOneString: TPLString;
var
  e: TPLHTMLParserError;
begin
  Result := '';

  for e in self do Result += '(%d, %d) %s'.Format([e.Position.Row, e.Position.Column, e.Name]) + LineEnding;
end;

{ TPLHTMLParser }

function TPLHTMLParser.GetErrors: TPLHTMLParserErrors;
begin
  Result := FErrors;
end;

function TPLHTMLParser.GetHasCriticalError: TPLBool;
var
  e: TPLHTMLParserError;
begin
  Result := false;

  for e in FErrors do
    if e.Name.Exists('CRITICAL') then exit(true);
end;

function TPLHTMLParser.GetStrict: TPLBool;
begin
  Result := FStrict;
end;

procedure TPLHTMLParser.SetStrict(AValue: TPLBool);
begin
  FStrict := AValue;
end;

procedure TPLHTMLParser.MovePosToNextRow;
begin
  FPos.Row += 1;
  FPos.Column := 1;
end;

procedure TPLHTMLParser.MovePosForward;
begin
  if FCurrent^ = #13 then MovePosToNextRow else FPos.Column += 1;
  Inc(FCurrent);
end;

function TPLHTMLParser.IsVoidElement(AName: TPLString): TPLBool;
begin
  Result := AName.ToLower in TPLHTMLObjectFactory.VoidElements;
end;

function TPLHTMLParser.IsEOF: TPLBool;
begin
  Result := FCurrent > FEnd;
end;

procedure TPLHTMLParser.Consume(AChar: TPLChar; AMoveNext: TPLBool);
var
  p: SizeInt;
begin
  if IsEOF then begin
    FErrors.Add(TPLHTMLParserError.Create(FPos, 'CRITICAL: Unexpected end of file.'));
    exit;
  end;

  if FCurrent^ <> AChar then begin
    FErrors.Add(TPLHTMLParserError.Create(FPos, '"%s" expected but "%s" found.'.Format([AChar, FCurrent^])));

    //p := FSource.Find(AChar, Position);
    //if p > 0 then begin
    //  while FCurrent^ <> AChar do MovePosForward;
    //end else begin
    //  FCurrent := FEnd + 1;
    //  FPos.Column := FSource.Length;
    //end;

    //exit;
  end;

  if AMoveNext then MovePosForward;
end;

procedure TPLHTMLParser.ConsumeWhitespace;
begin
  while not IsEOF and FCurrent^.IsWhitespace do MovePosForward;
end;

function TPLHTMLParser.Position: SizeInt;
begin
  Result := FCurrent - @FSource[1] + 1;
end;

function TPLHTMLParser.ReadObject(AParent: TPLHTMLBasicObject
  ): TPLHTMLBasicObject;
begin
  Consume('<', false);
  if IsEOF then exit(nil);

  if FSource.SubStr(Position, 4) = '<!--' then
    Result := ReadVoidObject(AParent, '<!--', '-->', 'comment')
  else if FSource.SubStr(Position, 9) = '<!DOCTYPE' then
    Result := ReadVoidObject(AParent, '<!DOCTYPE', '>', 'DOCTYPE')
  else if FSource.SubStr(Position, 9) = '<!CDATA[' then
    Result := ReadVoidObject(AParent, '<!CDATA[', ']]>', 'CDATA')
  else if FSource.SubStr(Position, 2) = '<!' then
    Result := ReadVoidObject(AParent, '<!', '>', 'void')
  else
    Result := ReadNormalObject(AParent);
end;

function TPLHTMLParser.ReadVoidObject(AParent: TPLHTMLBasicObject; AStart,
  AEnd, AName: TPLString): TPLHTMLVoidObject;
var
  pom: TPLChar;
  s, l: SizeInt;
begin
  for pom in AStart do Consume(pom);
  s := Position;
  l := AEnd.Length;
  Result := nil;

  while not IsEOF and (FSource.SubStr(Position, l) <> AEnd) do MovePosForward;

  if FSource.SubStr(Position, l) = AEnd then begin
    Result := TPLHTMLVoidObject.Create(AParent, AParent.Renderer);
    Result.Position := s;
    Result.Text := FSource.SubStr(s, Position - s - 1).Trim;
    Result.Name := AName;
    Inc(FCurrent, l);
  end else FErrors.Add(TPLHTMLParserError.Create(FPos, '"%s" tag ending expected but "%s" found.'.Format([AEnd, FSource.SubStr(Position, l)])));
end;

function TPLHTMLParser.ReadNormalObject(AParent: TPLHTMLBasicObject
  ): TPLHTMLBasicObject;
var
  obj: TPLHTMLBasicObject;
  un: TPLBool = false;
  eof: TPLBool;
  n: TPLString;
  s: ^TPLChar;
  arrc: specialize TArray<TPLString>;
begin
  Result := nil;
  arrc := TPLStringFuncs.NewArray(TPLString.WhitespacesArrayString) + TPLStringFuncs.NewArray(['/', '>']);

  Consume('<');
  ConsumeWhitespace;
  n := ReadText(arrc).ToLower;
  ConsumeWhitespace;

  try
    Result := TPLHTMLObjectFactory.CreateObjectByTagName(n, AParent);

    while not IsEOF and not (FCurrent^ in ['/', '>']) do begin
      ReadObjectAttribute(Result);
      ConsumeWhitespace;
    end;

    if IsVoidElement(n) then begin
      while not IsEOF and (FCurrent^ in arrc) do MovePosForward;
    end else begin
      if FCurrent^ = '>' then begin
        Consume('>');

        repeat
          while not IsEOF and (FCurrent^ <> '/') do MovePosForward;

          s := FCurrent;
          Consume('<');
          eof := FCurrent^ = '/';

          if eof then begin
            Consume('/');
            n := ReadText(arrc).ToLower;
            if n <> Result.Name then begin
              un := true;
              FCurrent := s;
            end;
          end else if not FCurrent^.IsWhiteSpace then begin
            FCurrent := s;
            obj := ReadObject(Result);
            if Assigned(obj) then Result.Children.Add(obj);
          end;
        until eof;
      end;

      if not un then Consume('>');
    end;
  except
    on e: Exception do begin
      if Assigned(Result) then FreeAndNil(Result);
      FErrors.Add(TPLHTMLParserError.Create(FPos, 'CRITICAL: Unexpected error: ' + e.Message));
    end;
  end;
end;

function TPLHTMLParser.ReadText(AChars: array of TPLString): TPLString;
var
  s: SizeInt;
begin
  s := Position;
  while not IsEOF and not (FCurrent^ in TPLStringFuncs.NewArray(AChars)) do MovePosForward;

  if not IsEOF and (FCurrent^ in TPLStringFuncs.NewArray(AChars)) then
    Result := FSource.SubStr(s, Position - s)
  else
    Result := '';
end;

procedure TPLHTMLParser.ReadObjectAttribute(var AObject: TPLHTMLBasicObject);
var
  q: TPLChar;
  attr: TPLHTMLObjectAttribute;
  r: TPLBool;
begin
  attr.Key := ReadText(['=']);
  ConsumeWhitespace;
  Consume('=');
  ConsumeWhitespace;

  if FCurrent^ in ['"', ''''] then q := FCurrent^ else begin
    if FStrict then FErrors.Add(TPLHTMLParserError.Create(FPos, '"%s" attribute value is not strict.'.Format([FCurrent^])));
    q := #0;
  end;

  if q <> #0 then begin
    Consume(q);
    ConsumeWhitespace;
    attr.Value := '';

    repeat
      attr.Value += ReadText([q]);
      ConsumeWhitespace;
      Consume(q);
      ConsumeWhitespace;
      r := FCurrent^ <> q;

      if not r then begin
        Consume(q);
        attr.Value += q;
      end;
    until r;
  end else if not FStrict then
    attr.Value := ReadText(TPLStringFuncs.NewArray(TPLString.WhitespacesArrayString) + TPLStringFuncs.NewArray(['/', '>']))
  else exit;

  AObject.Attributes.Add(attr);
end;

constructor TPLHTMLParser.Create;
begin
  inherited Create;

  FErrors := TPLHTMLParserErrors.Create;
  FStrict := false;
  CleanUp;
end;

destructor TPLHTMLParser.Destroy;
begin
  FErrors.Free;

  inherited Destroy;
end;

procedure TPLHTMLParser.Parse(const ASource: TPLString; var ARoot: TPLHTMLRootObject
  );
var
  obj: TPLHTMLBasicObject;
begin
  CleanUp;

  if ASource.Length < 2 then exit;

  FSource := ASource;
  FCurrent := @ASource[1];
  FEnd := @ASource[ASource.Length];
  FPos := TPLHTMLParserPosition.Create(1, 1);
  FRoot := ARoot;

  ConsumeWhitespace;
  while not IsEOF do begin
    obj := ReadObject(FRoot);
    if Assigned(obj) then
      FRoot.Children.Add(obj);
    ConsumeWhitespace;
  end;
end;

procedure TPLHTMLParser.CleanUp;
begin
  FErrors.Clear;
  FSource := '';
  FCurrent := nil;
  FEnd := nil;
  FPos := TPLHTMLParserPosition.Create(0, 0);
end;

end.

