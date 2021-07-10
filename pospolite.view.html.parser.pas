unit Pospolite.View.HTML.Parser;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

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

  TPLHTMLParserErrors = class(specialize TPLList<TPLHTMLParserError>);

  { IPLHTMLParser }

  IPLHTMLParser = interface
    ['{F939F08E-B228-44DD-AA3B-BEE2F82F4AF0}']
    function GetErrors: TPLHTMLParserErrors;
    function GetStrict: TPLBool;
    procedure SetStrict(AValue: TPLBool);

    procedure Parse(const ASource: TPLString; var ARoot: IPLHTMLObject);
    procedure CleanUp;

    property Strict: TPLBool read GetStrict write SetStrict;
    property Errors: TPLHTMLParserErrors read GetErrors;
  end;

  { TPLHTMLParser }

  TPLHTMLParser = class(TInterfacedObject, IPLHTMLParser)
  private
    FSource: TPLString;
    FStart, FEnd: ^TPLChar;
    FPos: TPLHTMLParserPosition;
    FStrict: TPLBool;
    FRoot: IPLHTMLObject;
    FErrors: TPLHTMLParserErrors;
    function GetErrors: TPLHTMLParserErrors;
    function GetStrict: TPLBool;
    procedure SetStrict(AValue: TPLBool);
    procedure MovePosToNextRow;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(const ASource: TPLString; var ARoot: IPLHTMLObject);
    procedure CleanUp;

    property Strict: TPLBool read GetStrict write SetStrict;
    property Errors: TPLHTMLParserErrors read GetErrors;
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

{ TPLHTMLParser }

function TPLHTMLParser.GetErrors: TPLHTMLParserErrors;
begin
  Result := FErrors;
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

procedure TPLHTMLParser.Parse(const ASource: TPLString; var ARoot: IPLHTMLObject
  );
begin
  CleanUp;

  if ASource.Length < 2 then exit;

  FSource := ASource;
  FStart := @ASource[1];
  FEnd := @ASource[ASource.Length];
  FPos := TPLHTMLParserPosition.Create(1, 1);
  FRoot := ARoot;
end;

procedure TPLHTMLParser.CleanUp;
begin
  FErrors.Clear;
  FSource := '';
  FStart := nil;
  FEnd := nil;
  FPos := TPLHTMLParserPosition.Create(0, 0);
end;

end.

