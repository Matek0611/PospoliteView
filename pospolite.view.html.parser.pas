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

  TPLHTMLParserErrorKind = (pek);

  { TPLHTMLParserError }

  TPLHTMLParserError = record
  public
    Position: TPLHTMLParserPosition;
    Kind: TPLHTMLParserErrorKind;

    constructor Create(const APosition: TPLHTMLParserPosition; const AKind: TPLHTMLParserErrorKind);
    class operator =(a, b: TPLHTMLParserError): TPLBool; inline;
  end;

  TPLHTMLParserErrors = class(specialize TPLList<TPLHTMLParserError>);

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
  const AKind: TPLHTMLParserErrorKind);
begin
  Position := APosition;
  Kind := AKind;
end;

class operator TPLHTMLParserError.=(a, b: TPLHTMLParserError): TPLBool;
begin
  Result := (a.Kind = b.Kind) and (a.Position = b.Position);
end;

end.

