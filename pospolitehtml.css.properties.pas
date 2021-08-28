unit PospoLiteHTML.CSS.Properties;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, strutils, PospoLiteHTML.CSS.Basics;

type

  { TPLCSSProperty }

  TPLCSSProperty = packed record
  strict private
    FImportant: boolean;
    FName: TPLCSSString;
    FValue: TPLCSSString;
  public
    constructor Create(const AName, AValue: TPLCSSString);

    class operator = (a, b: TPLCSSProperty) r: boolean;
    function IsInit: boolean;
    function IsInherit: boolean;

    property Name: TPLCSSString read FName;
    property Value: TPLCSSString read FValue;
    property Important: boolean read FImportant;
  end;

  operator := (a: TPLCSSProperty) r: TPLCSSString;

type

  { TPLCSSProperties }

  TPLCSSProperties = class(specialize TPLList<TPLCSSProperty>);

implementation

{ TPLCSSProperty }

constructor TPLCSSProperty.Create(const AName, AValue: TPLCSSString);
begin
  FName := AName.Trim;
  FValue := AValue.Trim;
  FImportant := ProcessImportant(FValue);
end;

class operator TPLCSSProperty.=(a, b: TPLCSSProperty) r: boolean;
begin
  r := (a.FName = b.FName) and (a.FValue = b.FValue);
end;

function TPLCSSProperty.IsInit: boolean;
begin
  Result := (FValue = 'initial') or (FValue = 'revert') or (FValue = '');
end;

function TPLCSSProperty.IsInherit: boolean;
begin
  Result := (FValue = 'inherit') or (FValue = 'unset');
end;

operator := (a: TPLCSSProperty) r: TPLCSSString;
begin
  r := a.Name + ': ' + a.Value + ifthen(a.Important, ' !important');
end;

end.

