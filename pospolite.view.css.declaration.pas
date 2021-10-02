unit Pospolite.View.CSS.Declaration;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
  ...
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, character, strutils, Dialogs, math;

// https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Values_and_units

type

  TPLCSSPropertyValuePartKind = (pvpkNumber, pvpkDimension, pvpkFunction,
    pvpkStringOrIdentifier);

  // Note: value in hex is a function named '#'

  { TPLCSSPropertyValuePart }

  TPLCSSPropertyValuePart = packed class
  private
    FKind: TPLCSSPropertyValuePartKind;
  public
    constructor Create(const AValue: TPLString); virtual;

    function SetValue(const AValue: TPLString): TPLBool; virtual;
    function AsString: TPLString; virtual;

    property Kind: TPLCSSPropertyValuePartKind read FKind;
  end;

  TPLCSSPropertyValuePartClass = class of TPLCSSPropertyValuePart;

  { TPLCSSPropertyValueParts }

  TPLCSSPropertyValueParts = class(specialize TPLObjectList<TPLCSSPropertyValuePart>)
  public
    function CountTypes(ATypes: array of TPLCSSPropertyValuePartClass; AFrom: TPLInt = 0): TPLInt;
  end;

  { TPLCSSPropertyValuePartNumber }

  TPLCSSPropertyValuePartNumber = packed class(TPLCSSPropertyValuePart)
  private
    FValue: TPLFloat;
  public
    function SetValue(const AValue: TPLString): TPLBool; override;
    function AsString: TPLString; override;

    property Value: TPLFloat read FValue;
  end;

  { TPLCSSPropertyValuePartDimension }

  TPLCSSPropertyValuePartDimension = packed class(TPLCSSPropertyValuePart)
  private
    FUnit: TPLString;
    FValue: TPLFloat;
  public
    function SetValue(const AValue: TPLString): TPLBool; override;
    function AsString: TPLString; override;

    class function IsDimensionValue(const AValue: TPLString): TPLBool;

    property Value: TPLFloat read FValue;
    property &Unit: TPLString read FUnit;
  end;

  { TPLCSSPropertyValuePartFunction }

  TPLCSSPropertyValuePartFunction = packed class(TPLCSSPropertyValuePart)
  private
    FArguments: TPLCSSPropertyValueParts;
    FName: TPLString;
  public
    constructor Create(const AValue: TPLString); override;
    destructor Destroy; override;

    function SetValue(const AValue: TPLString): TPLBool; override;
    function AsString: TPLString; override;

    property Name: TPLString read FName;
    property Arguments: TPLCSSPropertyValueParts read FArguments;
  end;

  { TPLCSSPropertyValuePartStringOrIdentifier }

  TPLCSSPropertyValuePartStringOrIdentifier = packed class(TPLCSSPropertyValuePart)
  private
    FQuoted: TPLBool;
    FValue: TPLString;
  public
    function SetValue(const AValue: TPLString): TPLBool; override;
    function AsString: TPLString; override;

    property Value: TPLString read FValue;
    property Quoted: TPLBool read FQuoted;
  end;

  TPLCSSPropertyValue = TPLCSSPropertyValueParts;
  TPLCSSProperty = class;

  { TPLCSSProperty }

  TPLCSSProperty = class(TInterfacedObject, specialize IPLCloneable<TPLCSSProperty>)
  private
    FImportant: TPLBool;
    FName: TPLString;
    FValue: TPLCSSPropertyValue;
    FRaw: TPLString;
    procedure SetName(AValue: TPLString);
  public
    constructor Create(const AName, AValue: TPLString);
    constructor Create(const AValue: TPLString);
    destructor Destroy; override;

    procedure SetProperty(const AValue: TPLString);
    procedure SetValue(AValue: TPLString);

    function AsString: TPLString;
    function Clone: TPLCSSProperty;

    property Name: TPLString read FName write SetName;
    property Value: TPLCSSPropertyValue read FValue write FValue;
    property Important: TPLBool read FImportant write FImportant;
  end;

  { IPLCSSDeclarations }

  IPLCSSDeclarations = interface
    procedure SetDeclarations(AValue: TPLString);
    function AsString: TPLString;

    procedure Add(AItem: TPLCSSProperty);
    function Find(AItem: TPLCSSProperty; AComparator: specialize TPLObjectListFindCompare<TPLCSSProperty>
      = nil): SizeInt;
    function Exists(AName: TPLString; out AProperty: TPLCSSProperty): TPLBool;
    procedure Delete(AName: TPLString);
  end;

  { TPLCSSDeclarations }

  TPLCSSDeclarations = class(specialize TPLObjectList<TPLCSSProperty>, IPLCSSDeclarations)
  private
    FSelector: TPLString;
    function Compare(a, b: T): TPLBool; inline;
    function CompareSort(a, b: T): TPLSign;
    function GetProperties(AName: TPLString): TPLCSSProperty;
    function BinarySearch(AName: TPLString; ALeft, ARight: SizeInt): SizeInt;
    function Search(AName: TPLString): SizeInt;
  public
    constructor Create(const AValue: TPLString = '');

    procedure SetDeclarations(AValue: TPLString);
    function AsString: TPLString;

    procedure Add(AItem: T); override;
    function Find(AItem: T; AComparator: specialize TPLObjectListFindCompare<T>
      = nil): SizeInt; override;
    function Exists(AName: TPLString; out AProperty: TPLCSSProperty): TPLBool;
    procedure Delete(AName: TPLString);
    function IsFor(const AObject: TPLHTMLObject): TPLBool;

    property Properties[AName: TPLString]: TPLCSSProperty read GetProperties;
    property Selector: TPLString read FSelector write FSelector;
  end;

  TPLCSSDeclarationsList = class(specialize TPLObjectList<TPLCSSDeclarations>);

  { TPLCSSPropertyParser }

  TPLCSSPropertyParser = packed class sealed
  public
    class procedure ParseFullProperty(AValue: TPLString; var AProperty: TPLCSSProperty); static;
    class procedure ParsePropertyValue(AValue: TPLString; var AParts: TPLCSSPropertyValueParts); static;
  end;

implementation

uses Pospolite.View.CSS.Basics, Pospolite.View.DOM.Document;

{ TPLCSSPropertyValuePart }

constructor TPLCSSPropertyValuePart.Create(const AValue: TPLString);
begin
  inherited Create;

  SetValue(AValue);
end;

function TPLCSSPropertyValuePart.SetValue(const AValue: TPLString): TPLBool;
begin
  Result := true;
end;

function TPLCSSPropertyValuePart.AsString: TPLString;
begin
  Result := '';
end;

{ TPLCSSPropertyValueParts }

function TPLCSSPropertyValueParts.CountTypes(
  ATypes: array of TPLCSSPropertyValuePartClass; AFrom: TPLInt): TPLInt;
var
  i, j: TPLInt;
begin
  Result := 0;

  for i := AFrom to Count-1 do begin
    for j := Low(ATypes) to High(ATypes) do begin
      if Item[i] is ATypes[j] then begin
        Inc(Result);
        break;
      end;
    end;
  end;
end;

{ TPLCSSPropertyValuePartNumber }

function TPLCSSPropertyValuePartNumber.SetValue(const AValue: TPLString
  ): TPLBool;
var
  v: TPLString;
begin
  FKind := pvpkNumber;
  FValue := 0;

  v := AValue.Trim;
  if v.StartsWith('-') or v.StartsWith('+') then v := v.Insert(2, '0') else v := '0' + v;
  FValue := v;
  Result := (FValue <> 0) or ((FValue = 0) and (v.EndsWith('0')));
end;

function TPLCSSPropertyValuePartNumber.AsString: TPLString;
begin
  Result := FValue;
end;

{ TPLCSSPropertyValuePartDimension }

function TPLCSSPropertyValuePartDimension.SetValue(const AValue: TPLString
  ): TPLBool;
var
  v, u: TPLString;
  i: TPLInt;
begin
  FKind := pvpkDimension;
  FValue := 0;
  FUnit := '';

  v := AValue.Trim;
  if (v.Length < 1) or not (v[1] in ['0'..'9', '+', '-']) then exit(false);

  if (v.Length > 1) and not (v[2] in ['0'..'9']) then begin
    if v.StartsWith('-') or v.StartsWith('+') then v := v.Insert(2, '0') else v := '0' + v;
  end;

  i := v.Length;
  u := '';
  while i > 0 do begin
    if TCharacter.IsNumber(v[i]) then break;

    u := v[i] + u;
    Dec(i);
  end;

  FValue := v.SubStr(1, i);
  FUnit := u.ToLower;
  Result := true;
end;

function TPLCSSPropertyValuePartDimension.AsString: TPLString;
begin
  Result := FValue;
  Result += FUnit;
end;

class function TPLCSSPropertyValuePartDimension.IsDimensionValue(
  const AValue: TPLString): TPLBool;
var
  d: TPLCSSPropertyValuePartDimension;
begin
  Result := false;

  d := TPLCSSPropertyValuePartDimension.Create('x');
  try
    Result := d.SetValue(AValue);
  finally
    d.Free;
  end;
end;

{ TPLCSSPropertyValuePartFunction }

constructor TPLCSSPropertyValuePartFunction.Create(const AValue: TPLString);
begin
  FArguments := TPLCSSPropertyValueParts.Create;

  inherited Create(AValue);
end;

destructor TPLCSSPropertyValuePartFunction.Destroy;
begin
  FArguments.Free;

  inherited Destroy;
end;

function TPLCSSPropertyValuePartFunction.SetValue(const AValue: TPLString
  ): TPLBool;
var
  v: TPLString;
  i: SizeInt;
begin
  v := AValue.Trim;
  i := v.Find('(');
  FName := v.SubStr(1, i - 1).Trim;
  v := v.SubStr(i + 1).Trim;
  v := v.SubStr(1, v.Length - 1);

  TPLCSSPropertyParser.ParsePropertyValue(v, FArguments);

  Result := FArguments.Count > 0;
end;

function TPLCSSPropertyValuePartFunction.AsString: TPLString;
var
  i: SizeInt;
begin
  if (FName = '#') and (FArguments.Count = 1) then begin
    Result := '#' + FArguments[0].AsString;
    exit;
  end;

  Result := FName + '(';

  for i := 0 to FArguments.Count-1 do begin
    Result += FArguments[i].AsString;
    if i < FArguments.Count-1 then Result += ', ';
  end;

  Result += ')';
end;

{ TPLCSSPropertyValuePartStringOrIdentifier }

function TPLCSSPropertyValuePartStringOrIdentifier.SetValue(const AValue: TPLString
  ): TPLBool;
begin
  FQuoted := false;
  Result := FValue <> AValue;
  FValue := AValue;
end;

function TPLCSSPropertyValuePartStringOrIdentifier.AsString: TPLString;
begin
  if FQuoted then Result := '''' + FValue + ''''
  else Result := FValue;
end;

{ TPLCSSProperty }

procedure TPLCSSProperty.SetName(AValue: TPLString);
begin
  if (FName = AValue) or AValue.Exists(' ') then exit;
  FName := AValue;
end;

constructor TPLCSSProperty.Create(const AName, AValue: TPLString);
begin
  inherited Create;

  FName := '';
  Name := AName;
  FValue := TPLCSSPropertyValue.Create;
  FRaw := AValue;
  TPLCSSPropertyParser.ParsePropertyValue(AValue, FValue);
end;

constructor TPLCSSProperty.Create(const AValue: TPLString);
begin
  inherited Create;

  FName := '';
  FImportant := false;
  FValue := TPLCSSPropertyValue.Create;
  SetProperty(AValue);
end;

destructor TPLCSSProperty.Destroy;
begin
  FValue.Free;

  inherited Destroy;
end;

procedure TPLCSSProperty.SetProperty(const AValue: TPLString);
begin
  FRaw := AValue;
  TPLCSSPropertyParser.ParseFullProperty(AValue, self);
end;

procedure TPLCSSProperty.SetValue(AValue: TPLString);
begin
  FRaw := AValue;
  RemoveCSSComments(AValue);
  TPLCSSPropertyParser.ParsePropertyValue(AValue, FValue);
end;

function TPLCSSProperty.AsString: TPLString;
var
  i: SizeInt;
begin
  Result := FName + ': ';

  for i := 0 to FValue.Count-1 do begin
    Result += FValue[i].AsString;
    if i < FValue.Count-1 then Result += ' '
    else if FImportant then Result += ' !important';
  end;
end;

function TPLCSSProperty.Clone: TPLCSSProperty;
begin
  Result := TPLCSSProperty.Create(FName, FRaw);
end;

{ TPLCSSDeclarations }

function TPLCSSDeclarations.Compare(a, b: T): TPLBool;
begin
  Result := a.FName = b.FName;
end;

function TPLCSSDeclarations.CompareSort(a, b: T): TPLSign;
begin
  if a.FName = b.FName then Result := 0
  else if a.FName > b.FName then Result := 1
  else Result := -1;
end;

function TPLCSSDeclarations.GetProperties(AName: TPLString): TPLCSSProperty;
var
  i: SizeInt;
begin
  i := Search(AName);
  if i > -1 then Result := Self[i] else
    Add(TPLCSSProperty.Create(AName, 'initial'));
end;

function TPLCSSDeclarations.BinarySearch(AName: TPLString; ALeft,
  ARight: SizeInt): SizeInt;
var
  m: SizeInt;
begin
  if ARight >= ALeft then begin
    m := ALeft + (ARight - ALeft) div 2;

    if Self[m].Name = AName then exit(m);
    if Self[m].Name > AName then exit(BinarySearch(AName, ALeft, m - 1));

    exit(BinarySearch(AName, m + 1, ARight));
  end;

  Result := -1;
end;

// https://en.wikipedia.org/wiki/Exponential_search
// O(log i)
function TPLCSSDeclarations.Search(AName: TPLString): SizeInt;
var
  bound: SizeInt = 1;
begin
  Result := -1;
  AName := AName.ToLower; // case in-sensitive

  if Count = 0 then exit;

  while (bound < Count) and (Self[bound].Name.ToLower < AName) do bound *= 2;

  Result := BinarySearch(AName, bound div 2, min(bound + 1, Count - 1));
end;

constructor TPLCSSDeclarations.Create(const AValue: TPLString);
begin
  inherited Create(true);

  FSelector := '';

  if not AValue.IsEmpty then SetDeclarations(AValue);
end;

procedure TPLCSSDeclarations.SetDeclarations(AValue: TPLString);
var
  is_string: TPLChar = #0;
  i, j, k: SizeInt;
  s: TPLString = '';
  p: TPLCSSProperty;

  function IsNotEnd: TPLBool;
  begin
    Result := i <= AValue.Length;
  end;

begin
  Clear;
  i := 1;
  j := i;
  RemoveCSSComments(AValue);
  AValue := AValue.Replace(#10, '').Replace(#13, '');

  while IsNotEnd do begin
    if is_string = #0 then begin
      if (AValue[i] = ';')  then begin
        s := AValue.SubStr(j, i - j + 1).Trim;

        if not s.IsEmpty then begin
          p := TPLCSSProperty.Create(s);
          k := Find(p, @Compare);
          if (k > -1) then begin
            if not (Self[k].Important and not p.Important) then begin
              Self[k].Free;
              Self[k] := p;
            end else p.Free;
          end else Add(p);
        end;

        while IsNotEnd and (AValue[i] = ';') do Inc(i);
        j := i;
      end else begin
        if AValue[i] in ['"', ''''] then is_string := AValue[i];
        Inc(i);
      end;
    end else begin
      if is_string = AValue[i] then is_string := #0;

      Inc(i);
    end;
  end;

  Sort(@CompareSort);
end;

function TPLCSSDeclarations.AsString: TPLString;
var
  i: SizeInt;
begin
  Result := '';

  for i := 0 to Count-1 do begin
    Result += Self[i].AsString + ';' + LineEnding;
  end;
end;

procedure TPLCSSDeclarations.Add(AItem: T);
begin
  inherited Add(AItem);

  Sort(@CompareSort);
end;

function TPLCSSDeclarations.Find(AItem: T; AComparator: specialize
  TPLObjectListFindCompare<T> = nil): SizeInt;
begin
  Result := Search(AItem.Name);
end;

function TPLCSSDeclarations.Exists(AName: TPLString; out
  AProperty: TPLCSSProperty): TPLBool;
var
  i: SizeInt;
begin
  i := Search(AName);
  Result := i > -1;

  if Result then AProperty := Self[i] else AProperty := nil;
end;

procedure TPLCSSDeclarations.Delete(AName: TPLString);
var
  p: TPLCSSProperty;
begin
  if Exists(AName, p) then Remove(p);
end;

function TPLCSSDeclarations.IsFor(const AObject: TPLHTMLObject): TPLBool;
begin
  if TPLString.IsNullOrEmpty(FSelector) then exit(true);
  Result := TPLHTMLDocumentQueries.isQuerySelectorFor(AObject, FSelector);
end;

{ TPLCSSPropertyParser }

class procedure TPLCSSPropertyParser.ParseFullProperty(AValue: TPLString;
  var AProperty: TPLCSSProperty);
var
  i: SizeInt;
begin
  RemoveCSSComments(AValue);
  i := AValue.Find(':');
  AProperty.Name := AValue.SubStr(1, i - 1);

  AValue := AValue.SubStr(i + 1);
  while AValue.EndsWith(';') and not AValue.IsEmpty do AValue := AValue.SubStr(1, AValue.Length - 1);
  if AValue.ToLower.EndsWith('!important') then begin
    AProperty.Important := true;
    AValue := AValue.SubStr(1, AValue.Length - 10);
  end;

  ParsePropertyValue(AValue, AProperty.FValue);
end;

{$goto on}
class procedure TPLCSSPropertyParser.ParsePropertyValue(AValue: TPLString;
  var AParts: TPLCSSPropertyValueParts);
var
  i: SizeInt = 1;
  j, k, to_close: SizeInt;
  str_open: TPLChar;
  pom: TPLString;
  f: TPLCSSPropertyValuePartFunction;

  function IsNotEnd: TPLBool;
  begin
    Result := i <= AValue.Length;
  end;

  procedure SkipSpaces;
  begin
    while IsNotEnd and (AValue[i].IsWhiteSpace or (AValue[i] = ',')) do Inc(i);
  end;

label
  varjump;

begin
  AValue := AValue.Trim;
  while AValue.EndsWith(';') and not AValue.IsEmpty do AValue := AValue.SubStr(1, AValue.Length - 1);

  while IsNotEnd do begin
    SkipSpaces;

    case AValue[i] of
      '#': begin
        Inc(i);
        j := i;

        while IsNotEnd and not (AValue[i].IsWhiteSpace or (AValue[i] = ',')) do Inc(i);

        f := TPLCSSPropertyValuePartFunction.Create('#()');
        f.Arguments.Add(TPLCSSPropertyValuePartStringOrIdentifier.Create(AValue.SubStr(j, i - j)));
        AParts.Add(f);
      end;
      '+', '-', '0'..'9', '.': begin
        if (i + 1 <= AValue.Length) and (((AValue[i] = '-') and (AValue[i+1] = '-')) or ((AValue[i] in ['-', '+']) and not (AValue[i+1] in ['.', '0'..'9']))) then goto varjump;

        j := i;

        while IsNotEnd and not (AValue[i].IsWhiteSpace or (AValue[i] = ',')) do Inc(i);

        if AValue[i].IsWhiteSpace or (AValue[i] = ',') then k := i - 1 else k := i;

        pom := AValue.SubStr(j, k - j + 1).Trim;
        if pom[pom.Length] in ['0'..'9'] then
          AParts.Add(TPLCSSPropertyValuePartNumber.Create(pom))
        else
          AParts.Add(TPLCSSPropertyValuePartDimension.Create(pom));
      end;
      '"', '''': begin
        j := i + 1;
        pom := AValue[i];

        Inc(i);
        while IsNotEnd and (AValue[i] <> pom[1]) do Inc(i);

        if AValue[i] = pom[1] then begin
          k := i - 1;
          Inc(i);
        end else k := i;
        pom := AValue.SubStr(j, k - j + 1);

        AParts.Add(TPLCSSPropertyValuePartStringOrIdentifier.Create(pom));
        TPLCSSPropertyValuePartStringOrIdentifier(AParts.Last).FQuoted := true;
      end;
      else begin
        varjump:

        j := i;

        while IsNotEnd do begin
          if AValue[i].IsWhiteSpace or (AValue[i] = '(') or (AValue[i] = ',') then break;
          Inc(i);
        end;
        if (AValue[i] = '(') then begin
          to_close := 1;
          str_open := #0;
          Inc(i);

          while IsNotEnd and ((to_close <> 0) or (str_open <> #0)) do begin
            if AValue[i] = '(' then Inc(to_close)
            else if AValue[i] = ')' then Dec(to_close)
            else if (AValue[i] in ['"', '''']) and (str_open = #0) then str_open := AValue[i]
            else if AValue[i] = str_open then str_open := #0;
            Inc(i);
          end;

          pom := AValue.SubStr(j, i - j + 1).Trim;
          if pom.EndsWith(',') then pom := pom.SubStr(1, pom.Length - 1);

          AParts.Add(TPLCSSPropertyValuePartFunction.Create(pom));
          Inc(i);
        end else begin
          k := i;
          while (k <= AValue.Length) and not (AValue[k].IsWhiteSpace) and (AValue[k] <> ',') do Inc(k);
          i := k;

          pom := AValue.SubStr(j, k - j + 1).Trim;
          if pom.EndsWith(',') then pom := pom.SubStr(1, pom.Length - 1);
          if not pom.IsEmpty then
            AParts.Add(TPLCSSPropertyValuePartStringOrIdentifier.Create(pom));
        end;
      end;
    end;
  end;
end;
{$goto off}

end.

