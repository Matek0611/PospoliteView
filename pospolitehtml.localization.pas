unit PospoLiteHTML.Localization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphUtil, Controls, strutils, math, contnrs,
  fgl {$IFDEF WINDOWS}, windows{$ENDIF}, LazUTF8;

type

  { TPLHTMLLocalizationLang }

  TPLHTMLLocalizationLang = class
  private
    FList: TFPStringHashTable;
    FName: string;
  protected
    FShortName: string;
    procedure PopulateList; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function IsSystemLang: boolean;

    property List: TFPStringHashTable read FList write FList;
    property Name: string read FName write FName;
  end;

  TPLHTMLLocalizationLangs = specialize TFPGObjectList<TPLHTMLLocalizationLang>;

  { TPLHTMLLocalizationLangEnglish }

  TPLHTMLLocalizationLangEnglish = class(TPLHTMLLocalizationLang)
  protected
    procedure PopulateList; override;
  end;

  { TPLHTMLLocalizationLangPolish }

  TPLHTMLLocalizationLangPolish = class(TPLHTMLLocalizationLang)
  protected
    procedure PopulateList; override;
  end;

  { TPLHTMLLocalizationManager }

  TPLHTMLLocalizationManager = class sealed
  private
    FActive: SizeInt;
    FList: TPLHTMLLocalizationLangs;
    function GetField(AName: string): string;
    procedure SetField(AName: string; AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DetectSystemLanguage;

    property Field[AName: string]: string read GetField write SetField; default;
  end;

var
  PLLocMng: TPLHTMLLocalizationManager;

implementation

{ TPLHTMLLocalizationLangPolish }

procedure TPLHTMLLocalizationLangPolish.PopulateList;
begin
  inherited PopulateList;
  FName := 'Polski';
  FShortName := 'pl';

  FList['SAVEDIALOG_TITLE'] := 'Zapisz plik';
  FList['SAVEDIALOG_FILTER'] := 'Wszystkie pliki|*.*';
end;

{ TPLHTMLLocalizationLangEnglish }

procedure TPLHTMLLocalizationLangEnglish.PopulateList;
begin
  inherited PopulateList;
  FName := 'English';
  FShortName := 'en';

  FList['SAVEDIALOG_TITLE'] := 'Save File';
  FList['SAVEDIALOG_FILTER'] := 'All Files|*.*';
end;

{ TPLHTMLLocalizationLang }

procedure TPLHTMLLocalizationLang.PopulateList;
begin
  //
end;

constructor TPLHTMLLocalizationLang.Create;
begin
  inherited Create;

  FList := TFPStringHashTable.Create;
  FName := '';
  FShortName := '';

  PopulateList;
end;

destructor TPLHTMLLocalizationLang.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

function TPLHTMLLocalizationLang.IsSystemLang: boolean;
var
  s: string;
begin
  LazGetShortLanguageID(s);
  Result := FShortName.ToLower = s.ToLower;
end;

{ TPLHTMLLocalizationManager }

function TPLHTMLLocalizationManager.GetField(AName: string): string;
begin
  Result := FList[FActive].List[AName];
end;

procedure TPLHTMLLocalizationManager.SetField(AName: string; AValue: string);
begin
  FList[FActive].List[AName] := AValue;
end;

constructor TPLHTMLLocalizationManager.Create;
begin
  inherited Create;

  FList := TPLHTMLLocalizationLangs.Create(true);
  FList.Add(TPLHTMLLocalizationLangEnglish.Create);
  FList.Add(TPLHTMLLocalizationLangPolish.Create);

  FActive := 0;

  DetectSystemLanguage;
end;

destructor TPLHTMLLocalizationManager.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

procedure TPLHTMLLocalizationManager.DetectSystemLanguage;
var
  i: SizeInt;
begin
  for i := 0 to FList.Count-1 do
    if FList[i].IsSystemLang then begin
      FActive := i;
      break;
    end;
end;

initialization
  PLLocMng := TPLHTMLLocalizationManager.Create;

finalization
  PLLocMng.Free;

end.

