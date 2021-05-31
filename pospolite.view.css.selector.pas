unit Pospolite.View.CSS.Selector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Declaration;

type

  TPLCSSSelectorList = class(specialize TPLList<TPLString>);

  { TPLCSSSelectorsGroup }

  TPLCSSSelectorsGroup = class
  private
    FDeclarations: TPLCSSDeclarations;
    FSelectors: TPLCSSSelectorList;
  public
    constructor Create(AValue: TPLString);
    destructor Destroy; override;

    procedure SetGroup(AValue: TPLString);

    property Selectors: TPLCSSSelectorList read FSelectors;
    property Declarations: TPLCSSDeclarations read FDeclarations;
  end;

implementation

{ TPLCSSSelectorsGroup }

constructor TPLCSSSelectorsGroup.Create(AValue: TPLString);
begin
  inherited Create;

  FSelectors := TPLCSSSelectorList.Create;
  FDeclarations := TPLCSSDeclarations.Create;
end;

destructor TPLCSSSelectorsGroup.Destroy;
begin
  FDeclarations.Free;
  FSelectors.Free;

  inherited Destroy;
end;

procedure TPLCSSSelectorsGroup.SetGroup(AValue: TPLString);
begin

end;

end.

