unit Pospolite.View.CSS.Binder;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.HTML.Document, Pospolite.View.CSS.StyleSheet;

type

  { TPLCSSSelectorBind }

  TPLCSSSelectorBind = packed class(specialize TPLList<TPLCSSDeclarations>);

  { TPLCSSSelectorBinder }

  TPLCSSSelectorBinder = packed class sealed
  public
    class procedure BindAllNodes(AMainStyles: TPLCSSStyleSheet; var ADocument: TPLHTMLDocument);
  end;

implementation

{ TPLCSSSelectorBinder }

class procedure TPLCSSSelectorBinder.BindAllNodes(
  AMainStyles: TPLCSSStyleSheet; var ADocument: TPLHTMLDocument);
begin

end;

end.

