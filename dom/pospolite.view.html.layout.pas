unit Pospolite.View.HTML.Layout;

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
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

type

  { TPLHTMLObjectLayouter }

  TPLHTMLObjectLayouter = packed class
  private
    FBody: TPLHTMLObject;
    procedure UpdateLayout;
  public
    constructor Create;
    destructor Destroy; override;

    property Body: TPLHTMLObject read FBody write FBody;
  end;

implementation

{ TPLHTMLObjectLayouter }

procedure TPLHTMLObjectLayouter.UpdateLayout;
begin
  if Assigned(FBody) then FBody.UpdateLayoutForAll;
end;

constructor TPLHTMLObjectLayouter.Create;
begin
  inherited Create;


end;

destructor TPLHTMLObjectLayouter.Destroy;
begin
  inherited Destroy;
end;

end.

