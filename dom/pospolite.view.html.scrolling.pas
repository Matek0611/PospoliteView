unit Pospolite.View.HTML.Scrolling;

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
  Classes, SysUtils, math, Pospolite.View.Basics, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Renderer;

type

  { TPLHTMLScrollingRealSize }

  TPLHTMLScrollingRealSize = packed record
  public
    Width, Height: TPLFloat;

    procedure Apply(const AWidth, AHeight: TPLFloat);
  end;

  { TPLHTMLScrollingInfo }

  TPLHTMLScrollingInfo = packed record
  public
    RealSize: TPLHTMLScrollingRealSize;
    Position: TPLPointF;

    class operator = (a, b: TPLHTMLScrollingInfo) r: TPLBool;
  end;

  { TPLHTMLScrolling }

  TPLHTMLScrolling = class(TObject)
  private
    FInfo: TPLHTMLScrollingInfo;
    FObject: TPLHTMLObject;
    FRenderer: TObject;
    procedure SetInfo(AValue: TPLHTMLScrollingInfo);
  public
    constructor Create(AObject: TPLHTMLObject);

    procedure UpdateScrolling;
    procedure Draw(ARenderer: TPLDrawingRenderer);

    property Info: TPLHTMLScrollingInfo read FInfo write SetInfo;
  end;

implementation

{ TPLHTMLScrollingRealSize }

procedure TPLHTMLScrollingRealSize.Apply(const AWidth, AHeight: TPLFloat);
begin
  Width := AWidth;
  Height := AHeight;
end;

{ TPLHTMLScrollingInfo }

class operator TPLHTMLScrollingInfo.=(a, b: TPLHTMLScrollingInfo) r: TPLBool;
begin
  r := (a.Position = b.Position) and (a.RealSize.Height = b.RealSize.Height)
    and (a.RealSize.Width = b.RealSize.Width);
end;

{ TPLHTMLScrolling }

procedure TPLHTMLScrolling.SetInfo(AValue: TPLHTMLScrollingInfo);
begin
  if FInfo = AValue then exit;

  FInfo := AValue;
  UpdateScrolling;
end;

constructor TPLHTMLScrolling.Create(AObject: TPLHTMLObject);
begin
  inherited Create;

  FObject := AObject;
end;

procedure TPLHTMLScrolling.UpdateScrolling;
var
  obj: TPLHTMLObject;
begin
  if not Assigned(FObject) then exit;

  Info.RealSize.Apply(FObject.GetWidth, FObject.GetHeight);

  for obj in FObject.Children do begin
    if (obj.PositionType = 'fixed') or ((FObject.PositionType = 'static') and
      (obj.PositionType = 'absolute')) then continue; // child obj is not in parent FObject

    Info.RealSize.Apply(
      max(Info.RealSize.Width, obj.GetLeft + obj.GetWidth),
      max(Info.RealSize.Height, obj.GetTop + obj.GetHeight)
    );
  end;
end;

procedure TPLHTMLScrolling.Draw(ARenderer: TPLDrawingRenderer);
var
  r: TPLDrawingRenderer absolute ARenderer;
begin

end;

end.

