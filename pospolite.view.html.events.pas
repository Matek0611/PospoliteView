unit Pospolite.View.HTML.Events;

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
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.Threads;

type

  { TPLHTMLEvent }

  TPLHTMLEvent = packed record
  strict private
    FEvent: TPLAsyncProc;
    FTarget: TPLHTMLObject;
    FType: TPLString;
  public
    constructor Create(AEvent: TPLAsyncProc; ATarget: TPLHTMLObject; AType: TPLString);

    procedure Call(const AArgs: array of const);

    property Target: TPLHTMLObject read FTarget write FTarget;
    property &Type: TPLString read FType write FType;
  end;

  { TPLEventListener }

  TPLHTMLEventListener = packed record
  strict private
    FEvent: TPLHTMLEvent;
  public
    constructor Create(AEvent: TPLHTMLEvent; ATarget: TPLHTMLObject; AType: TPLString);


  end;

implementation

{ TPLHTMLEvent }

constructor TPLHTMLEvent.Create(AEvent: TPLAsyncProc; ATarget: TPLHTMLObject;
  AType: TPLString);
begin
  FEvent := AEvent;
  FTarget := ATarget;
  FType := AType;
end;

procedure TPLHTMLEvent.Call(const AArgs: array of const);
begin

end;

end.

