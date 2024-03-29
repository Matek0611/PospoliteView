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
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Document;

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

  TPLHTMLObjectLayoutManager = class;

  { TPLHTMLObjectLayoutThread }

  TPLHTMLObjectLayoutThread = class(TThread)
  private
    FManager: TPLHTMLObjectLayoutManager;
    FChange: TPLBool;
  public
    constructor Create(AManager: TPLHTMLObjectLayoutManager);

    procedure Execute; override;

    procedure UpdateLayout;
    procedure Annihilate;
  end;

  { TPLHTMLObjectLayoutManager }

  TPLHTMLObjectLayoutManager = class
  private
    FThread: TPLHTMLObjectLayoutThread;
    FDocument: TPLHTMLDocument;
  public
    constructor Create(const ADocument: TPLHTMLDocument);
    destructor Destroy; override;

    procedure StartLayouting;
    procedure StopLayouting;

    procedure Change;
    function IsWorking: TPLBool; inline;
  end;

implementation

{ TPLHTMLObjectLayouter }

procedure TPLHTMLObjectLayouter.UpdateLayout;
begin
  if Assigned(FBody) then FBody.UpdateLayout;
end;

constructor TPLHTMLObjectLayouter.Create;
begin
  inherited Create;

  //
end;

destructor TPLHTMLObjectLayouter.Destroy;
begin
  //

  inherited Destroy;
end;

{ TPLHTMLObjectLayoutThread }

constructor TPLHTMLObjectLayoutThread.Create(AManager: TPLHTMLObjectLayoutManager);
begin
  inherited Create(true);

  FManager := AManager;
end;

procedure TPLHTMLObjectLayoutThread.Execute;
var
  delay: Cardinal = 500;
begin
  FChange := true;

  while not Terminated and not Suspended do begin
    if FChange then begin
      FChange := false;
      Synchronize(@UpdateLayout);
    end;

    Sleep(delay);
  end;
end;

procedure TPLHTMLObjectLayoutThread.UpdateLayout;
var
  lt: TPLHTMLObjectLayouter;
begin
  lt := TPLHTMLObjectLayouter.Create;
  try
    lt.Body := FManager.FDocument.Body;
    lt.UpdateLayout;
  finally
    lt.Free;
  end;
end;

procedure TPLHTMLObjectLayoutThread.Annihilate;
begin
  Suspended := true;
  Free;
end;

{ TPLHTMLObjectLayoutManager }

constructor TPLHTMLObjectLayoutManager.Create(const ADocument: TPLHTMLDocument);
begin
  inherited Create;

  FThread := TPLHTMLObjectLayoutThread.Create(self);
  FDocument := ADocument;
end;

destructor TPLHTMLObjectLayoutManager.Destroy;
begin
  FThread.Annihilate;

  inherited Destroy;
end;

procedure TPLHTMLObjectLayoutManager.StartLayouting;
begin
  FThread.Start;
end;

procedure TPLHTMLObjectLayoutManager.StopLayouting;
begin
  FThread.Annihilate;
  FThread := TPLHTMLObjectLayoutThread.Create(self);
end;

procedure TPLHTMLObjectLayoutManager.Change;
begin
  FThread.FChange := true;
end;

function TPLHTMLObjectLayoutManager.IsWorking: TPLBool;
begin
  Result := Assigned(FThread) and not FThread.Suspended;
end;

end.

