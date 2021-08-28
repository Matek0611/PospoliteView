unit PospoLiteHTML.HTML.Scrolling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, math, Forms,
  LCLType, LCLProc, LCLIntf, strutils, PospoLiteHTML.CSS.Basics,
  PospoLiteHTML.CSS.Classes, PospoLiteHTML.HTML.Scrollbar,
  PospoLiteHTML.CSS.Values;

type

  { TPLHTMLScrollingControl }

  TPLHTMLScrollingControl = class(TPLCSSCustomControl)
  private
    FHScrollBar: TPLHTMLScrollbar;
    FVScrollBar: TPLHTMLScrollbar;
    FOverflow: string;
    FScrollPos: TPLRealSize;
    function GetScrollBehavior: string;
    procedure SetOverflow(AValue: string);
    procedure OnVScrollChange(Sender: TObject);
    procedure OnHScrollChange(Sender: TObject);
  protected
    function CalculateRealSize: TPLRealSize;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    function GetOverflow: TStringArray;
    procedure Resize; override;
    function GetClientScrollOffset: TPoint; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateAllStyles; override;
    procedure UpdateScrollBars;

    procedure ScrollBy(DeltaX, DeltaY: Integer); override;
    procedure ScrollTo(AX: SizeInt = 0; AY: SizeInt = 0);

    property ScrollBehavior: string read GetScrollBehavior;
    property Overflow: string read FOverflow write SetOverflow;

    property HScrollbar: TPLHTMLScrollbar read FHScrollBar;
    property VScrollbar: TPLHTMLScrollbar read FVScrollBar;
  end;

implementation

{ TPLHTMLScrollingControl }

procedure TPLHTMLScrollingControl.SetOverflow(AValue: string);
var
  arr: TStringArray;
begin
  if FOverflow = AValue then exit;

  AValue := AValue.ToLower.Trim;
  arr := AValue.Split(' ');
  case Length(arr) of
    1: FOverflow := AValue + ' ' + AValue;
    2: FOverflow := AValue;
  end;
end;

function TPLHTMLScrollingControl.GetScrollBehavior: string;
begin
  //Result := Styles['::self::'].Properties['scroll-behavior'].ToLower.Trim;
  //if not AnsiMatchStr(Result, ['auto', 'smooth']) then Result := 'auto';
end;

procedure TPLHTMLScrollingControl.OnVScrollChange(Sender: TObject);
begin
  FScrollPos.Height := FVScrollBar.Position;

  Resize;
  Invalidate;
end;

procedure TPLHTMLScrollingControl.OnHScrollChange(Sender: TObject);
begin
  FScrollPos.Width := FHScrollBar.Position;

  Resize;
  Invalidate;
end;

function TPLHTMLScrollingControl.CalculateRealSize: TPLRealSize;
var
  i: integer;
  s: string;
  sv, sh: boolean;
  arr: TStringArray;
begin
  Result.Width := 0;
  Result.Height := 0;

  for i := 0 to ControlCount-1 do begin
    if (Controls[i] is TPLHTMLScrollbar) or not (Controls[i] is TPLCSSCustomControl) then continue;
    //s := TPLCSSCustomControl(Controls[i]).Styles['::self::'].Properties['position'].ToLower.Trim;
    ProcessImportant(s);
    if AnsiMatchStr(s, ['fixed', 'absolute']) then continue;

    Result.Width := Max(Result.Width, TPLCSSCustomControl(Controls[i]).BasePos.Left + Controls[i].Width);
    Result.Height := Max(Result.Height, TPLCSSCustomControl(Controls[i]).BasePos.Top + Controls[i].Height);
  end;

  Result.Width := Max(Result.Width, Width);
  Result.Height := Max(Result.Height, Height);

  arr := GetOverflow;
  case arr[0] of
    'scroll': sv := true;
    'auto': sv := Result.Height > Height;
    else sv := false;
  end;
  case arr[1] of
    'scroll': sh := true;
    'auto': sh := Result.Width > Width;
    else sh := false;
  end;
  if sv then begin
    if sh then begin
      Result.Width := Result.Width + FVScrollBar.Width;
      Result.Height := Result.Height + FHScrollBar.Height;
    end else begin
      //
    end;
  end else begin
    if sh then begin
      //
    end;
  end;
end;

procedure TPLHTMLScrollingControl.UpdateScrollBars;
var
  arr: TStringArray;
  rs: TPLRealSize;
begin
  arr := GetOverflow;
  rs := CalculateRealSize;

  FVScrollBar.BringToFront;
  FHScrollBar.BringToFront;

  FVScrollBar.Min := 0;
  FVScrollBar.Max := rs.Height;
  FVScrollBar.PageSize := Height;
  FVScrollBar.Position := FScrollPos.Height;
  FScrollPos.Height := FVScrollBar.Position;
  case arr[0] of
    'auto': FVScrollBar.Visible := rs.Height > Height;
    'hidden': FVScrollBar.Visible := false;
    'scroll': FVScrollBar.Visible := true;
    else FVScrollBar.Visible := false;
  end;

  FHScrollBar.Min := 0;
  FHScrollBar.Max := rs.Width;
  FHScrollBar.PageSize := Width;
  FHScrollBar.Position := FScrollPos.Width;
  FScrollPos.Width := FHScrollBar.Position;
  case arr[1] of
    'auto': FHScrollBar.Visible := rs.Width > Width;
    'hidden': FHScrollBar.Visible := false;
    'scroll': FHScrollBar.Visible := true;
    else FHScrollBar.Visible := false;
  end;

  FHScrollBar.HasCorner := FVScrollBar.Visible and FHScrollBar.Visible;

  if not FVScrollBar.Visible then FScrollPos.Height := 0;
  if not FHScrollBar.Visible then FScrollPos.Width := 0;

  FVScrollBar.Invalidate;
  FHScrollBar.Invalidate;
end;

procedure TPLHTMLScrollingControl.WMSize(var Message: TLMSize);
begin
  UpdateScrollBars;
end;

procedure TPLHTMLScrollingControl.WMHScroll(var Message: TLMHScroll);
begin
  if (Message.ScrollBar = 0) and FHScrollBar.Visible then
    FHScrollBar.ScrollMessage(Message) else inherited;
end;

procedure TPLHTMLScrollingControl.WMVScroll(var Message: TLMVScroll);
begin
  if (Message.ScrollBar = 0) and FVScrollBar.Visible then
    FVScrollBar.ScrollMessage(Message) else inherited;
end;

function TPLHTMLScrollingControl.GetOverflow: TStringArray;
begin
  Result := FOverflow.Split(' ');
end;

procedure TPLHTMLScrollingControl.Resize;
begin
  inherited Resize;

  FScrollPos.Width := Min(FScrollPos.Width, FHScrollBar.Max - FHScrollBar.PageSize);
  FScrollPos.Height := Min(FScrollPos.Height, FVScrollBar.Max - FVScrollBar.PageSize);

  UpdateScrollBars;

  ScrollBy(FScrollPos.Width, FScrollPos.Height);
end;

function TPLHTMLScrollingControl.GetClientScrollOffset: TPoint;
begin
  if Assigned(FHScrollBar) and Assigned(FVScrollBar) then
    Result := TPoint.Create(FHScrollBar.Position, FVScrollBar.Position)
  else
    Result := TPoint.Create(0, 0);
end;

function TPLHTMLScrollingControl.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);

  if FVScrollBar.Visible then begin
    FVScrollBar.Position := FVScrollBar.Position - WheelDelta;
  end else if FHScrollBar.Visible then begin
    FHScrollBar.Position := FHScrollBar.Position - WheelDelta;
  end;

  Resize;
end;

constructor TPLHTMLScrollingControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csAcceptsControls, csClickEvents,
    csDoubleClicks, csAutoSizeKeepChildLeft, csAutoSizeKeepChildTop];

  FHScrollBar := TPLHTMLScrollbar.Create(self);
  FHScrollBar.Visible := false;
  FHScrollBar.Kind := sbHorizontal;
  FHScrollBar.Parent := self;
  FHScrollBar.OnChange := @OnHScrollChange;
  //FHScrollBar.Browser := self;
  FVScrollBar := TPLHTMLScrollbar.Create(self);
  FVScrollBar.Visible := false;
  FVScrollBar.Kind := sbVertical;
  FVScrollBar.Parent := self;
  FVScrollBar.OnChange := @OnVScrollChange;
  //FVScrollBar.Browser := self;
             //  x    y
  Overflow := 'auto auto';

  FScrollPos.Width := 0;
  FScrollPos.Height := 0;
end;

destructor TPLHTMLScrollingControl.Destroy;
begin
  FHScrollBar.Free;
  FVScrollBar.Free;

  inherited Destroy;
end;

procedure TPLHTMLScrollingControl.UpdateAllStyles;
begin
  inherited UpdateAllStyles;

  Overflow := Style('overflow');
  //Styles['::self::'].Properties['overflow'] := Overflow;
end;

procedure TPLHTMLScrollingControl.ScrollBy(DeltaX, DeltaY: Integer);
var
  dx, dy, i, pl, pt: SizeInt;
  s: string;
begin
  dx := DeltaX;
  dy := DeltaY;

  for i := 0 to ControlCount-1 do begin
    if (Controls[i] is TPLHTMLScrollbar) or not (Controls[i] is TPLCSSCustomControl) then continue;
    //s := TPLCSSCustomControl(Controls[i]).Styles['::self::'].Properties['position'].ToLower.Trim;
    ProcessImportant(s);
    if AnsiMatchStr(s, ['fixed', 'absolute']) then continue;

    if s = 'sticky' then begin
      //s := TPLCSSCustomControl(Controls[i]).Styles['::self::'].Properties['left'].ToLower.Trim;
      ProcessImportant(s);
      if IsInitValue(s) then s := '0px';
      pl := CSSLengthToScreen(PLCSSLength(s), self, 'left');
      //s := TPLCSSCustomControl(Controls[i]).Styles['::self::'].Properties['top'].ToLower.Trim;
      ProcessImportant(s);
      if IsInitValue(s) then s := '0px';
      pt := CSSLengthToScreen(PLCSSLength(s), self, 'top');
      TPLCSSCustomControl(Controls[i]).Left := Max(pl, TPLCSSCustomControl(Controls[i]).BasePos.Left - dx);
      TPLCSSCustomControl(Controls[i]).Top := Max(pt, TPLCSSCustomControl(Controls[i]).BasePos.Top - dy);
    end else begin
      TPLCSSCustomControl(Controls[i]).Left := TPLCSSCustomControl(Controls[i]).BasePos.Left - dx;
      TPLCSSCustomControl(Controls[i]).Top := TPLCSSCustomControl(Controls[i]).BasePos.Top - dy;
    end;
  end;
end;

procedure TPLHTMLScrollingControl.ScrollTo(AX: SizeInt; AY: SizeInt);
begin
  if GetScrollBehavior = 'auto' then begin
    FScrollPos.Height := FVScrollBar.Position;
    FScrollPos.Width := FHScrollBar.Position;

    Resize;
    Invalidate;
  end else begin

  end;
end;

end.

