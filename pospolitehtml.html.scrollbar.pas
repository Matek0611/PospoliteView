unit PospoLiteHTML.HTML.Scrollbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, BGRABitmap, BGRABitmapTypes, Forms,
  PospoLiteHTML.CSS.Classes, LMessages, math, ExtCtrls, Dialogs, LCLType,
  LCLIntf, LCLProc, dateutils, PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Values;

type

  { TPLHTMLScrollbar }

  TPLHTMLScrollbar = class(TPLCSSCustomControl)
  private
    FHasCorner: boolean;
    FKind: TScrollBarKind;
    FLargeChange: SizeInt;
    FMax: SizeInt;
    FMin: SizeInt;
    FOnChange: TNotifyEvent;
    FPageSize: SizeInt;
    FPos: SizeInt;
    FSmallChange: SizeInt;
    FDeltaOfThumb: SizeInt;
    FMouseDown: boolean;
    FMouseDragOffset: SizeInt;
    FMouseDownOnUp,
    FMouseDownOnDown,
    FMouseDownOnThumb,
    FMouseDownOnPageUp,
    FMouseDownOnPageDown: boolean;
    FMousePos: TPoint;
    FTimer: TTimer;
    FRectMain: TRect;
    FRectArrUp: TRect;
    FRectArrDown: TRect;
    FRectThumb: TRect;
    FRectCorner: TRect;
    FRectPageUp: TRect;
    FRectPageDown: TRect;
    FScrollPosTo: TValueRelationship;
    procedure SetHasCorner(AValue: boolean);
    procedure SetKind(AValue: TScrollBarKind);
    procedure SetMax(AValue: SizeInt);
    procedure SetMin(AValue: SizeInt);
    procedure SetPageSize(AValue: SizeInt);
    procedure SetPos(AValue: SizeInt);
    procedure DoScrollBy(NDelta: SizeInt);
    procedure DoUpdateThumbRect;
    procedure DoUpdateCornerRect;
    procedure DoUpdatePosOnDrag(X, Y: SizeInt);
    function CoordToPos(X, Y: SizeInt): SizeInt;
    function PosToCoord(APos: SizeInt): SizeInt;
    function OptimalRectSize: SizeInt;
  protected
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer
      ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseLeave; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
      override;
    procedure TimerTimer(Sender: TObject);
    procedure {%H-}InvalidatePreferredSize; override;
  public
    property ScrollPosTo: TValueRelationship read FScrollPosTo;

    procedure ScrollMessage(var Msg: TLMScroll);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //procedure SetStyles(AValue: string); override;
    procedure ResetAllStyles;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property DoubleBuffered;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnResize;

    property HasCorner: boolean read FHasCorner write SetHasCorner;
    property Position: SizeInt read FPos write SetPos default 0;
    property Min: SizeInt read FMin write SetMin default 0;
    property Max: SizeInt read FMax write SetMax default 100;
    property SmallChange: SizeInt read FSmallChange write FSmallChange default 1;
    property LargeChange: SizeInt read FLargeChange write FLargeChange default 0;
    property PageSize: SizeInt read FPageSize write SetPageSize default 20;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
  end;

implementation

uses Types, CommCtrl, windows, PospoLiteHTML.CSS.Parser;

{ TPLHTMLScrollbar }

procedure TPLHTMLScrollbar.SetHasCorner(AValue: boolean);
begin
  if FHasCorner = AValue then exit;
  FHasCorner := AValue;

  InvalidatePreferredSize;
  Invalidate;
end;

procedure TPLHTMLScrollbar.SetKind(AValue: TScrollBarKind);
begin
  if FKind = AValue then exit;
  FKind := AValue;
  if AValue = sbVertical then Align := alRight else Align := alBottom;

  InvalidatePreferredSize;
  Invalidate;
end;

procedure TPLHTMLScrollbar.SetMax(AValue: SizeInt);
begin
  if FMax = AValue then exit;
  FMax := AValue;
  FPos := Math.Min(FPos, FMax);

  Invalidate;
end;

procedure TPLHTMLScrollbar.SetMin(AValue: SizeInt);
begin
  if FMin = AValue then exit;
  FMin := AValue;
  FPos:= Math.Max(FPos, FMin);

  Invalidate;
end;

procedure TPLHTMLScrollbar.SetPageSize(AValue: SizeInt);
begin
  if FPageSize = AValue then exit;
  FPageSize := AValue;

  Invalidate;
end;

procedure TPLHTMLScrollbar.SetPos(AValue: SizeInt);
begin
  AValue := Math.Min(AValue, FMax);
  AValue := Math.Max(AValue, FMin);

  FScrollPosTo := 0;

  if FPos = AValue then exit;

  if FPos < AValue then FScrollPosTo := -1 else FScrollPosTo := 1;

  FPos := AValue;

  if Assigned(FOnChange) then FOnChange(self);

  Repaint;
end;

procedure TPLHTMLScrollbar.DoScrollBy(NDelta: SizeInt);
var
  N: SizeInt;
begin
  N := FPos + NDelta;
  if NDelta > 0 then N := Math.Min(N, FMax - FPageSize);
  SetPos(N);
end;

procedure TPLHTMLScrollbar.DoUpdateThumbRect;
var
  R: TRect;
begin
  FRectThumb := TRect.Empty;
  FRectPageUp := TRect.Empty;
  FRectPageDown := TRect.Empty;

  if FKind = sbHorizontal then begin
    if FRectMain.Width < 5 then exit;

    R.Top := FRectMain.Top;
    R.Bottom := FRectMain.Bottom;
    R.Left := PosToCoord(FPos);
    R.Right := PosToCoord(FPos + FPageSize);

    FDeltaOfThumb := R.Right-R.Left - 2;

    R.Left := Math.Min(R.Left, FRectMain.Right - 2);
    R.Right := Math.Max(R.Right, R.Left + 2);
    R.Right := Math.Min(R.Right, FRectMain.Right);
  end else begin
    if FRectMain.Height < 5 then exit;

    R.Left := FRectMain.Left;
    R.Right := FRectMain.Right;
    R.Top := PosToCoord(FPos);
    R.Bottom := PosToCoord(FPos + FPageSize);

    FDeltaOfThumb := R.Bottom - R.Top - 2;

    R.Top := Math.Min(R.Top, FRectMain.Bottom - 2);
    R.Bottom := Math.Max(R.Bottom, R.Top + 2);
    R.Bottom := Math.Min(R.Bottom, FRectMain.Bottom);
  end;

  if FKind = sbVertical then InflateRect(R, -ScaleValue(1.5), 0) else InflateRect(R, 0, -ScaleValue(1.5));
  FRectThumb := R;

  if FKind = sbHorizontal then begin
    FRectPageUp := TRect.Create(FRectMain.Left, FRectMain.Top, FRectThumb.Left, FRectMain.Bottom);
    FRectPageDown := TRect.Create(FRectThumb.Right, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
  end else begin
    FRectPageUp := TRect.Create(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectThumb.Top);
    FRectPageDown := TRect.Create(FRectMain.Left, FRectThumb.Bottom, FRectMain.Right, FRectMain.Bottom);
  end;
end;

procedure TPLHTMLScrollbar.DoUpdateCornerRect;
var
  w, h, d: integer;
begin
  w := Width;
  h := Height;
  d := ifthen(FHasCorner, ifthen(FKind = sbVertical, w, h));
  FRectCorner := TRect.Empty;

  if FKind = sbHorizontal then
  begin
    if d > 0 then begin
      FRectCorner := TRect.Create(w - d, 0, w, h);
      Dec(FRectMain.Right, d);
    end else if d < 0 then begin
      FRectCorner := TRect.Create(0, 0, abs(d), h);
      Inc(FRectMain.Left, abs(d));
    end;
  end else begin
    if d > 0 then begin
      FRectCorner := TRect.Create(0, h - d, w, h);
      Dec(FRectMain.Bottom, d);
    end else if d < 0 then begin
      FRectCorner := TRect.Create(0, 0, w, abs(d));
      Inc(FRectMain.Top, abs(d));
    end;
  end;
end;

procedure TPLHTMLScrollbar.DoUpdatePosOnDrag(X, Y: SizeInt);
var
  N: SizeInt;
begin
  N := CoordToPos(X - FMouseDragOffset, Y - FMouseDragOffset);
  N := Math.Max(N, FMin);
  N := Math.Min(N, FMax - FPageSize);
  SetPos(N);
end;

function TPLHTMLScrollbar.CoordToPos(X, Y: SizeInt): SizeInt;
begin
  Result:= FMin + (ifthen(FKind = sbVertical, Y - FRectMain.Top, X - FRectMain.Left)) * (FMax - FMin) div OptimalRectSize;
end;

function TPLHTMLScrollbar.PosToCoord(APos: SizeInt): SizeInt;
begin
  Result := ifthen(FKind = sbVertical, FRectMain.Top, FRectMain.Left) +
    (APos - FMin) * OptimalRectSize div Math.Max(1, FMax - FMin);
end;

function TPLHTMLScrollbar.OptimalRectSize: SizeInt;
begin
  Result := ifthen(FKind = sbVertical, FRectMain.Height, FRectMain.Width) +
    ifthen(FDeltaOfThumb < 0, FDeltaOfThumb);
  if Result < 1 then Result := 1;
end;

procedure TPLHTMLScrollbar.ScrollMessage(var Msg: TLMScroll);
var
  Incr, FinalIncr, Count, CurrentTime, StartTime, ElapsedTime: SizeInt;

  function GetRealScrollPosition: Integer;
  var
    SI: Windows.TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(Parent.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
  begin
    if ScrollCode in [SB_LINEUP, SB_LINEDOWN, SB_PAGEUP, SB_PAGEDOWN] then
    begin
      case ScrollCode of
        SB_LINEUP, SB_LINEDOWN:
          begin
            Incr := FSmallChange div 4;
            FinalIncr := FSmallChange mod 4;
            Count := 4;
          end;
        SB_PAGEUP, SB_PAGEDOWN:
          begin
            Incr := FPageSize;
            FinalIncr := Incr mod 12;
            Incr := Incr div 12;
            Count := 12;
          end;
      else
        Count := 0;
        Incr := 0;
        FinalIncr := 0;
      end;
      CurrentTime := 0;
      while Count > 0 do
      begin
        StartTime := GetCurrentTime;
        ElapsedTime := StartTime - CurrentTime;
        if ElapsedTime < 10 then Sleep(10 - ElapsedTime);
        CurrentTime := StartTime;
        case ScrollCode of
          SB_LINEUP: SetPos(FPos - Incr);
          SB_LINEDOWN: SetPos(FPos + Incr);
          SB_PAGEUP: SetPos(FPos - Incr);
          SB_PAGEDOWN: SetPos(FPos + Incr);
        end;
        Parent.Update;
        Dec(Count);
      end;
      if FinalIncr > 0 then
      begin
        case ScrollCode of
          SB_LINEUP: SetPos(FPos - FinalIncr);
          SB_LINEDOWN: SetPos(FPos + FinalIncr);
          SB_PAGEUP: SetPos(FPos - FinalIncr);
          SB_PAGEDOWN: SetPos(FPos + FinalIncr);
        end;
      end;
    end else begin
      case ScrollCode of
        SB_LINEUP: SetPos(FPos - FSmallChange);
        SB_LINEDOWN: SetPos(FPos + FSmallChange);
        SB_PAGEUP: SetPos(FPos - FPageSize);
        SB_PAGEDOWN: SetPos(FPos + FPageSize);
        SB_THUMBPOSITION:
            if FMax-FMin > 32767 then
              SetPos(GetRealScrollPosition) else
              SetPos(Pos);
        SB_THUMBTRACK:
          if FMax-FMin > 32767 then
            SetPos(GetRealScrollPosition) else
            SetPos(Pos);
        SB_TOP: SetPos(FMin);
        SB_BOTTOM: SetPos(FMax);
        SB_ENDSCROLL: ;
      end;
    end;
  end;
end;

procedure TPLHTMLScrollbar.Paint;
var
  //w: TPLCSSSelector;
  //bg: TPLCSSBackground;
  s: SizeInt;
  pw: TPLCSSFloat;
  FBitmap: TBGRABitmap;
begin
  inherited Paint;

  FBitmap := TBGRABitmap.Create(Width, Height);
  try
    //FRectMain := ClientRect;
    //FRectArrUp := TRect.Empty;
    //FRectArrDown := TRect.Empty;
    //
    //// corner
    //DoUpdateCornerRect;
    //if FHasCorner then begin
    //  if PtInRect(FRectCorner, FMousePos) then begin
    //    if FMouseDown then w := FStyles['::-webkit-scrollbar-corner:active']
    //      else w := FStyles['::-webkit-scrollbar-corner:hover'];
    //  end else w := FStyles['::-webkit-scrollbar-corner'];
    //
    //  bg := TPLCSSBackground.Create(w);
    //  bg.Location := ExtractFilePath(FFilename);
    //  bg.Draw(FBitmap, FRectCorner, self);
    //end;
    //
    //if FStyles['::-webkit-scrollbar-button'].Properties['display'].Trim.ToLower <> 'none' then begin
    //  // arrows
    //  if FKind = sbVertical then begin
    //    s := Math.Min(FRectMain.Width, FRectMain.Height div 2);
    //    FRectArrUp := TRect.Create(FRectMain.Left, FRectMain.Top, FRectMain.Right, FRectMain.Top+s);
    //    FRectArrDown := TRect.Create(FRectMain.Left, FRectMain.Bottom-s, FRectMain.Right, FRectMain.Bottom);
    //    Inc(FRectMain.Top, s);
    //    Dec(FRectMain.Bottom, s);
    //  end else begin
    //    s := Math.Min(FRectMain.Height, FRectMain.Width div 2);
    //    FRectArrUp := TRect.Create(FRectMain.Left, FRectMain.Top, FRectMain.Left+s, FRectMain.Bottom);
    //    FRectArrDown := TRect.Create(FRectMain.Right-s, FRectMain.Top, FRectMain.Right, FRectMain.Bottom);
    //    Inc(FRectMain.Left, s);
    //    Dec(FRectMain.Right, s);
    //  end;
    //
    //  FBitmap.LineCap := TPenEndCap.pecSquare;
    //  FBitmap.PenStyle := psSolid;
    //  pw := CSSLengthToScreenF(TPLCSSLength.Create('1px'), nil, '');
    //
    //  //  góra
    //  if FPos = FMin then w := FStyles['::-webkit-scrollbar-button:disabled'] else
    //  if PtInRect(FRectArrUp, FMousePos) then begin
    //    if FMouseDown then w := FStyles['::-webkit-scrollbar-button:active']
    //      else w := FStyles['::-webkit-scrollbar-button:hover'];
    //  end else w := FStyles['::-webkit-scrollbar-button'];
    //
    //  bg := TPLCSSBackground.Create(w);
    //  bg.Location := ExtractFilePath(FFilename);
    //  bg.Draw(FBitmap, FRectArrUp, self);
    //
    //  if FKind = sbVertical then
    //    FBitmap.DrawPolyLineAntialias([
    //      PointF(FRectArrUp.Left+FRectArrUp.Width / 4, FRectArrUp.Bottom-FRectArrUp.Height / 2.5),
    //      PointF(FRectArrUp.Left+FRectArrUp.Width / 2, FRectArrUp.Top+FRectArrUp.Height / 3),
    //      PointF(FRectArrUp.Left+3*FRectArrUp.Width / 4, FRectArrUp.Bottom-FRectArrUp.Height / 2.5)
    //      ], BGRAColorFromCSS(TPLCSSColor.Create(w.Properties['color'])), pw, BGRAPixelTransparent)
    //  else
    //    FBitmap.DrawPolyLineAntialias([
    //      PointF(FRectArrUp.Right-FRectArrUp.Width / 2.5, FRectArrUp.Top+FRectArrUp.Height / 4),
    //      PointF(FRectArrUp.Left+FRectArrUp.Width / 3, FRectArrUp.Top+FRectArrUp.Height / 2),
    //      PointF(FRectArrUp.Right-FRectArrUp.Width / 2.5, FRectArrUp.Bottom-FRectArrUp.Height / 4)
    //      ], BGRAColorFromCSS(TPLCSSColor.Create(w.Properties['color'])), pw, BGRAPixelTransparent);
    //
    //  //  dół
    //  if FPos = FMax - FPageSize then w := FStyles['::-webkit-scrollbar-button:disabled'] else
    //  if PtInRect(FRectArrDown, FMousePos) then begin
    //    if FMouseDown then w := FStyles['::-webkit-scrollbar-button:active']
    //      else w := FStyles['::-webkit-scrollbar-button:hover'];
    //  end else w := FStyles['::-webkit-scrollbar-button'];
    //
    //  bg := TPLCSSBackground.Create(w);
    //  bg.Location := ExtractFilePath(FFilename);
    //  bg.Draw(FBitmap, FRectArrDown, self);
    //
    //  if FKind = sbVertical then
    //    FBitmap.DrawPolyLineAntialias([
    //      PointF(FRectArrDown.Left+FRectArrDown.Width / 4, FRectArrDown.Top+FRectArrDown.Height / 2.5),
    //      PointF(FRectArrDown.Left+FRectArrDown.Width / 2, FRectArrDown.Top+2*FRectArrDown.Height / 3),
    //      PointF(FRectArrDown.Left+3*FRectArrDown.Width / 4, FRectArrDown.Top+FRectArrDown.Height / 2.5)
    //      ], BGRAColorFromCSS(TPLCSSColor.Create(w.Properties['color'])), pw, BGRAPixelTransparent)
    //  else
    //    FBitmap.DrawPolyLineAntialias([
    //      PointF(FRectArrDown.Left+FRectArrDown.Width / 2.5, FRectArrDown.Top+FRectArrDown.Height / 4),
    //      PointF(FRectArrDown.Right-FRectArrDown.Width / 3, FRectArrDown.Top+FRectArrDown.Height / 2),
    //      PointF(FRectArrDown.Left+FRectArrDown.Width / 2.5, FRectArrDown.Bottom-FRectArrDown.Height / 4)
    //      ], BGRAColorFromCSS(TPLCSSColor.Create(w.Properties['color'])), pw, BGRAPixelTransparent);
    //end else begin
    //  FRectArrUp := TRect.Empty;
    //  FRectArrDown := TRect.Empty;
    //end;
    //
    //// whole bg
    //if PtInRect(FRectMain, FMousePos) then begin
    //  if FMouseDown then w := FStyles['::-webkit-scrollbar:active']
    //    else w := FStyles['::-webkit-scrollbar:hover'];
    //end else w := FStyles['::-webkit-scrollbar'];
    //
    //bg := TPLCSSBackground.Create(w);
    //bg.Location := ExtractFilePath(FFilename);
    //bg.Draw(FBitmap, FRectMain, self);
    //
    //// thumb
    //DoUpdateThumbRect;
    //if PtInRect(FRectThumb, FMousePos) then begin
    //  if FMouseDown then w := FStyles['::-webkit-scrollbar-thumb:active']
    //    else w := FStyles['::-webkit-scrollbar-thumb:hover'];
    //end else w := FStyles['::-webkit-scrollbar-thumb'];
    //
    //if FMax <> FPageSize then begin
    //  bg := TPLCSSBackground.Create(w);
    //  bg.Location := ExtractFilePath(FFilename);
    //  bg.Draw(FBitmap, FRectThumb, self);
    //end;

    FBitmap.Draw(Canvas, 0, 0, false);
  finally
    FBitmap.Free;
  end;
end;

procedure TPLHTMLScrollbar.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  w: string;
  l: TPLCSSLength;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  //FStyles['::self::'].Properties['width'] := FStyles['::-webkit-scrollbar'].Properties['width'];
  //FStyles['::self::'].Properties['height'] := FStyles['::-webkit-scrollbar'].Properties['height'];
  //
  //if FKind = sbVertical then begin
  //  w := FStyles['::-webkit-scrollbar'].Properties['width'];
  //  ProcessImportant(w);
  //  if IsInitValue(w) then l := TPLCSSLength.Create('17px')
  //    else l := TPLCSSLength.Create(w);
  //  PreferredWidth := CSSLengthToScreen(l, self, 'width');
  //end else begin
  //  w := FStyles['::-webkit-scrollbar'].Properties['height'];
  //  ProcessImportant(w);
  //  if IsInitValue(w) then l := TPLCSSLength.Create('17px')
  //    else l := TPLCSSLength.Create(w);
  //  PreferredHeight := CSSLengthToScreen(l, self, 'height');
  //end;
end;

procedure TPLHTMLScrollbar.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  FMousePos := TPoint.Create(X, Y);

  Invalidate;

  if FMouseDownOnThumb then DoUpdatePosOnDrag(X, Y);
end;

procedure TPLHTMLScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  ScrollValue: SizeInt;
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseDown := Button = mbLeft;
  FMouseDownOnThumb := PtInRect(FRectThumb, TPoint.Create(X, Y));
  FMouseDownOnUp := PtInRect(FRectArrUp, TPoint.Create(X, Y));
  FMouseDownOnDown := PtInRect(FRectArrDown, TPoint.Create(X, Y));
  FMouseDownOnPageUp := PtInRect(FRectPageUp, TPoint.Create(X, Y));
  FMouseDownOnPageDown := PtInRect(FRectPageDown, TPoint.Create(X, Y));

  Invalidate;

  if FKind = sbHorizontal then
    FMouseDragOffset := X - FRectThumb.Left
  else
    FMouseDragOffset := Y - FRectThumb.Top;

  if FMouseDown then begin
    if FMouseDownOnUp then begin
      FTimer.Interval := 8;
      DoScrollBy(-FSmallChange);
      FTimer.Enabled := true;
    end else if FMouseDownOnDown then begin
      FTimer.Interval := 8;
      DoScrollBy(FSmallChange);
      FTimer.Enabled := true;
    end else if FMouseDownOnPageUp or FMouseDownOnPageDown then
    begin
      FTimer.Interval := 100;

      if FLargeChange > 0 then
        ScrollValue := FLargeChange
      else
        ScrollValue := FPageSize;

      if FMouseDownOnPageUp then
        DoScrollBy(-ScrollValue)
      else
        DoScrollBy(ScrollValue);

      FTimer.Enabled := true;
    end;
  end;
end;

procedure TPLHTMLScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FMouseDown := false;
  FMouseDownOnThumb := false;
  FMouseDownOnUp := false;
  FMouseDownOnDown := false;

  FTimer.Enabled := false;

  Invalidate;
end;

procedure TPLHTMLScrollbar.MouseLeave;
begin
  inherited MouseLeave;

  FMousePos := TPoint.Create(-1, -1);
  Invalidate;
end;

function TPLHTMLScrollbar.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  DoScrollBy(-FSmallChange);
end;

function TPLHTMLScrollbar.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint
  ): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  DoScrollBy(FSmallChange);
end;

procedure TPLHTMLScrollbar.TimerTimer(Sender: TObject);
begin
  if FMouseDownOnDown and PtInRect(FRectArrDown, FMousePos) then
    DoScrollBy(FSmallChange)
  else
  if FMouseDownOnUp and PtInRect(FRectArrUp, FMousePos) then
    DoScrollBy(-FSmallChange)
  else
  if FMouseDownOnPageDown and PtInRect(FRectPageDown, FMousePos) then
    DoScrollBy(FPageSize)
  else
  if FMouseDownOnPageUp and PtInRect(FRectPageUp, FMousePos) then
    DoScrollBy(-FPageSize);
end;

procedure TPLHTMLScrollbar.InvalidatePreferredSize;
var
  w, h: integer;
begin
  inherited InvalidatePreferredSize;

  w := Width;
  h := Height;
  //if Assigned(FStyles) then CalculatePreferredSize(w, h, false);
  Width := w;
  Height := h;
end;

constructor TPLHTMLScrollbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHasCorner := false;
  FKind := sbVertical;
  Align := alRight;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 0;
  FPageSize := 20;
  FPos := 0;
  FMouseDown := false;
  FMousePos := TPoint.Create(-1, -1);
  FMouseDragOffset := 0;
  FOnChange := nil;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := false;
  FTimer.Interval := 16;
  FTimer.OnTimer := @TimerTimer;

  //FStyles['::-webkit-scrollbar'];
  //FStyles['::-webkit-scrollbar:hover'];
  //FStyles['::-webkit-scrollbar:active'];
  //FStyles['::-webkit-scrollbar-button'];
  //FStyles['::-webkit-scrollbar-button:hover'];
  //FStyles['::-webkit-scrollbar-button:active'];
  //FStyles['::-webkit-scrollbar-button:disabled'];
  //FStyles['::-webkit-scrollbar-thumb'];
  //FStyles['::-webkit-scrollbar-thumb:hover'];
  //FStyles['::-webkit-scrollbar-thumb:active'];
  //FStyles['::-webkit-scrollbar-track'];
  //FStyles['::-webkit-scrollbar-track:hover'];
  //FStyles['::-webkit-scrollbar-track:active'];
  //FStyles['::-webkit-scrollbar-track-piece'];
  //FStyles['::-webkit-scrollbar-track-piece:hover'];
  //FStyles['::-webkit-scrollbar-track-piece:active'];
  //FStyles['::-webkit-scrollbar-corner'];
  //FStyles['::-webkit-scrollbar-corner:hover'];
  //FStyles['::-webkit-scrollbar-corner:active'];
  //FStyles['::-webkit-resizer'];
  //FStyles['::-webkit-resizer:hover'];
  //FStyles['::-webkit-resizer:active'];

  ResetAllStyles;
  InvalidatePreferredSize;
end;

destructor TPLHTMLScrollbar.Destroy;
begin
  FTimer.Enabled := false;
  FreeAndNil(FTimer);

  inherited Destroy;
end;

procedure TPLHTMLScrollbar.ResetAllStyles;
begin
  //// whole bg
  //if IsInitValue(FStyles['::-webkit-scrollbar'].Properties['background-color']) then FStyles['::-webkit-scrollbar'].Properties['background-color'] := '#f1f1f1';
  //if IsInitValue(FStyles['::-webkit-scrollbar:hover'].Properties['background-color']) then FStyles['::-webkit-scrollbar:hover'].Properties['background-color'] := '#f1f1f1';
  //if IsInitValue(FStyles['::-webkit-scrollbar:active'].Properties['background-color']) then FStyles['::-webkit-scrollbar:active'].Properties['background-color'] := '#f1f1f1';
  //// thumb
  //if IsInitValue(FStyles['::-webkit-scrollbar-thumb'].Properties['background-color']) then FStyles['::-webkit-scrollbar-thumb'].Properties['background-color'] := '#c1c1c1';
  //if IsInitValue(FStyles['::-webkit-scrollbar-thumb:hover'].Properties['background-color']) then FStyles['::-webkit-scrollbar-thumb:hover'].Properties['background-color'] := '#a8a8a8';
  //if IsInitValue(FStyles['::-webkit-scrollbar-thumb:active'].Properties['background-color']) then FStyles['::-webkit-scrollbar-thumb:active'].Properties['background-color'] := '#787878';
  //// buttons
  //if IsInitValue(FStyles['::-webkit-scrollbar-button'].Properties['background-color']) then FStyles['::-webkit-scrollbar-button'].Properties['background-color'] := '#f1f1f1';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button'].Properties['color']) then FStyles['::-webkit-scrollbar-button'].Properties['color'] := '#505050';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button'].Properties['display']) then FStyles['::-webkit-scrollbar-button'].Properties['display'] := 'block';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:hover'].Properties['background-color']) then FStyles['::-webkit-scrollbar-button:hover'].Properties['background-color'] := '#d2d2d2';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:hover'].Properties['color']) then FStyles['::-webkit-scrollbar-button:hover'].Properties['color'] := '#505050';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:active'].Properties['background-color']) then FStyles['::-webkit-scrollbar-button:active'].Properties['background-color'] := '#787878';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:active'].Properties['color']) then FStyles['::-webkit-scrollbar-button:active'].Properties['color'] := 'white';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:disabled'].Properties['background-color']) then FStyles['::-webkit-scrollbar-button:disabled'].Properties['background-color'] := '#f1f1f1';
  //if IsInitValue(FStyles['::-webkit-scrollbar-button:disabled'].Properties['color']) then FStyles['::-webkit-scrollbar-button:disabled'].Properties['color'] := '#a3a3a3';
  //// corner
  //if IsInitValue(FStyles['::-webkit-scrollbar-corner'].Properties['background-color']) then FStyles['::-webkit-scrollbar-corner'].Properties['background-color'] := '#dcdcdc';
  //if IsInitValue(FStyles['::-webkit-scrollbar-corner:hover'].Properties['background-color']) then FStyles['::-webkit-scrollbar-corner:hover'].Properties['background-color'] := '#dcdcdc';
  //if IsInitValue(FStyles['::-webkit-scrollbar-corner:active'].Properties['background-color']) then FStyles['::-webkit-scrollbar-corner:active'].Properties['background-color'] := '#dcdcdc';
end;

end.

