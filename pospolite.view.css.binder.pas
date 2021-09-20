unit Pospolite.View.CSS.Binder;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
   - https://www.tutorialrepublic.com/css-reference/css3-properties.php
   - time is in ms
}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Controls, Pospolite.View.Basics, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Drawer;

type

  TPLCSSSimpleUnitValue = specialize TPLParameter<TPLFloat, TPLString>;

operator := (const AValue: TPLFloat) r: TPLCSSSimpleUnitValue;

type

  { TPLCSSSimpleUnit }

  TPLCSSSimpleUnit = packed record
  public
    Value: TPLCSSSimpleUnitValue;
    Calculated: TPLFloat;
  public
    constructor Create(AValue: TPLCSSSimpleUnitValue; ACalculated: TPLFloat = 0);

    class operator =(a, b: TPLCSSSimpleUnit) r: TPLBool;
    class operator :=(a: TPLCSSSimpleUnit) r: Variant;

    function IsAuto: TPLBool; inline;
    class function Auto: TPLCSSSimpleUnit; static;
  end;

  TPLCSSSimpleUnitFuncs = specialize TPLFuncs<TPLCSSSimpleUnit>;

  { TPLCSSSimpleUnitRect }

  TPLCSSSimpleUnitRect = record
  public
    Left, Right, Top, Bottom: TPLCSSSimpleUnit;
  public
    constructor Create(const ALeft, ARight, ATop, ABottom: TPLCSSSimpleUnit);
    constructor Create(const AWhole: TPLCSSSimpleUnit);
  end;

  { TPLCSSBindingProperties }

  TPLCSSBindingProperties = packed record
  public type
    TAlignContentType = (actCenter, actFlexStart, actFlexEnd, actSpaceBetween,
      actSpaceAround, actStretch);
    TAlignItemsType = (aitBaseline, aitCenter, aitFlexStart, aitFlexEnd, aitStretch);
    TAlignSelfType = (astAuto, astBaseline, astCenter, astFlexStart, astFlexEnd,
      astStretch);
    TAnimationDirectionType = (adtNormal, adtReverse, adtAlternate, adtAlternateReverse);
    TAnimationFillModeType = (afmtNone, afmtForwards, afmtBackwards, afmtBoth);
    TAnimationPlayStateType = (pstPaused, pstRunning);
    TTimingFunctionType = record
    public
      Name: TPLString;
      Args: array[0..3] of TPLFloat;
    end;
    TBackgroundAttachmentType = (batScroll, batFixed);
    TBoxModelType = (bmtBorderBox, bmtPaddingBox, bmtContentBox);
    TBackgroundRepeatType = (brtRepeat, brtRepeatX, brtRepeatY, brtNoRepeat);
    TClearType = (ctLeft, ctRight, ctAuto, ctBoth, ctNone);
    TCaptionSideType = (cstTop, cstBottom);
    TColumnFillType = (cftAuto, cftBalance);
    TFlexDirectionType = (fdtRow, fdtRowReverse, fdtColumn, fdtColumnReverse);
    TFlexWrapType = (fwtNowrap, fwtWrap, fwtWrapReverse);
    TFloatType = (ftLeft, ftRight, ftNone);
    TJustifyContentType = (jctFlexStart, jctFlexEnd, jctCenter, jctSpaceBetween,
      jctSpaceAround);
    TListStylePositionType = (lsptInside, lsptOutside);
    TListStyleType = (lstDisc, lstCircle, lstSquare, lstDecimal, lstDecimalLeadingZero,
      lstLowerRoman, lstUpperRoman, lstLowerGreek, lstLowerLatin, lstUpperLatin,
      lstArmenian, lstGeorgian, lstLowerAlpha, lstUpperAlpha, lstNone);
    TPageBreakType = (pbtAuto, pbtAlways, pbtAvoid, pbtLeft, pbtRight);
    TTextDecorationStyleType = (tdstSolid, tdstDouble, tdstDotted, tdstDashed, tdstWavy);
    TTextTransformType = (tttCapitalize, tttLowercase, tttNone, tttUppercase);
    TTransformFunctionType = record
      Name: TPLString;
      Args: array of TPLFloat;
    end;
    TSimpleSizeType = record
      Width, Height: TPLCSSSimpleUnit;
    end;
  public
    Align: record
      Content: TAlignContentType;
      Items: TAlignItemsType;
      Self: TAlignSelfType;
    end;
    Animation: record
      Delay: TPLInt;
      Direction: TAnimationDirectionType;
      Duration: TPLInt;
      FillMode: TAnimationFillModeType;
      IterationCount: TPLFloat; // infinity = TPLFloat.PositiveInfinity
      Name: TPLString;
      PlayState: TAnimationPlayStateType;
      TimingFunction: TTimingFunctionType;
    end;
    BackfaceVisibility: TPLBool;
    Background: record
      Attachment: TBackgroundAttachmentType;
      Clip: TBoxModelType;
      Color: TPLColor;
      Image: IPLDrawingBitmap;
      Origin: TBoxModelType;
      Position: array[0..1] of Variant;
      &Repeat: TBackgroundRepeatType;
      Size: array[0..1] of Variant;
    end;
    Border: record
      Calculated: TPLDrawingBorders;
      Units: array[0..1] of array[0..3] of TPLCSSSimpleUnit; // 0 - width, 1 - radius
      Spacing: TPLCSSSimpleUnit;
    end;
    Bottom: TPLCSSSimpleUnit;
    BoxShadow: Variant;
    BoxSizing: TBoxModelType;
    CaptionSide: TCaptionSideType;
    Clear: TClearType;
    Color: TPLColor;
    Column: record
      Count: TPLInt; // auto = -1
      Fill: TColumnFillType;
      Gap: TPLCSSSimpleUnit; // normal = 1em
      Rule: record
        Color: TPLColor;
        Style: TPLDrawingBorderStyle;
        Width: TPLCSSSimpleUnit;
      end;
      Span: TPLBool;
      Width: TPLCSSSimpleUnit;
    end;
    Content: TPLString;
    Counter: record
      Increment: array of Variant;
      Reset: Variant;
    end;
    Cursor: TCursor;
    Direction: TPLString;
    EmptyCells: TPLBool;
    Flex: record
      Basis: TPLCSSSimpleUnit;
      Direction: TFlexDirectionType;
      Grow: TPLInt;
      Shrink: TPLInt;
      Wrap: TFlexWrapType;
    end;
    Float: TFloatType;
    Font: record
      Family: array of TPLString;
      Size: TPLCSSSimpleUnit;
      Adjust: TPLCSSSimpleUnit; // Firefox supports it only, so no need for supporting it here
      Stretch: TPLDrawingFontStretch;
      Style: TPLDrawingFontStyle;
      SmallCaps: TPLBool; // = font-variant
      Weight: TPLDrawingFontWeight;
    end;
    Height: TPLCSSSimpleUnit;
    JustifyContent: TJustifyContentType;
    Left: TPLCSSSimpleUnit;
    LetterSpacing: TPLCSSSimpleUnit; // auto = normal
    LineHeight: TPLCSSSimpleUnit;
    ListStyle: record
      Image: IPLDrawingBitmap;
      Position: TListStylePositionType;
      Kind: TListStyleType; // = list-style-type
    end;
    Margin: TPLCSSSimpleUnitRect;
    Max: TSimpleSizeType; // max-width/height  none = auto
    Min: TSimpleSizeType; // min-width/height  -//-
    Opacity: TPLFloat;
    Order: TPLInt;
    Outline: record
      Color: TPLString; // TPLColor or 'invert'
      Offset: TPLCSSSimpleUnit;
      Style: TPLDrawingBorderStyle;
      Width: TPLCSSSimpleUnit;
    end;
    Overflow: array[0..1] of TPLString; // x, y
    Padding: TPLCSSSimpleUnitRect;
    PageBreak: record
      After: TPageBreakType;
      Before: TPageBreakType;
      Inside: TPageBreakType; // 'auto' and 'avoid' only
    end;
    Perspective: record
      Main: TPLCSSSimpleUnit;
      Origin: array[0..1] of Variant;
    end;
    Quotes: array of array[0..1] of TPLString;
    Resize: TPLString;
    Right: TPLCSSSimpleUnit;
    TabSize: TPLCSSSimpleUnit;
    TableLayout: TPLUInt;
    Text: record
      Align: TPLTextDirection; // left = tdLeft, right = tdRight, center = tdCenter, justify = tdFill
      AlignLast: TPLTextDirection;
      Decoration: record
        Color: TPLColor;
        Line: TPLDrawingFontDecorations;
        Style: TTextDecorationStyleType;
      end;
      Indent: TPLCSSSimpleUnit;
      Overflow: Variant;
      Shadow: Variant;
      Transform: TTextTransformType;
    end;
    Top: TPLCSSSimpleUnit;
    Transform: record
      Main: TTransformFunctionType;
      Origin: array[0..2] of TPLCSSSimpleUnit;
      Style: TPLUInt;
    end;
    Transition: record
      Delay: TPLInt;
      Duration: TPLInt;
      &Property: array of TPLString; // if empty = all
      TimingFunction: TTimingFunctionType;
    end;
    UnicodeBidi: TPLUInt; // no support
    VerticalAlign: Variant;
    WhiteSpace: TPLUInt; // no support
    Width: TPLCSSSimpleUnit;
    Word: record
      &Break: TPLUInt; // no support
      Spacing: TPLCSSSimpleUnit; // auto = normal
      Wrap: TPLBool;
    end;
  public
    class function GetDefault: TPLCSSBindingProperties; static;
  end;

  { TPLCSSStyleBind }

  TPLCSSStyleBind = packed record
  public
    Properties: array[TPLCSSElementState] of TPLCSSBindingProperties;
  public
    procedure RestoreDefault;
  end;

  TPLCSSStyleBinder = class;

  { TPLCSSStyleBinderThread }

  TPLCSSStyleBinderThread = class(TThread)
  private
    FEnabled: TPLBool;
    FBinder: TPLCSSStyleBinder;

    procedure SetEnabled(AValue: TPLBool);
  public
    constructor Create(ABinder: TPLCSSStyleBinder);

    procedure Execute; override;

    property Enabled: TPLBool read FEnabled write SetEnabled;
  end;

  { TPLCSSStyleBinder }

  TPLCSSStyleBinder = class sealed
  private
    FThread: TPLCSSStyleBinderThread;
    FDocument: Pointer;

    procedure InternalUpdate;
  public
    constructor Create(ADocument: Pointer);
    destructor Destroy; override;

    procedure UpdateBindings;
  end;

implementation

uses Pospolite.View.HTML.Document, Pospolite.View.HTML.Basics;

operator :=(const AValue: TPLFloat) r: TPLCSSSimpleUnitValue;
begin
  r := TPLCSSSimpleUnitValue.Create(AValue, '');
end;

{ TPLCSSSimpleUnit }

constructor TPLCSSSimpleUnit.Create(AValue: TPLCSSSimpleUnitValue;
  ACalculated: TPLFloat);
begin
  Value := AValue;
  Calculated := ACalculated;
end;

class operator TPLCSSSimpleUnit.=(a, b: TPLCSSSimpleUnit) r: TPLBool;
begin
  r := a.Calculated = b.Calculated;
end;

class operator TPLCSSSimpleUnit.:=(a: TPLCSSSimpleUnit) r: Variant;
begin
  r := TPLString(a.Value.Key) + a.Value.Value;
end;

function TPLCSSSimpleUnit.IsAuto: TPLBool;
begin
  Result := Value.Value = 'auto';
end;

class function TPLCSSSimpleUnit.Auto: TPLCSSSimpleUnit;
begin
  Result := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, 'auto'));
end;

{ TPLCSSSimpleUnitRect }

constructor TPLCSSSimpleUnitRect.Create(const ALeft, ARight, ATop,
  ABottom: TPLCSSSimpleUnit);
begin
  Left := ALeft;
  Right := ARight;
  Top := ATop;
  Bottom := ABottom;
end;

constructor TPLCSSSimpleUnitRect.Create(const AWhole: TPLCSSSimpleUnit);
begin
  Create(AWhole, AWhole, AWhole, AWhole);
end;

{ TPLCSSBindingProperties }

class function TPLCSSBindingProperties.GetDefault: TPLCSSBindingProperties;
begin
  Result := Default(TPLCSSBindingProperties);

  with Result do begin
    with Align do begin
      Content := actStretch;
      Items := aitStretch;
      Self := astAuto;
    end;
    with Animation do begin
      Delay := 0;
      Direction := adtNormal;
      Duration := 0;
      FillMode := afmtNone;
      IterationCount := 1;
      Name := '';
      PlayState := pstRunning;
      TimingFunction.Name := 'ease';
      TPLFloatFuncs.FillArray(TimingFunction.Args, 0);
    end;
    BackfaceVisibility := true;
    with Background do begin
      Attachment := batScroll;
      Clip := bmtBorderBox;
      Color := TPLColor.Transparent;
      Image := nil;
      Origin := bmtPaddingBox;
      TPLVariantFuncs.FillArray(Position, '0%');
      &Repeat := brtRepeat;
      TPLVariantFuncs.FillArray(Size, 'auto');
    end;
    with Border do begin
      Calculated := PLDrawingBordersDef;
      TPLCSSSimpleUnitFuncs.FillArray(Units[0], TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, 'px')));
      TPLCSSSimpleUnitFuncs.FillArray(Units[1], TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, 'px')));
      Spacing := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, 'px'));
    end;
    Bottom := TPLCSSSimpleUnit.Auto;
    BoxShadow := Null;
    BoxSizing := bmtContentBox;
    CaptionSide := cstTop;
    Clear := ctNone;
    Color := TPLColor.Black;
    with Column do begin
      Count := -1;
      Fill := cftBalance;
      Gap := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(1, 'em'));
      with Rule do begin
        Color := TPLColor.Black;
        Style := dbsNone;
        Width := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(3, 'px')); // medium
      end;
      Span := false;
      Width := TPLCSSSimpleUnit.Auto;
    end;
    Content := 'normal';
    with Counter do begin
      SetLength(Increment, 0);
      Reset := 'none';
    end;
    Cursor := crDefault;
    Direction := 'ltr';
    EmptyCells := true;
    with Flex do begin
      Basis := TPLCSSSimpleUnit.Auto;
      Direction := fdtRow;
      Grow := 0;
      Shrink := 1;
      Wrap := fwtNowrap;
    end;
    Float := ftNone;
    with Font do begin
      SetLength(Family, 1);
      Family[0] := 'sans-serif';
      Size := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(16, 'px'));
      Adjust := TPLCSSSimpleUnit.Auto;
      Stretch := dfstNormal;
      Style := dfsNormal;
      SmallCaps := false;
      Weight := dfwNormal;
    end;
    Height := TPLCSSSimpleUnit.Auto;
    JustifyContent := jctFlexStart;
    Left := TPLCSSSimpleUnit.Auto;
    LetterSpacing := TPLCSSSimpleUnit.Auto;
    LineHeight := TPLCSSSimpleUnit.Auto;
    with ListStyle do begin
      Image := nil;
      Position := lsptOutside;
      Kind := lstDisc;
    end;
    Margin := TPLCSSSimpleUnitRect.Create(TPLCSSSimpleUnit.Create(0));
    with Max do begin
      Width := TPLCSSSimpleUnit.Auto;
      Height := TPLCSSSimpleUnit.Auto;
    end;
    with Min do begin
      Width := TPLCSSSimpleUnit.Auto;
      Height := TPLCSSSimpleUnit.Auto;
    end;
    Opacity := 1;
    Order := 0;
    with Outline do begin
      Color := 'invert';
      Offset := TPLCSSSimpleUnit.Create(0);
      Style := dbsNone;
      Width := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(3, 'px'));
    end;
    TPLStringFuncs.FillArray(Overflow, 'auto');
    Padding := TPLCSSSimpleUnitRect.Create(TPLCSSSimpleUnit.Create(0));
    with PageBreak do begin
      After := pbtAuto;
      Before := pbtAuto;
      Inside := pbtAuto;
    end;
    with Perspective do begin
      Main := TPLCSSSimpleUnit.Auto;
      TPLVariantFuncs.FillArray(Origin, '0%');
    end;
    SetLength(Quotes, 0);
    Resize := 'none';
    Right := TPLCSSSimpleUnit.Auto;
    TableLayout := 0;
    TabSize := TPLCSSSimpleUnit.Create(8);
    with Text do begin
      Align := tdLeft;
      AlignLast := tdLeft;
      with Decoration do begin
        Color := TPLColor.Black;
        Line := [dfdUnderline];
        Style := tdstSolid;
      end;
      Indent := TPLCSSSimpleUnit.Create(0);
      Overflow := 'clip';
      Shadow := Null;
      Transform := tttNone;
    end;
    Top := TPLCSSSimpleUnit.Auto;
    with Transform do begin
      Main.Name := '';
      SetLength(Main.Args, 0);
      TPLCSSSimpleUnitFuncs.FillArray(Origin, TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, '%')));
      Style := 0;
    end;
    with Transition do begin
      Delay := 0;
      Duration := 0;
      SetLength(&Property, 0);
      TimingFunction.Name := 'ease';
      TPLFloatFuncs.FillArray(TimingFunction.Args, 0);
    end;
    UnicodeBidi := 0;
    VerticalAlign := 'baseline';
    WhiteSpace := 0;
    Width := TPLCSSSimpleUnit.Auto;
    with Word do begin
      &Break := 0;
      Spacing := TPLCSSSimpleUnit.Auto;
      Wrap := true;
    end;
  end;
end;

{ TPLCSSStyleBind }

procedure TPLCSSStyleBind.RestoreDefault;
var
  st: TPLCSSElementState;
begin
  for st in TPLCSSElementState do
    Properties[st] := TPLCSSBindingProperties.GetDefault;
end;

{ TPLCSSStyleBinderThread }

procedure TPLCSSStyleBinderThread.SetEnabled(AValue: TPLBool);
begin
  if FEnabled = AValue then exit;
  FEnabled := AValue;

  if FEnabled then Start
  else Suspended := true;
end;

constructor TPLCSSStyleBinderThread.Create(ABinder: TPLCSSStyleBinder);
begin
  inherited Create(true);

  FEnabled := false;
  FBinder := ABinder;
end;

procedure TPLCSSStyleBinderThread.Execute;
begin
  Synchronize(@FBinder.InternalUpdate);
  FEnabled := false;
end;

{ TPLCSSStyleBinder }

procedure TPLCSSStyleBinder.InternalUpdate;
var
  b: TPLHTMLBasicObject;
begin
  if not Assigned(FDocument) then exit;
  b := TPLHTMLDocument(FDocument).Body as TPLHTMLBasicObject;
  if not Assigned(b) then exit;

  b.RefreshStyles;
end;

constructor TPLCSSStyleBinder.Create(ADocument: Pointer);
begin
  inherited Create;

  FThread := TPLCSSStyleBinderThread.Create(self);
  FDocument := ADocument;
end;

destructor TPLCSSStyleBinder.Destroy;
begin
  FThread.Enabled := false;
  FThread.Terminate;
  FThread.Free;

  inherited Destroy;
end;

procedure TPLCSSStyleBinder.UpdateBindings;
begin
  FThread.Enabled := true;
end;

end.

