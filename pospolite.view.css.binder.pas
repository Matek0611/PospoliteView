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
  end;

  TPLCSSSimpleUnitFuncs = specialize TPLFuncs<TPLCSSSimpleUnit>;

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
      Count: TPLInt;
      Fill: TColumnFillType;
      Gap: TPLCSSSimpleUnit; // normal = 1em
      Rule: record
        Color: TPLColor;
        Style: TPLDrawingBorderStyle;
        Width: TPLCSSSimpleUnit;
      end;
      Span: TPLBool;
      Width: TPLCSSSimpleUnit; // auto = -1
    end;
    Content: TPLString;
    Counter: record
      Increment: Variant;
      Reset: Variant;
    end;
    Cursor: TCursor;
    Direction: TPLString;
    EmptyCells: TPLBool;
    Flex: record
      Basis: TPLInt;
      Direction: TFlexDirectionType;
      Grow: TPLInt;
      Shrink: TPLInt;
      Wrap: TFlexWrapType;
    end;
    Float: TFloatType;
    Font: record
      Name: array of TPLString;
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
    LetterSpacing: TPLCSSSimpleUnit;
    LineHeight: TPLCSSSimpleUnit;
    ListStyle: record
      Image: IPLDrawingBitmap;
      Position: TListStylePositionType;
      Kind: TListStyleType; // = list-style-type
    end;
    Margin: TPLRectF;
    Max: TSimpleSizeType; // max-width/height
    Min: TSimpleSizeType; // min-width/height
    Opacity: TPLFloat;
    Order: TPLInt;
    Outline: record
      Color: TPLColor;
      Offset: TPLCSSSimpleUnit;
      Style: TPLDrawingBorderStyle;
      Width: TPLCSSSimpleUnit;
    end;
    Overflow: array[0..1] of TPLString; // x, y
    Padding: TPLRectF;
    PageBreak: record
      After: TPLString;
      Before: TPLString;
      Inside: TPLString;
    end;
    Perspective: record
      Main: TPLFloat;
      Origin: TPLPointF;
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
      Origin: TPLPointF;
      Style: TPLUInt;
    end;
    Transition: record
      Delay: TPLInt;
      Duration: TPLInt;
      &Property: TPLString;
      TimingFunction: TTimingFunctionType;
    end;
    UnicodeBidi: TPLUInt; // no support
    VerticalAlign: Variant;
    WhiteSpace: TPLUInt; // no support
    Width: TPLCSSSimpleUnit;
    Word: record
      &Break: TPLUInt; // no support
      Spacing: TPLCSSSimpleUnit;
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
    Bottom := TPLCSSSimpleUnit.Create(TPLCSSSimpleUnitValue.Create(0, 'auto'));
    BoxShadow := Null;
    BoxSizing := bmtContentBox;
    CaptionSide := cstTop;
    Clear := ctNone;
    Color := TPLColor.Black;
    with Column do begin

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

