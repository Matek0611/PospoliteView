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
      Size: Variant;
    end;
    Borders: record
      Main: TPLDrawingBorders;
      Spacing: TPLFloat;
    end;
    Bottom: TPLFloat;
    BoxShadow: Variant;
    BoxSizing: TBoxModelType;
    CaptionSide: TCaptionSideType;
    Clear: TClearType;
    Color: TPLColor;
    Column: record

    end;
    Content: TPLString;
    Counter: record

    end;
    Cursor: TCursor;
    Direction: TPLString;
    EmptyCells: TPLBool;

  end;

  { TPLCSSStyleBind }

  TPLCSSStyleBind = packed record
  public
    Properties: array[TPLCSSElementState] of TPLCSSBindingProperties;
    Current: TPLCSSBindingProperties;
  end;

implementation


end.

