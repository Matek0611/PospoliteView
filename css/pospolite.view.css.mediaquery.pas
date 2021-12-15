unit Pospolite.View.CSS.MediaQuery;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
    custom variables in media queries are not supported
    or = comma
}

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$goto on}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Declaration,
  Pospolite.View.DOM.Screen, Pospolite.View.DOM.Window, LCLType, LCLProc,
  LCLIntf, Printers, OSPrinters, Graphics, Forms, strutils;

type
  // - Media Queries Level 4 and some 5 - //
  // https://www.w3.org/TR/mediaqueries-4/
  // https://developer.mozilla.org/en-US/docs/Web/CSS/@media

  // actual values

  { TPLCSSMediaQueriesEnvironment }

  TPLCSSMediaQueriesEnvironment = packed record
  public
    function IsTouchDevicePresent: TPLBool;
    function IsDeviceUsedAsATablet: TPLBool;

    function FeatureAnyHover: TPLStringArray;
    function FeatureAnyPointer: TPLStringArray;
    function FeatureAspectRatio: TPLFloat;
    function FeatureColor: TPLInt;
    function FeatureColorGamut: TPLString;
    function FeatureColorIndex: TPLUInt;
    function FeatureDeviceAspectRatio: TPLFloat;
    function FeatureDeviceHeight: TPLInt;
    function FeatureDeviceWidth: TPLInt;
    function FeatureDisplayMode: TPLString;
    function FeatureGrid: TPLBool;
    function FeatureHeight: TPLInt;
    function FeatureHover: TPLBool;
    function FeatureMonochrome: TPLInt;
    function FeatureOrientation: TPLString;
    function FeaturePointer: TPLString;
    function FeaturePrefersColorScheme: TPLString;
    function FeaturePrefersReducedMotion: TPLString;
    function FeatureResolution: TPLFloat; // in dpi
    function FeatureScan: TPLString;
    function FeatureUpdate: TPLString;
    function FeatureWidth: TPLInt;
  public
    Screen: TPLDOMScreen;

    Hook: TPLDOMWindow;
    DocumentBody: TPLHTMLObject;
    Viewport: record
      Width, Height: TPLInt;
    end;
    PreferredColorScheme: TPLString;
    PreferredReducedMotion: TPLBool;
    UsePrinter: TPLBool;

    constructor Create(AVWidth, AVHeight: TPLInt);
  end;

  TPLCSSMediaExpressionBase = class;

  { TPLCSSMediaExpressionBase }

  TPLCSSMediaExpressionBase = class(TInterfacedObject, specialize IPLCloneable<TPLCSSMediaExpressionBase>)
  protected
    FFeature: TPLString;
  public
    function IsCorrect: TPLBool; virtual;
    function Clone: TPLCSSMediaExpressionBase; virtual;
    function Evaluate(const {%H-}AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool; virtual;
    function AsString: TPLString; virtual;

    property Feature: TPLString read FFeature;
  end;

  { TPLCSSMediaExpression }

  TPLCSSMediaExpression = class(TPLCSSMediaExpressionBase)
  private
    FValue: TPLCSSPropertyValuePart;
  public
    constructor Create(const AFeature: TPLString);
    constructor Create(const AFeature: TPLString; const AValue: TPLCSSPropertyValuePart);
    destructor Destroy; override;

    function IsCorrect: TPLBool; override;
    function HasValue: TPLBool; inline;
    function Clone: TPLCSSMediaExpressionBase; override;
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
      override;
    function AsString: TPLString; override;

    property Value: TPLCSSPropertyValuePart read FValue;
  end;

  TPLCSSMediaExpressionRangeOperator = (meroEqual, meroLess, meroLessEqual,
    meroGreater, meroGreaterEqual);

  { TPLCSSMediaExpressionRange }
  // https://www.w3.org/TR/mediaqueries-4/#mq-range-context
  TPLCSSMediaExpressionRange = class(TPLCSSMediaExpressionBase)
  private
    FOperatorFirst: TPLCSSMediaExpressionRangeOperator;
    FOperatorSecond: TPLCSSMediaExpressionRangeOperator;
    FValueFirst: TPLCSSPropertyValuePart;
    FValueSecond: TPLCSSPropertyValuePart;
  public
    constructor Create(const AFeature: TPLString; const AOperator: TPLCSSMediaExpressionRangeOperator;
      const AValue: TPLCSSPropertyValuePart);
    constructor Create( const AFeature: TPLString; const AOperator1, AOperator2: TPLCSSMediaExpressionRangeOperator;
      const AValue1, AValue2: TPLCSSPropertyValuePart);
    destructor Destroy; override;

    function IsCorrect: TPLBool; override;
    function HasOneSide: TPLBool; inline;
    function HasTwoSides: TPLBool; inline;
    function Clone: TPLCSSMediaExpressionBase; override;
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
      override;
    function AsString: TPLString; override;

    property ValueFirst: TPLCSSPropertyValuePart read FValueFirst;
    property ValueSecond: TPLCSSPropertyValuePart read FValueSecond;
    property OperatorFirst: TPLCSSMediaExpressionRangeOperator read FOperatorFirst;
    property OperatorSecond: TPLCSSMediaExpressionRangeOperator read FOperatorSecond;
  end;

  { TPLCSSMediaExpressions }

  TPLCSSMediaExpressions = class(specialize TPLObjectList<TPLCSSMediaExpressionBase>);

  TPLCSSMediaQualifier = (mqOnly, mqNot, mqNone);

  { TPLCSSMediaQuery }

  TPLCSSMediaQuery = class(TObject)
  private
    FExpressions: TPLCSSMediaExpressions;
    FMediaType: TPLString;
    FQualifier: TPLCSSMediaQualifier;
  public
    constructor Create;
    destructor Destroy; override;

    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
    function AsString: TPLString;

    property Qualifier: TPLCSSMediaQualifier read FQualifier write FQualifier;
    property MediaType: TPLString read FMediaType write FMediaType;
    property Expressions: TPLCSSMediaExpressions read FExpressions;
  end;

  { TPLCSSMediaQueries }

  TPLCSSMediaQueries = class(specialize TPLObjectList<TPLCSSMediaQuery>)
  public
    function Evaluate(const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
    function AsString: TPLString;
  end;

  { TPLCSSMediaQueriesList }

  TPLCSSMediaQueriesList = class(specialize TPLObjectList<TPLCSSMediaQueries>);

  { TPLCSSMediaQueryParser }

  TPLCSSMediaQueryParser = packed class sealed
  public
    class procedure ParseMediaQueries(ASource: TPLString; var AMediaQueries: TPLCSSMediaQueries); static;
  end;

implementation

{$ifdef windows}
uses
  windows, Win32Proc;

type
  TAR_STATE = (
    AR_ENABLED        = $0,
    AR_DISABLED       = $1,
    AR_SUPPRESSED     = $2,
    AR_REMOTESESSION  = $4,
    AR_MULTIMON       = $8,
    AR_NOSENSOR       = $10,
    AR_NOT_SUPPORTED  = $20,
    AR_DOCKED         = $40,
    AR_LAPTOP         = $80
  );
  //PAR_STATE = ^TAR_STATE;

  POWER_PLATFORM_ROLE = (
    PlatformRoleUnspecified        = 0,
    PlatformRoleDesktop            = 1,
    PlatformRoleMobile             = 2,
    PlatformRoleWorkstation        = 3,
    PlatformRoleEnterpriseServer   = 4,
    PlatformRoleSOHOServer         = 5,
    PlatformRoleAppliancePC        = 6,
    PlatformRolePerformanceServer  = 7,
    PlatformRoleSlate              = 8,
    PlatformRoleMaximum
  );

const
  //POWER_PLATFORM_ROLE_V1 = ULONG($00000001);
  POWER_PLATFORM_ROLE_V2 = ULONG($00000002);

function GetAutoRotationState(out pState: TAR_STATE): LongBool; stdcall; external user32;
function PowerDeterminePlatformRoleEx(Version: ULONG): POWER_PLATFORM_ROLE; stdcall; external 'PowrProf.dll';

{$endif}

operator := (a: TPLCSSMediaExpressionRangeOperator) r: TPLString;
begin
  case a of
    meroEqual: r := '=';
    meroGreater: r := '>';
    meroGreaterEqual: r := '>=';
    meroLess: r := '<';
    meroLessEqual: r := '<=';
  end;
end;

{ TPLCSSMediaQueriesEnvironment }

function TPLCSSMediaQueriesEnvironment.IsTouchDevicePresent: TPLBool;
const
  NID_READY = $00000080;
  NID_INTEGRATED_TOUCH = $00000001;
  NID_EXTERNAL_TOUCH = $00000002;
var
  w: TPLInt;
begin
{$ifdef windows}
  w := GetSystemMetrics(SM_DIGITIZER);
  Result := (w and NID_READY <> 0) and ((w and NID_INTEGRATED_TOUCH <> 0) or (w and NID_EXTERNAL_TOUCH <> 0));
{$else}
  Result := false;
{$endif}
end;

function TPLCSSMediaQueriesEnvironment.IsDeviceUsedAsATablet: TPLBool;
const
  SM_SYSTEMDOCKED = $2004;
  SM_CONVERTIBLESLATEMODE = $2003;
var
  {$ifdef windows}
  ts: TAR_STATE = TAR_STATE.AR_ENABLED;
  r: POWER_PLATFORM_ROLE;
  {$endif}
  ist: TPLBool = false;
begin
  Result := false;

{$ifdef windows}
  if (WindowsVersion < wv8) or (GetSystemMetrics(SM_MAXIMUMTOUCHES) = 0)
    or (GetSystemMetrics(SM_SYSTEMDOCKED) <> 0) then exit;

  if GetAutoRotationState(ts) and (ord(ts) and (ord(AR_NOT_SUPPORTED) or
    ord(AR_LAPTOP) or ord(AR_NOSENSOR)) <> 0) then exit;

  r := PowerDeterminePlatformRoleEx(POWER_PLATFORM_ROLE_V2);
  if r in [PlatformRoleMobile, PlatformRoleSlate] then
    ist := not GetSystemMetrics(SM_CONVERTIBLESLATEMODE);

  Result := ist;
{$endif}
end;

function TPLCSSMediaQueriesEnvironment.FeatureAnyHover: TPLStringArray;
begin
  if UsePrinter and Assigned(Printer) then exit(TPLStringFuncs.NewArray(['none']));

  if IsDeviceUsedAsATablet then exit(TPLStringFuncs.NewArray(['none']));

  if GetSystemMetrics(SM_MOUSEPRESENT) <> 0 then
    exit(TPLStringFuncs.NewArray(['hover']));

  Result := TPLStringFuncs.NewArray(['none']);
end;

function TPLCSSMediaQueriesEnvironment.FeatureAnyPointer: TPLStringArray;
var
  itdp: TPLBool;
begin
  if UsePrinter and Assigned(Printer) then exit(TPLStringFuncs.NewArray(['none']));

  if IsDeviceUsedAsATablet then exit(TPLStringFuncs.NewArray(['coarse']));

  itdp := IsTouchDevicePresent;
  if (GetSystemMetrics(SM_MOUSEPRESENT) = 0) and not itdp then
    exit(TPLStringFuncs.NewArray(['none']));

  Result := TPLStringFuncs.NewArray(['fine']);
  if itdp then Result += TPLStringFuncs.NewArray(['coarse']);
end;

function TPLCSSMediaQueriesEnvironment.FeatureAspectRatio: TPLFloat;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageWidth / Printer.PageHeight
  else
    Result := Viewport.Width / Viewport.Height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureColor: TPLInt;
var
  dc: HDC;
  hw: HWND = 0;
begin
  if FeatureMonochrome > 0 then Result := 0
  else begin
    {$ifdef windows}
    if UsePrinter and Assigned(Printer) then
      hw := TWinPrinter(Printer).Handle;
    {$endif}
    dc := GetDC(hw);
    Result := GetDeviceCaps(dc, BITSPIXEL);
    ReleaseDC(hw, dc);
  end;
end;

function TPLCSSMediaQueriesEnvironment.FeatureColorGamut: TPLString;
begin
  Result := 'srgb';
end;

function TPLCSSMediaQueriesEnvironment.FeatureColorIndex: TPLUInt;
begin
  if FeatureMonochrome > 0 then Result := 0
  else begin
    Result := QWord(2) << (FeatureColor - 1);
  end;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceAspectRatio: TPLFloat;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageWidth / Printer.PageHeight
  else
    Result := Screen.width / Screen.height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceHeight: TPLInt;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageHeight
  else
    Result := Screen.height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDeviceWidth: TPLInt;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageWidth
  else
    Result := Screen.width;
end;

function TPLCSSMediaQueriesEnvironment.FeatureDisplayMode: TPLString;
begin
  if Assigned(Hook.Hook) then begin
    if Hook.Hook.WindowState = wsFullScreen then
      Result := 'fullscreen'
    else case Hook.Display of
      wdStandalone: Result := 'standalone';
      wdMinimalUI: Result := 'minimal-ui';
      wdBrowser: Result := 'browser';
    end;
  end else Result := 'standalone';
end;

function TPLCSSMediaQueriesEnvironment.FeatureGrid: TPLBool;
var
  dc: HDC;
begin
  if UsePrinter and Assigned(Printer) then exit(false);

  dc := GetDC(0);
  Result := (FeatureMonochrome > 0) and ((GetSystemMetrics(SM_SLOWMACHINE) <> 0)
    or (GetDeviceCaps(dc, NUMFONTS) < 2));
  ReleaseDC(0, dc);
end;

function TPLCSSMediaQueriesEnvironment.FeatureHeight: TPLInt;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageHeight
  else
    Result := Viewport.Height;
end;

function TPLCSSMediaQueriesEnvironment.FeatureHover: TPLBool;
begin
  Result := 'hover' in FeatureAnyHover;
end;

function TPLCSSMediaQueriesEnvironment.FeatureMonochrome: TPLInt;
var
  dc: HDC;
begin
  if UsePrinter and Assigned(Printer) then begin
    dc := GetDC(TWinPrinter(Printer).Handle);
    if GetDeviceCaps(dc, BITSPIXEL) > 2 then
      Result := GetDeviceCaps(dc, BITSPIXEL)
    else
      Result := 0;
    ReleaseDC(TWinPrinter(Printer).Handle, dc);
  end else begin
    if Screen.colorDepth <= 2 then Result := Screen.pixelDepth
    else Result := 0;
  end;
end;

function TPLCSSMediaQueriesEnvironment.FeatureOrientation: TPLString;
begin
  if UsePrinter and Assigned(Printer) then begin
    case Printer.Orientation of
      poLandscape, poReverseLandscape: Result := 'landscape';
      poPortrait, poReversePortrait: Result := 'portrait';
    end;
  end else begin
    if Viewport.Height >= Viewport.Width then Result := 'portrait'
    else Result := 'landscape';
  end;
end;

function TPLCSSMediaQueriesEnvironment.FeaturePointer: TPLString;
var
  ap: TPLStringArray;
begin
  ap := FeatureAnyPointer;

  if 'fine' in ap then Result := 'fine' else
    if 'coarse' in ap then Result := 'coarse' else
      Result := 'none';
end;

function TPLCSSMediaQueriesEnvironment.FeaturePrefersColorScheme: TPLString;
begin
  Result := PreferredColorScheme;
end;

function TPLCSSMediaQueriesEnvironment.FeaturePrefersReducedMotion: TPLString;
begin
  if PreferredReducedMotion or (UsePrinter and Assigned(Printer)) then
    Result := 'reduce'
  else
    Result := 'no-preference';
end;

function TPLCSSMediaQueriesEnvironment.FeatureResolution: TPLFloat;
begin
  if UsePrinter and Assigned(Printer) then
    Result := min(Printer.XDPI, Printer.YDPI)
  else
    Result := Screen.devicePPI;
end;

function TPLCSSMediaQueriesEnvironment.FeatureScan: TPLString;
begin
  Result := 'progressive';
end;

function TPLCSSMediaQueriesEnvironment.FeatureUpdate: TPLString;
begin
  if UsePrinter and Assigned(Printer) then
    Result := 'none'
  else if GetSystemMetrics(SM_SLOWMACHINE) <> 0 then
    Result := 'slow'
  else
    Result := 'fast';
end;

function TPLCSSMediaQueriesEnvironment.FeatureWidth: TPLInt;
begin
  if UsePrinter and Assigned(Printer) then
    Result := Printer.PageWidth
  else
    Result := Viewport.Width;
end;

constructor TPLCSSMediaQueriesEnvironment.Create(AVWidth, AVHeight: TPLInt);
begin
  Viewport.Width := AVWidth;
  Viewport.Height := AVHeight;
  PreferredColorScheme := 'light';
  PreferredReducedMotion := false;
  UsePrinter := false;
  Hook := TPLDOMWindow.Create(nil);
  DocumentBody := nil;
end;

{ TPLCSSMediaExpressionBase }

function TPLCSSMediaExpressionBase.IsCorrect: TPLBool;
begin
  Result := not TPLString.IsNullOrEmpty(FFeature);
end;

function TPLCSSMediaExpressionBase.Clone: TPLCSSMediaExpressionBase;
begin
  Result := TPLCSSMediaExpressionBase.Create;
end;

function TPLCSSMediaExpressionBase.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
begin
  if not IsCorrect then exit(false);
  Result := true;
  // ...
end;

function TPLCSSMediaExpressionBase.AsString: TPLString;
begin
  Result := '';
end;

{ TPLCSSMediaExpression }

constructor TPLCSSMediaExpression.Create(const AFeature: TPLString);
begin
  inherited Create;

  FFeature := AFeature.ToLower;
  FValue := nil;
end;

constructor TPLCSSMediaExpression.Create(const AFeature: TPLString;
  const AValue: TPLCSSPropertyValuePart);
begin
  inherited Create;

  FFeature := AFeature.ToLower;
  FValue := AValue;
end;

destructor TPLCSSMediaExpression.Destroy;
begin
  if HasValue then FreeAndNil(FValue);

  inherited Destroy;
end;

function TPLCSSMediaExpression.IsCorrect: TPLBool;
begin
  Result := inherited IsCorrect;
end;

function TPLCSSMediaExpression.HasValue: TPLBool;
begin
  Result := Assigned(FValue);
end;

function TPLCSSMediaExpression.Clone: TPLCSSMediaExpressionBase;
begin
  Result := TPLCSSMediaExpression.Create(FFeature, FValue);
end;

function TPLCSSMediaExpression.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
var
  vf: TPLFloat;
begin
  Result := inherited Evaluate(AEnvironment);
  if not Result then exit;

  Result := false;

  case FFeature of
    'any-hover': begin
      if not HasValue then
        Result := 'hover' in AEnvironment.FeatureAnyHover
      else if FValue is TPLCSSPropertyValuePartStringOrIdentifier then
        Result := TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value in AEnvironment.FeatureAnyHover;
    end;
    'any-pointer': begin
      if not HasValue then
        Result := not ('none' in AEnvironment.FeatureAnyPointer)
      else if FValue is TPLCSSPropertyValuePartStringOrIdentifier then
        Result := TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value in AEnvironment.FeatureAnyPointer;
    end;
    'aspect-ratio', 'min-aspect-ratio', 'max-aspect-ratio':
    if HasValue and (FValue is TPLCSSPropertyValuePartFunction) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.Count = 2) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.First is TPLCSSPropertyValuePartNumber) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.Last is TPLCSSPropertyValuePartNumber) then begin

      vf := TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValue).Arguments.First).Value /
        TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValue).Arguments.Last).Value;
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureAspectRatio >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureAspectRatio <= vf
      else Result := AEnvironment.FeatureAspectRatio = vf;
    end;
    'color': begin
      if not HasValue then
        Result := AEnvironment.FeatureColor > 0
      else if FValue is TPLCSSPropertyValuePartNumber then
        Result := TPLCSSPropertyValuePartNumber(FValue).Value = AEnvironment.FeatureColor;
    end;
    'min-color', 'max-color': if HasValue and (FValue is TPLCSSPropertyValuePartNumber) then begin
      if FFeature.StartsWith('min') then
        Result := AEnvironment.FeatureColor >= TPLCSSPropertyValuePartNumber(FValue).Value
      else
        Result := AEnvironment.FeatureColor <= TPLCSSPropertyValuePartNumber(FValue).Value;
    end;
    'color-gamut': if HasValue and (FValue is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      Result := AEnvironment.FeatureColorGamut = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
    'color-index': begin
      if not HasValue then
        Result := AEnvironment.FeatureColorIndex > 0
      else if FValue is TPLCSSPropertyValuePartNumber then
        Result := TPLCSSPropertyValuePartNumber(FValue).Value = AEnvironment.FeatureColorIndex;
    end;
    'min-color-index', 'max-color-index': if HasValue and (FValue is TPLCSSPropertyValuePartNumber) then begin
      if FFeature.StartsWith('min') then
        Result := AEnvironment.FeatureColorIndex >= TPLCSSPropertyValuePartNumber(FValue).Value
      else
        Result := AEnvironment.FeatureColorIndex <= TPLCSSPropertyValuePartNumber(FValue).Value;
    end;
    'device-aspect-ratio', 'min-device-aspect-ratio', 'max-device-aspect-ratio':
    if HasValue and (FValue is TPLCSSPropertyValuePartFunction) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.Count = 2) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.First is TPLCSSPropertyValuePartNumber) and
      (TPLCSSPropertyValuePartFunction(FValue).Arguments.Last is TPLCSSPropertyValuePartNumber) then begin

      vf := TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValue).Arguments.First).Value /
        TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValue).Arguments.Last).Value;
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureDeviceAspectRatio >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureDeviceAspectRatio <= vf
      else Result := AEnvironment.FeatureDeviceAspectRatio = vf;
    end;
    'device-height', 'min-device-height', 'max-device-height':
    if HasValue and (FValue is TPLCSSPropertyValuePartDimension) then begin
      vf := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValue).Value, TPLCSSPropertyValuePartDimension(FValue).&Unit, AEnvironment.DocumentBody);
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureDeviceHeight >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureDeviceHeight <= vf
      else Result := AEnvironment.FeatureDeviceHeight = vf;
    end;
    'device-width', 'min-device-width', 'max-device-width':
    if HasValue and (FValue is TPLCSSPropertyValuePartDimension) then begin
      vf := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValue).Value, TPLCSSPropertyValuePartDimension(FValue).&Unit, AEnvironment.DocumentBody);
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureDeviceWidth >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureDeviceWidth <= vf
      else Result := AEnvironment.FeatureDeviceWidth = vf;
    end;
    'height', 'min-height', 'max-height':
    if HasValue and (FValue is TPLCSSPropertyValuePartDimension) then begin
      vf := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValue).Value, TPLCSSPropertyValuePartDimension(FValue).&Unit, AEnvironment.DocumentBody);
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureHeight >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureHeight <= vf
      else Result := AEnvironment.FeatureHeight = vf;
    end;
    'width', 'min-width', 'max-width':
    if HasValue and (FValue is TPLCSSPropertyValuePartDimension) then begin
      vf := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValue).Value, TPLCSSPropertyValuePartDimension(FValue).&Unit, AEnvironment.DocumentBody);
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureWidth >= vf
      else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureWidth <= vf
      else Result := AEnvironment.FeatureWidth = vf;
    end;
    'display-mode': if HasValue and (FValue is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      Result := AEnvironment.FeatureDisplayMode = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
    'grid': begin
      if not HasValue then
        Result := AEnvironment.FeatureGrid
      else if FValue is TPLCSSPropertyValuePartNumber then
        Result := BoolToStr(AEnvironment.FeatureGrid, '1', '0').ToExtended = TPLCSSPropertyValuePartNumber(FValue).Value;
    end;
    'hover': begin
      if not HasValue then
        Result := AEnvironment.FeatureHover
      else if FValue is TPLCSSPropertyValuePartStringOrIdentifier then
        Result := AEnvironment.FeatureHover and (TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value = 'hover');
    end;
    'monochrome', 'min-monochrome', 'max-monochrome': begin
      if not HasValue and (FFeature = 'monochrome') then
        Result := AEnvironment.FeatureMonochrome > 0
      else if FValue is TPLCSSPropertyValuePartNumber then begin
        vf := TPLCSSPropertyValuePartNumber(FValue).Value;
        if FFeature.StartsWith('min') then Result := AEnvironment.FeatureMonochrome >= vf
        else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureMonochrome <= vf
        else Result := AEnvironment.FeatureMonochrome = vf;
      end;
    end;
    'orientation': if HasValue and (FValue is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      Result := AEnvironment.FeatureOrientation = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
    'pointer': begin
      if not HasValue then
        Result := AEnvironment.FeaturePointer <> 'none'
      else if FValue is TPLCSSPropertyValuePartStringOrIdentifier then
        Result := AEnvironment.FeaturePointer = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
    'prefers-color-scheme': if HasValue and (FValue is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      Result := AEnvironment.PreferredColorScheme = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
    'prefers-reduced-motion': if HasValue and (FValue is TPLCSSPropertyValuePartStringOrIdentifier) then begin
      Result := AEnvironment.PreferredReducedMotion = (TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value = 'reduce');
    end;
    'resolution', 'min-resolution', 'max-resolution':
    if HasValue and (FValue is TPLCSSPropertyValuePartDimension) then begin
      vf := TPLCSSPropertyValuePartDimension(FValue).Value;
      case TPLCSSPropertyValuePartDimension(FValue).&Unit of
        'dpi': ; // = vf
        'dpcm': vf /= 2.54;
        'dppx', 'x': vf /= 96;
        else vf := 0;
      end;
      if FFeature.StartsWith('min') then Result := AEnvironment.FeatureResolution >= vf
        else if FFeature.StartsWith('max') then Result := AEnvironment.FeatureResolution <= vf
        else Result := AEnvironment.FeatureResolution = vf;
    end;
    'update': begin
      if not HasValue then
        Result := AEnvironment.FeatureUpdate <> 'none'
      else if FValue is TPLCSSPropertyValuePartStringOrIdentifier then
        Result := AEnvironment.FeatureUpdate = TPLCSSPropertyValuePartStringOrIdentifier(FValue).Value;
    end;
  end;
end;

function TPLCSSMediaExpression.AsString: TPLString;
begin
  Result := '(' + FFeature;

  if HasValue then
    Result += ': ' + FValue.AsString;

  Result += ')';
end;

{ TPLCSSMediaExpressionRange }

constructor TPLCSSMediaExpressionRange.Create(const AFeature: TPLString;
  const AOperator: TPLCSSMediaExpressionRangeOperator;
  const AValue: TPLCSSPropertyValuePart);
begin
  Create(AFeature, AOperator, meroEqual, AValue, nil);
end;

constructor TPLCSSMediaExpressionRange.Create(const AFeature: TPLString;
  const AOperator1, AOperator2: TPLCSSMediaExpressionRangeOperator;
  const AValue1, AValue2: TPLCSSPropertyValuePart);
begin
  inherited Create;

  FFeature := AFeature;
  FOperatorFirst := AOperator1;
  FOperatorSecond := AOperator2;
  FValueFirst := AValue1;
  FValueSecond := AValue2;
end;

destructor TPLCSSMediaExpressionRange.Destroy;
begin
  if Assigned(FValueFirst) then FreeAndNil(FValueFirst);
  if Assigned(FValueSecond) then FreeAndNil(FValueSecond);

  inherited Destroy;
end;

function TPLCSSMediaExpressionRange.IsCorrect: TPLBool;
begin
  Result := inherited IsCorrect and (HasOneSide or HasTwoSides);
end;

function TPLCSSMediaExpressionRange.HasOneSide: TPLBool;
begin
  Result := Assigned(FValueFirst) and not Assigned(FValueSecond);
end;

function TPLCSSMediaExpressionRange.HasTwoSides: TPLBool;
begin
  Result := Assigned(FValueFirst) and Assigned(FValueSecond);
end;

function TPLCSSMediaExpressionRange.Clone: TPLCSSMediaExpressionBase;
begin
  Result := TPLCSSMediaExpressionRange.Create(FFeature, FOperatorFirst,
    FOperatorSecond, FValueFirst, FValueSecond);
end;

function TPLCSSMediaExpressionRange.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
var
  vf1, vf2, vc: TPLFloat;
begin
  Result := inherited Evaluate(AEnvironment);
  if not Result then exit;

  Result := false;

  case FFeature of
    'aspect-ratio', 'device-aspect-ratio': if (FValueFirst is TPLCSSPropertyValuePartFunction) and
      (TPLCSSPropertyValuePartFunction(FValueFirst).Arguments.Count = 2) and
      (TPLCSSPropertyValuePartFunction(FValueFirst).Arguments.First is TPLCSSPropertyValuePartNumber) and
      (TPLCSSPropertyValuePartFunction(FValueFirst).Arguments.Last is TPLCSSPropertyValuePartNumber) then begin

      if FFeature = 'aspect-ratio' then
        vc := AEnvironment.FeatureAspectRatio
      else
        vc := AEnvironment.FeatureDeviceAspectRatio;

      vf1 := TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValueFirst).Arguments.First).Value /
        TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValueFirst).Arguments.Last).Value;

      if HasTwoSides then begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 > vc;
          meroGreaterEqual: Result := vf1 >= vc;
          meroLess: Result := vf1 < vc;
          meroLessEqual: Result := vf1 <= vc;
        end;
      end else begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 < vc;
          meroGreaterEqual: Result := vf1 <= vc;
          meroLess: Result := vf1 > vc;
          meroLessEqual: Result := vf1 >= vc;
        end;
      end;

      if Result and HasTwoSides and (FValueSecond is TPLCSSPropertyValuePartFunction) and
        (TPLCSSPropertyValuePartFunction(FValueSecond).Arguments.Count = 2) and
        (TPLCSSPropertyValuePartFunction(FValueSecond).Arguments.First is TPLCSSPropertyValuePartNumber) and
        (TPLCSSPropertyValuePartFunction(FValueSecond).Arguments.Last is TPLCSSPropertyValuePartNumber) then begin

        vf2 := TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValueSecond).Arguments.First).Value /
        TPLCSSPropertyValuePartNumber(TPLCSSPropertyValuePartFunction(FValueSecond).Arguments.Last).Value;

        case FOperatorSecond of
          meroEqual: Result := vf2 = vc;
          meroGreater: Result := vf2 > vc;
          meroGreaterEqual: Result := vf2 >= vc;
          meroLess: Result := vf2 < vc;
          meroLessEqual: Result := vf2 <= vc;
        end;
      end;
    end;
    'color', 'color-index', 'monochrome': if FValueFirst is TPLCSSPropertyValuePartNumber then begin
      case FFeature of
        'color': vc := AEnvironment.FeatureColor;
        'color-index': vc := AEnvironment.FeatureColorIndex;
        'monochrome': vc := AEnvironment.FeatureMonochrome;
      end;

      vf1 := TPLCSSPropertyValuePartNumber(FValueFirst).Value;

      if HasTwoSides then begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 > vc;
          meroGreaterEqual: Result := vf1 >= vc;
          meroLess: Result := vf1 < vc;
          meroLessEqual: Result := vf1 <= vc;
        end;
      end else begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 < vc;
          meroGreaterEqual: Result := vf1 <= vc;
          meroLess: Result := vf1 > vc;
          meroLessEqual: Result := vf1 >= vc;
        end;
      end;

      if Result and HasTwoSides and (FValueSecond is TPLCSSPropertyValuePartNumber) then begin
        vf2 := TPLCSSPropertyValuePartNumber(FValueSecond).Value;

        case FOperatorSecond of
          meroEqual: Result := vf2 = vc;
          meroGreater: Result := vf2 > vc;
          meroGreaterEqual: Result := vf2 >= vc;
          meroLess: Result := vf2 < vc;
          meroLessEqual: Result := vf2 <= vc;
        end;
      end;
    end;
    'device-height', 'device-width', 'height', 'width', 'resolution':
    if FValueFirst is TPLCSSPropertyValuePartDimension then begin
      case FFeature of
        'device-height': vc := AEnvironment.FeatureDeviceHeight;
        'device-width': vc := AEnvironment.FeatureDeviceWidth;
        'height': vc := AEnvironment.FeatureHeight;
        'width': vc := AEnvironment.FeatureWidth;
        'resolution': vc := AEnvironment.FeatureResolution;
      end;

      if FFeature = 'resolution' then begin
        vf1 := TPLCSSPropertyValuePartDimension(FValueFirst).Value;
        case TPLCSSPropertyValuePartDimension(FValueFirst).&Unit of
          'dpi': ; // = vf1
          'dpcm': vf1 /= 2.54;
          'dppx', 'x': vf1 /= 96;
          else vf1 := 0;
        end;
      end else begin
        vf1 := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValueFirst).Value, TPLCSSPropertyValuePartDimension(FValueFirst).&Unit, AEnvironment.DocumentBody);
      end;

      if HasTwoSides then begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 > vc;
          meroGreaterEqual: Result := vf1 >= vc;
          meroLess: Result := vf1 < vc;
          meroLessEqual: Result := vf1 <= vc;
        end;
      end else begin
        case FOperatorFirst of
          meroEqual: Result := vf1 = vc;
          meroGreater: Result := vf1 < vc;
          meroGreaterEqual: Result := vf1 <= vc;
          meroLess: Result := vf1 > vc;
          meroLessEqual: Result := vf1 >= vc;
        end;
      end;

      if Result and HasTwoSides and (FValueSecond is TPLCSSPropertyValuePartDimension) then begin
        if FFeature = 'resolution' then begin
          vf2 := TPLCSSPropertyValuePartDimension(FValueSecond).Value;
          case TPLCSSPropertyValuePartDimension(FValueSecond).&Unit of
            'dpi': ; // = vf2
            'dpcm': vf2 /= 2.54;
            'dppx', 'x': vf2 /= 96;
            else vf2 := 0;
          end;
        end else begin
          vf2 := AutoLengthToPx(TPLCSSPropertyValuePartDimension(FValueSecond).Value, TPLCSSPropertyValuePartDimension(FValueSecond).&Unit, AEnvironment.DocumentBody);
        end;

        case FOperatorSecond of
          meroEqual: Result := vf2 = vc;
          meroGreater: Result := vf2 > vc;
          meroGreaterEqual: Result := vf2 >= vc;
          meroLess: Result := vf2 < vc;
          meroLessEqual: Result := vf2 <= vc;
        end;
      end;
    end;
  end;
end;

function TPLCSSMediaExpressionRange.AsString: TPLString;
begin
  Result := '(';

  if HasOneSide then begin
    Result += FFeature + ' ' + TPLString(FOperatorFirst) + ' ' + FValueFirst.AsString;
  end else if HasTwoSides then begin
    Result += FValueFirst.AsString + ' ' + TPLString(FOperatorFirst) + ' ' + FFeature +
      ' ' + TPLString(FOperatorSecond) + ' ' + FValueSecond.AsString;
  end;

  Result += ')';
end;

{ TPLCSSMediaQuery }

constructor TPLCSSMediaQuery.Create;
begin
  inherited Create;

  FExpressions := TPLCSSMediaExpressions.Create(true);
  FQualifier := mqNone;
  FMediaType := 'all';
end;

destructor TPLCSSMediaQuery.Destroy;
begin
  FExpressions.Free;

  inherited Destroy;
end;

function TPLCSSMediaQuery.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
var
  e: TPLCSSMediaExpressionBase;
begin
  if AEnvironment.UsePrinter then
    Result := FMediaType in TPLStringFuncs.NewArray(['all', 'print'])
  else Result := FMediaType in TPLStringFuncs.NewArray(['all', 'screen']);

  if Result then for e in FExpressions do begin
    Result := Result and e.Evaluate(AEnvironment);
    if not Result then break;
  end;

  if FQualifier = mqNot then Result := not Result;
end;

function TPLCSSMediaQuery.AsString: TPLString;
var
  e: TPLCSSMediaExpressionBase;
begin
  Result := '';

  if FQualifier <> mqNone then
  case FQualifier of
    mqNot: Result := 'not ';
    mqOnly: Result := 'only ';
  end;

  Result += FMediaType;

  if FExpressions.Empty then exit;

  Result += ' and ';

  for e in FExpressions do begin
    Result += e.AsString;
    if e <> FExpressions.Last then Result += ' and ';
  end;
end;

{ TPLCSSMediaQueries }

function TPLCSSMediaQueries.Evaluate(
  const AEnvironment: TPLCSSMediaQueriesEnvironment): TPLBool;
var
  q: TPLCSSMediaQuery;
begin
  Result := false;

  for q in self do begin
    Result := Result or q.Evaluate(AEnvironment);
    if Result then break;
  end;
end;

function TPLCSSMediaQueries.AsString: TPLString;
var
  q: TPLCSSMediaQuery;
begin
  Result := '';

  for q in self do begin
    Result += q.AsString;
    if q <> Last then Result += ', ';
  end;

  Result := Result.Trim;
end;

{ TPLCSSMediaQueryParser }

class procedure TPLCSSMediaQueryParser.ParseMediaQueries(ASource: TPLString;
  var AMediaQueries: TPLCSSMediaQueries);
label
  abort_end, one_side2;
const
  set_idm = ['A'..'Z', 'a'..'z', '_'];
  set_id = set_idm + ['-'];
  set_num = ['0'..'9'];
  set_comp = ['<', '>', '='];
var
  pos: SizeInt = 1;
  q: TPLCSSMediaQuery = nil;

  function IsEOF: TPLBool; inline;
  begin
    Result := pos > Length(ASource);
  end;

  function Current: TPLChar; inline;
  begin
    Result := ASource[pos];
  end;

  function Peek: TPLChar; inline;
  begin
    Result := ifthen(pos+1 > Length(ASource), #0, ASource[pos+1])[1];
  end;

  procedure ConsumeWhitespace;
  begin
    while not IsEOF and (Current in ''.WhitespacesArrayString) do Inc(pos);
  end;

  function GetIdentifier(out id: TPLString): TPLBool;
  var
    bpos: SizeInt;
  begin
    Result := true;
    id := '';
    bpos := pos;

    if (Current in ['0'..'9']) or ((Current = '-') and (Peek in ['0'..'9'])) then
      Result := false;

    if not Result then exit;

    while not IsEOF and Result do begin
      if Current.IsWhiteSpace or (Current = ':') then break;
      if not (Current in (set_id + set_num)) then
        Result := false
      else begin
        id += Current;
        Inc(pos);
      end;
    end;

    if not Result then begin
      id := '';
      pos := bpos;
    end;
  end;

  function GetValue(const stop_chars: TPLCharSet): TPLString;
  begin
    Result := '';

    while not (IsEOF or (Current in stop_chars)) do begin
      Result += Current;
      Inc(pos);
    end;
  end;

var
  idn, ft, v1, v2, ops: TPLString;
  op1, op2: TPLCSSMediaExpressionRangeOperator;
  part1, part2: TPLCSSPropertyValuePart;
  vf: TPLFloat;
  arr: TPLStringArray;

  procedure Abort;
  begin
    if Assigned(q) then q.Free;
    if Assigned(part1) then part1.Free;
    if Assigned(part2) then part2.Free;
    AMediaQueries.Clear;
  end;

begin
  ASource := ASource.Trim.ToLower.Replace('(', ' ').Replace(')', ' ') + ' ';

  while not IsEOF do begin
    ConsumeWhitespace;

    q := TPLCSSMediaQuery.Create;

    if GetIdentifier(idn) then begin
      case idn of
        'not': q.Qualifier := mqNot;
        'only': q.Qualifier := mqOnly;
      end;

      if q.Qualifier <> mqNone then begin
        ConsumeWhitespace;
        GetIdentifier(idn);
      end;

      if idn in TPLStringFuncs.NewArray(['all', 'print', 'screen', 'tty', 'tv',
        'projection', 'handheld', 'braille', 'embossed', 'aural', 'speech'])
      then begin
        q.MediaType := idn;
        ConsumeWhitespace;
        GetIdentifier(idn);
      end;
    end;

    while not IsEOF do begin
      if idn = '' then begin // range
        ConsumeWhitespace;
        v1 := GetValue(set_comp + [' ']);
        ConsumeWhitespace;
        if not (Current in set_comp) then goto abort_end;
        ops := Current;
        Inc(pos);
        if Current in set_comp then begin
          ops += Current;
          Inc(pos);
        end;
        ConsumeWhitespace;
        if not GetIdentifier(idn) then goto abort_end;
        ft := idn;
        ConsumeWhitespace;

        if TryStrToFloat(v1, vf) then
          part1 := TPLCSSPropertyValuePartNumber.Create(v1)
        else begin
          if v1.Exists('/') then begin
            arr := v1.Split('/');
            if Length(arr) <> 2 then goto abort_end;
            arr[0] := arr[0].Trim;
            arr[1] := arr[1].Trim;
            vf := arr[1];
            if vf = 0 then goto abort_end; // cannot divide by 0
            part1 := TPLCSSPropertyValuePartFunction.Create('r(%s, %s)'.Format([arr[0], arr[1]])); // ratio function
          end else if TPLCSSPropertyValuePartDimension.IsDimensionValue(v1) then
            part1 := TPLCSSPropertyValuePartDimension.Create(v1)
          else
            part1 := TPLCSSPropertyValuePartStringOrIdentifier.Create(v1);
        end;

        if GetIdentifier(idn) then begin // only one side
          case ops of // mirror
            '>': op1 := meroLess;
            '<': op1 := meroGreater;
            '>=': op1 := meroLessEqual;
            '<=': op1 := meroGreaterEqual;
            else op1 := meroEqual;
          end;

          q.Expressions.Add(TPLCSSMediaExpressionRange.Create(ft, op1, part1));
          part1 := nil;
          part2 := nil;
          continue;
        end;

        case ops of
          '<': op1 := meroLess;
          '>': op1 := meroGreater;
          '<=': op1 := meroLessEqual;
          '>=': op1 := meroGreaterEqual;
          else op1 := meroEqual;
        end;

        ConsumeWhitespace;
        if not (Current in set_comp) then goto abort_end;
        ops := Current;
        Inc(pos);
        if Current in set_comp then begin
          ops += Current;
          Inc(pos);
        end;
        ConsumeWhitespace;

        case ops of
          '<': op2 := meroLess;
          '>': op2 := meroGreater;
          '<=': op2 := meroLessEqual;
          '>=': op2 := meroGreaterEqual;
          else op2 := meroEqual;
        end;

        v2 := GetValue(set_comp + [' ']);
        ConsumeWhitespace;

        if TryStrToFloat(v2, vf) then
          part2 := TPLCSSPropertyValuePartNumber.Create(v2)
        else begin
          if v2.Exists('/') then begin
            arr := v2.Split('/');
            if Length(arr) <> 2 then goto abort_end;
            arr[0] := arr[0].Trim;
            arr[1] := arr[1].Trim;
            vf := arr[1];
            if vf = 0 then goto abort_end; // cannot divide by 0
            part2 := TPLCSSPropertyValuePartFunction.Create('r(%s, %s)'.Format([arr[0], arr[1]])); // ratio function
          end else if TPLCSSPropertyValuePartDimension.IsDimensionValue(v2) then
            part2 := TPLCSSPropertyValuePartDimension.Create(v2)
          else
            part2 := TPLCSSPropertyValuePartStringOrIdentifier.Create(v2);
        end;

        q.Expressions.Add(TPLCSSMediaExpressionRange.Create(ft, op1, op2, part1, part2));
        part1 := nil;
        part2 := nil;
        GetIdentifier(idn);
      end else if idn <> 'and' then begin // normal or range?
        if Current.IsWhiteSpace then begin // normal without value
          ConsumeWhitespace;
          if Current in set_comp then goto one_side2;
          q.Expressions.Add(TPLCSSMediaExpression.Create(idn));
          ConsumeWhitespace;
        end else if Current = ':' then begin // normal with value
          ConsumeWhitespace;
          Inc(pos);
          ConsumeWhitespace;
          ft := idn;
          v1 := GetValue(set_comp + [' ']);

          if TryStrToFloat(v1, vf) then
            part1 := TPLCSSPropertyValuePartNumber.Create(v1)
          else begin
            if v1.Exists('/') then begin
              arr := v1.Split('/');
              if Length(arr) <> 2 then goto abort_end;
              arr[0] := arr[0].Trim;
              arr[1] := arr[1].Trim;
              vf := arr[1];
              if vf = 0 then goto abort_end; // cannot divide by 0
              part1 := TPLCSSPropertyValuePartFunction.Create('r(%s, %s)'.Format([arr[0], arr[1]])); // ratio function
            end else if TPLCSSPropertyValuePartDimension.IsDimensionValue(v1) then
              part1 := TPLCSSPropertyValuePartDimension.Create(v1)
            else
              part1 := TPLCSSPropertyValuePartStringOrIdentifier.Create(v1);
          end;

          q.Expressions.Add(TPLCSSMediaExpression.Create(ft, part1));
          part1 := nil;
          part2 := nil;
        end else if Current in set_comp then begin // one-side range
          ConsumeWhitespace;
          one_side2:
          ft := idn;
          ops := Current;
          Inc(pos);
          if Current in set_comp then begin
            ops += Current;
            Inc(pos);
          end;
          ConsumeWhitespace;

          case ops of
            '<': op1 := meroLess;
            '>': op1 := meroGreater;
            '<=': op1 := meroLessEqual;
            '>=': op1 := meroGreaterEqual;
            else op1 := meroEqual;
          end;

          ConsumeWhitespace;
          v1 := GetValue(set_comp + [' ']);

          if TryStrToFloat(v1, vf) then
            part1 := TPLCSSPropertyValuePartNumber.Create(v1)
          else begin
            if v1.Exists('/') then begin
              arr := v1.Split('/');
              if Length(arr) <> 2 then goto abort_end;
              arr[0] := arr[0].Trim;
              arr[1] := arr[1].Trim;
              vf := arr[1];
              if vf = 0 then goto abort_end; // cannot divide by 0
              part1 := TPLCSSPropertyValuePartFunction.Create('r(%s, %s)'.Format([arr[0], arr[1]])); // ratio function
            end else if TPLCSSPropertyValuePartDimension.IsDimensionValue(v1) then
              part1 := TPLCSSPropertyValuePartDimension.Create(v1)
            else
              part1 := TPLCSSPropertyValuePartStringOrIdentifier.Create(v1);
          end;

          q.Expressions.Add(TPLCSSMediaExpressionRange.Create(ft, op1, part1));
          part1 := nil;
          part2 := nil;
        end else goto abort_end;
      end;

      ConsumeWhitespace;
      if (Current = ',') or (Current + Peek = 'or') then break;
      GetIdentifier(idn);
    end;

    AMediaQueries.Add(q);
    q := nil;

    ConsumeWhitespace;
    if Current = ',' then Inc(pos);
    ConsumeWhitespace;
    if Current + Peek = 'or' then Inc(pos, 2);
    ConsumeWhitespace;
  end;

  exit;

  abort_end:
    Abort;
end;

initialization
  Printer.SetPrinter('*'); // set default printer

end.

