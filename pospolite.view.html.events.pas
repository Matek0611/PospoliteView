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
  Classes, SysUtils, variants, Controls, Pospolite.View.Basics,
  Pospolite.View.Threads;

const
  // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/button#return_value
  THTMLMouseButtonsSet = [mbLeft, mbMiddle, mbRight, mbExtra1, mbExtra2];
  // https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/buttons#return_value
  THTMLMouseButtonsSetMulti: array[TMouseButton] of byte = (1, 2, 4, 8, 16);

type

  { TPLHTMLPointerEventInfo }

  PPLHTMLPointerEventInfo = ^TPLHTMLPointerEventInfo;
  TPLHTMLPointerEventInfo = packed record
  public
    Button: TMouseButton;
    Shift: TShiftState;
    X, Y: Integer;
    WheelDelta: Integer;
  end;

  { TPLHTMLEventProperties }

  TPLHTMLEventProperties = packed record
  public
    function button: Byte;
  public
    altKey: TPLBool;
    buttons: array[TMouseButton] of TPLBool;
    detail: Byte; // https://stackoverflow.com/questions/6480060/how-do-i-listen-for-triple-clicks-in-javascript
    // tbc ...
    shiftKey: TPLBool;
  end;

function GetDefaultEventProperties(const AType: TPLString): TPLHTMLEventProperties;

const
  PLHTMLEventPropertiesTemplate: TPLHTMLEventProperties = (
    altKey: false; buttons: (false, false, false, false, false); detail: 0;
    shiftKey: false;
  );

type

  { TPLHTMLEvent }

  TPLHTMLEvent = packed record
  strict private
    FEvent: TPLAsyncProc;
    FProperties: TPLHTMLEventProperties;
    FTarget: TPLHTMLObject;
    FType: TPLString;
    FPrevent: TPLBool;
  public
    constructor Create(AEvent: TPLAsyncProc; ATarget: TPLHTMLObject; AType: TPLString;
      AProps: TPLHTMLEventProperties);

    class operator = (a, b: TPLHTMLEvent) r: TPLBool;

    procedure Call(const AArgs: array of const);
    procedure PreventDefault;
    procedure RestoreDefault;

    property Target: TPLHTMLObject read FTarget write FTarget;
    property &Type: TPLString read FType write FType;
    property Properties: TPLHTMLEventProperties read FProperties write FProperties;
  end;

  { TPLHTMLEventListener }

  TPLHTMLEventListener = packed record
  private
    FEvent: TPLHTMLEvent;
  public
    constructor Create(AEvent: TPLHTMLEvent);

    class operator = (a, b: TPLHTMLEventListener) r: TPLBool;

    procedure HandleEvent(AEvent: TPLHTMLEvent; const AArgs: array of const);
  end;

  TPLHTMLEventListeners = class(specialize TPLList<TPLHTMLEventListener>);

  { TPLHTMLEventListenerItem }

  TPLHTMLEventListenerItem = class
  private
    FList: TPLHTMLEventListeners;
    FTypeName: TPLString;
  public
    constructor Create(const ATypeName: TPLString);
    destructor Destroy; override;

    property TypeName: TPLString read FTypeName;
    property List: TPLHTMLEventListeners read FList;
  end;

  TPLFuncsOfClassEventListenerItem = specialize TPLFuncsOfClass<TPLHTMLEventListenerItem>;
  TPLHTMLEventListenerList = class(specialize TPLObjectList<TPLHTMLEventListenerItem>);

  { TPLHTMLEventTarget }

  TPLHTMLEventTarget = class
  private
    FListeners: TPLHTMLEventListenerList;
    FObject: TPLHTMLObject;
    function ComparatorForSearch(const AObject: TPLHTMLEventListenerItem;
      const ACriteria: Variant): TPLSign;
    function ComparatorForSort(a, b: TPLHTMLEventListenerItem): TPLSign;
  public
    constructor Create(AObject: TPLHTMLObject);
    destructor Destroy; override;

    procedure AddEventListener(const AType: TPLString; AListener: TPLHTMLEventListener);
    procedure RemoveEventListener(const AType: TPLString; AListener: TPLHTMLEventListener);
    procedure DispatchEvent(AEvent: TPLHTMLEvent; AArgs: array of const);
    procedure DispatchAllEventsFromListeners(const AType: TPLString);

    property Listeners: TPLHTMLEventListenerList read FListeners;
  end;

  TPLHTMLEventManagerQueueItem = specialize TPLParameter<TPLHTMLObject, TPLString>;
  TPLHTMLEventManagerQueue = class(specialize TPLList<TPLHTMLEventManagerQueueItem>);

  { TPLHTMLEventManager }

  TPLHTMLEventManager = class sealed
  private
    FDocument: Pointer;
    FEnabled: TPLBool;
    FFocused: TPLBool;
    FFocusedElement: TPLHTMLObject;
    FTask: TPLAsyncTask;
    FQueue: TPLHTMLEventManagerQueue;

    procedure SetDocument(AValue: Pointer);
    procedure AsyncProc(const {%H-}AArguments: array of const);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartEvents;
    procedure StopEvents;
    procedure DoEvent(AObject: TPLHTMLObject; const AType: TPLString);

    property Document: Pointer read FDocument write SetDocument;
    property Focused: TPLBool read FFocused write FFocused;
    property FocusedElement: TPLHTMLObject read FFocusedElement write FFocusedElement;
  end;

implementation

function GetDefaultEventProperties(const AType: TPLString
  ): TPLHTMLEventProperties;
begin
  Result := PLHTMLEventPropertiesTemplate;

  case AType of
    'click': begin
      Result.detail := 1;
      Result.buttons[mbLeft] := true;
    end;
    'dblclick': begin
      Result.detail := 2;
      Result.buttons[mbLeft] := true;
    end;
    'tripleclick': begin
      Result.detail := 3;
      Result.buttons[mbLeft] := true;
    end;
    'quadclick': begin
      Result.detail := 4;
      Result.buttons[mbLeft] := true;
    end;
    'contextmenu': begin
      Result.detail := 1;
      Result.buttons[mbRight] := true;
    end;
    else begin
      // tmp
      Result.detail := 1;
      Result.buttons[mbLeft] := true;
    end;
  end;
end;

{ TPLHTMLEventProperties }

function TPLHTMLEventProperties.button: Byte;
var
  m: TMouseButton;
begin
  Result := 0;

  for m in THTMLMouseButtonsSet do
    if buttons[m] then Result += THTMLMouseButtonsSetMulti[m];
end;

{ TPLHTMLEvent }

constructor TPLHTMLEvent.Create(AEvent: TPLAsyncProc; ATarget: TPLHTMLObject;
  AType: TPLString; AProps: TPLHTMLEventProperties);
begin
  FEvent := AEvent;
  FTarget := ATarget;
  FType := AType;
  FPrevent := false;
  FProperties := AProps;
end;

class operator TPLHTMLEvent.=(a, b: TPLHTMLEvent) r: TPLBool;
begin
  r := a.FEvent = b.FEvent;
end;

procedure TPLHTMLEvent.Call(const AArgs: array of const);
begin
  if not FPrevent and Assigned(FEvent.AsPureProc) then
    FEvent.AsPureProc()(AArgs);
end;

procedure TPLHTMLEvent.PreventDefault;
begin
  FPrevent := true;
end;

procedure TPLHTMLEvent.RestoreDefault;
begin
  FPrevent := false;
end;

{ TPLHTMLEventListener }

constructor TPLHTMLEventListener.Create(AEvent: TPLHTMLEvent);
begin
  FEvent := AEvent;
end;

class operator TPLHTMLEventListener.=(a, b: TPLHTMLEventListener) r: TPLBool;
begin
  r := a.FEvent = b.FEvent;
end;

procedure TPLHTMLEventListener.HandleEvent(AEvent: TPLHTMLEvent;
  const AArgs: array of const);
begin
  AEvent.Call(AArgs);
end;

{ TPLHTMLEventListenerItem }

constructor TPLHTMLEventListenerItem.Create(const ATypeName: TPLString);
begin
  inherited Create;

  FTypeName := ATypeName;
  FList := TPLHTMLEventListeners.Create;
end;

destructor TPLHTMLEventListenerItem.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

{ TPLHTMLEventTarget }

function TPLHTMLEventTarget.ComparatorForSearch(
  const AObject: TPLHTMLEventListenerItem; const ACriteria: Variant): TPLSign;
var
  s: TPLString;
begin
  s := VarToStr(ACriteria);

  if AObject.FTypeName < s then Result := -1
  else if AObject.FTypeName > s then Result := 1
  else Result := 0;
end;

function TPLHTMLEventTarget.ComparatorForSort(a, b: TPLHTMLEventListenerItem
  ): TPLSign;
begin
  if a.FTypeName < b.FTypeName then Result := -1
  else if a.FTypeName > b.FTypeName then Result := 1
  else Result := 0;
end;

constructor TPLHTMLEventTarget.Create(AObject: TPLHTMLObject);
begin
  FObject := AObject;

  FListeners := TPLHTMLEventListenerList.Create;
end;

destructor TPLHTMLEventTarget.Destroy;
begin
  FListeners.Free;

  inherited Destroy;
end;

procedure TPLHTMLEventTarget.AddEventListener(const AType: TPLString;
  AListener: TPLHTMLEventListener);
var
  id: SizeInt;
  found: TPLBool = true;
begin
  id := TPLFuncsOfClassEventListenerItem.FastSearch(FListeners, AType, @ComparatorForSearch);

  if id < 0 then begin
    FListeners.Add(TPLHTMLEventListenerItem.Create(AType));
    id := FListeners.Count - 1;
    found := false;
  end;

  FListeners[id].FList.Add(AListener);

  if not found then
    FListeners.Sort(@ComparatorForSort);
end;

procedure TPLHTMLEventTarget.RemoveEventListener(const AType: TPLString;
  AListener: TPLHTMLEventListener);
var
  id: SizeInt;
begin
  id := TPLFuncsOfClassEventListenerItem.FastSearch(FListeners, AType, @ComparatorForSearch);

  if id < 0 then exit;

  FListeners[id].FList.Remove(AListener);
end;

procedure TPLHTMLEventTarget.DispatchEvent(AEvent: TPLHTMLEvent;
  AArgs: array of const);
begin
  AEvent.Call(AArgs);
end;

procedure TPLHTMLEventTarget.DispatchAllEventsFromListeners(
  const AType: TPLString);
var
  id, x: SizeInt;
begin
  id := TPLFuncsOfClassEventListenerItem.FastSearch(FListeners, AType, @ComparatorForSearch);

  if id < 0 then exit;

  for x := 0 to FListeners[id].FList.Count-1 do
    FListeners[id].FList[x].FEvent.Call(FObject.GetArgsFor(AType));
end;

{ TPLHTMLEventManager }

procedure TPLHTMLEventManager.SetDocument(AValue: Pointer);
begin
  if FDocument = AValue then exit;

  StopEvents;
  FDocument := AValue;
end;

procedure TPLHTMLEventManager.AsyncProc(const AArguments: array of const);
begin
  while FEnabled do begin
    if not Assigned(FDocument) or not Assigned(FQueue) then break;
    if FQueue.Empty then continue;

    TPLHTMLEventTarget(FQueue.First.Key.GetElementTarget).DispatchAllEventsFromListeners(FQueue.First.Value);
    FQueue.Remove(FQueue.First);
  end;
end;

constructor TPLHTMLEventManager.Create;
begin
  inherited Create;

  FQueue := TPLHTMLEventManagerQueue.Create;
  FTask := nil;
  FDocument := nil;
  FFocused := false;
  FFocusedElement := nil;
end;

destructor TPLHTMLEventManager.Destroy;
begin
  StopEvents;
  FQueue.Free;

  inherited Destroy;
end;

procedure TPLHTMLEventManager.StartEvents;
begin
  if not Assigned(FDocument) then exit;
  if Assigned(FTask) then FreeAndNil(FTask);

  FTask := TPLAsyncTask.Create(@AsyncProc);
  FTask.FreeIfDone := false;
  FTask.Name := 'Event Manager';
  FTask.Async([]);
end;

procedure TPLHTMLEventManager.StopEvents;
begin
  FEnabled := false;

  if Assigned(FTask) then begin
    FTask.Cancel;
    FreeAndNil(FTask);
  end;
end;

procedure TPLHTMLEventManager.DoEvent(AObject: TPLHTMLObject;
  const AType: TPLString);
begin
  FQueue.Add(TPLHTMLEventManagerQueueItem.Create(AObject, AType));
end;

end.

