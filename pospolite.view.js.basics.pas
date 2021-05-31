unit Pospolite.View.JS.Basics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, strutils, Pospolite.View.Basics;

{type

  TPLJSGarbageCollectorState = (gstNone, gstBegin, gstMark, gstClean);

  TPLJSLoopSignal = (lsNone, lsNoOperation, lsBreak);

  TPLJSValueType = (vtNumber = 1, vtString, vtBoolean, vtObject, vtNull, vtUndefined);

  TPLJSPropertyFlag = (pfNone, pfWrite, pfEnum, pfConfig, pfIgnore);
  TPLJSPropertyFlags = set of TPLJSPropertyFlag;

 const
   pfBuiltIn = [pfWrite, pfConfig];
   pfDefault = pfBuiltIn + [pfEnum];

type

  TPLJSValue = class;
  TPLJSArguments = class;
  TPLJSASTNode = class;
  TPLJSEvaluationState = class;
  TPLJSGarbageCollectorPart = class;

  TPLJSGarbageCollectorParts = specialize TPLObjectList<TPLJSGarbageCollectorPart>;

  TPLJSStateOpts = packed record
    Interactive, ShowTokens, ShowAST, KeepHistory: TPLBool;
    History: TPLString;
  end;

  { TPLJSState }

  TPLJSState = class
  private
    FCallstack: TPLJSEvaluationState;
    FGCLastStart: TDateTime;
    FGCParts: TPLJSGarbageCollectorParts;
    FGCRuns: TPLInt;
    FGCState: TPLJSGarbageCollectorState;
    FGCTime: TDateTime;
    FGlobal: TPLJSValue;
    FOptions: TPLJSStateOpts;
    FPrototypeArray: TPLJSValue;
    FPrototypeFunction: TPLJSValue;
    FPrototypeObject: TPLJSValue;
    FScriptName: TPLString;
  public
    constructor Create;
    destructor Destroy; override;

    property GCState: TPLJSGarbageCollectorState read FGCState write FGCState;
    property GCParts: TPLJSGarbageCollectorParts read FGCParts write FGCParts;
    property GCRuns: TPLInt read FGCRuns write FGCRuns;
    property GCLastStart: TDateTime read FGCLastStart write FGCLastStart;
    property GCTime: TDateTime read FGCTime write FGCTime;

    property Options: TPLJSStateOpts read FOptions write FOptions;
    property ScriptName: TPLString read FScriptName write FScriptName;
    property Callstack: TPLJSEvaluationState read FCallstack write FCallstack;

    property PrototypeFunction: TPLJSValue read FPrototypeFunction write FPrototypeFunction;
    property PrototypeObject: TPLJSValue read FPrototypeObject write FPrototypeObject;
    property PrototypeArray: TPLJSValue read FPrototypeArray write FPrototypeArray;
    property Global: TPLJSValue read FGlobal write FGlobal;
  end;

var
  PLJSGlobalState: TPLJSState;

type

  { TPLJSEvaluationState }

  TPLJSEvaluationState = class
  private
    FCaller: TPLString;
    FCatch: TPLBool;
    FColumn: TPLInt;
    FConstruct: TPLBool;
    FContext: TPLJSValue;
    FLine: TPLInt;
    FParent: TPLJSEvaluationState;
    FScope: TPLJSValue;
    FScriptName: TPLString;
    FThis: TPLJSValue;
  public
    constructor Create;

    property Line: TPLInt read FLine write FLine;
    property Column: TPLInt read FColumn write FColumn;

    property ScriptName: TPLString read FScriptName write FScriptName;
    property Caller: TPLString read FCaller write FCaller;
    property Parent: TPLJSEvaluationState read FParent write FParent;

    property Construct: TPLBool read FConstruct write FConstruct;
    property Catch: TPLBool read FCatch write FCatch;

    property Context: TPLJSValue read FContext write FContext;
    property This: TPLJSValue read FThis write FThis;
    property Scope: TPLJSValue read FScope write FScope;
  end;

  { TPLJSProperty }

  TPLJSProperty = packed class
  private
    FCanConfigure: TPLBool;
    FCanEnumerate: TPLBool;
    FCanWrite: TPLBool;
    FCircular: TPLBool;
    FName: TPLString;
    FValue: TPLJSValue;
  public
    property Name: TPLString read FName write FName;
    property Value: TPLJSValue read FValue write FValue;

    property CanWrite: TPLBool read FCanWrite write FCanWrite;
    property CanEnumerate: TPLBool read FCanEnumerate write FCanEnumerate;
    property CanConfigure: TPLBool read FCanConfigure write FCanConfigure;
    property Circular: TPLBool read FCircular write FCircular;
  end;

  TPLJSNumber = TPLFloat;
  TPLJSString = TPLString;
  TPLJSBool = TPLBool;

  TPLJSNativeFunction = function(AValue: TPLJSValue; AArgs: TPLJSArguments;
    AEvalState: TPLJSEvaluationState): TPLJSValue;

  { TPLJSObject }

  TPLJSObject = packed class
  private
    FBoundArgs: TPLJSArguments;
    FBoundThis: TPLJSValue;
    FClassName: TPLString;
    FInstance: TPLJSValue;
    FIsExtensible: TPLBool;
    FIsGenerator: TPLBool;
    FIsNative: TPLBool;
    FIsThisSupplier: TPLBool;
    FLength: TPLUInt;
    FNativeFunction: TPLJSNativeFunction;
    FNode: TPLJSASTNode;
    FParent: TPLJSValue;
    FPrimitive: TPLJSValue;
    FScope: TPLJSValue;
  public
    property ClassName: TPLString read FClassName write FClassName;
    property Length: TPLUInt read FLength write FLength;
    property NativeFunction: TPLJSNativeFunction read FNativeFunction write FNativeFunction;

    property IsGenerator: TPLBool read FIsGenerator write FIsGenerator;
    property IsNative: TPLBool read FIsNative write FIsNative;
    property IsExtensible: TPLBool read FIsExtensible write FIsExtensible;
    property IsThisSupplier: TPLBool read FIsThisSupplier write FIsThisSupplier;

    property Node: TPLJSASTNode read FNode write FNode;
    property BoundArgs: TPLJSArguments read FBoundArgs write FBoundArgs;

    property BoundThis: TPLJSValue read FBoundThis write FBoundThis;
    property Primitive: TPLJSValue read FPrimitive write FPrimitive;
    property Parent: TPLJSValue read FParent write FParent;
    property Scope: TPLJSValue read FScope write FScope;
    property Instance: TPLJSValue read FInstance write FInstance;
  end;

  { TPLJSValue }

  TPLJSValue = class
  private
    FAsBoolean: TPLJSBool;
    FAsNumber: TPLJSNumber;
    FAsObject: TPLJSObject;
    FAsString: TPLJSString;
    FFlagged: TPLBool;
    FKind: TPLJSValueType;
    FMap: TPLJSProperty;
    FMarked: TPLBool;
    FPrototype: TPLJSValue;
    FSignal: TPLJSLoopSignal;
  public
    constructor Create(AType: TPLJSValueType);
    constructor CreateNumber(AValue: TPLJSNumber);
    constructor CreateString(AValue: TPLJSString);
    constructor CreateBoolean(AValue: TPLJSBool);
    constructor CreateObject;
    constructor CreateArray;
    constructor CreateFunction(ANode: TPLJSASTNode);
    constructor CreateNativeFunction(AValue: TPLJSNativeFunction; ALength: TPLUInt);
    constructor CreateRegExp(AValue: TPLString);
    constructor CreateError(AName, AFormat: TPLString; AArgs: array of const);
    destructor Destroy; override;

    function ToString: TPLJSString;
    class function ToString(AValue: TPLJSValue): TPLJSValue;

    property AsObject: TPLJSObject read FAsObject write FAsObject;
    property AsNumber: TPLJSNumber read FAsNumber write FAsNumber;
    property AsString: TPLJSString read FAsString write FAsString;
    property AsBoolean: TPLJSBool read FAsBoolean write FAsBoolean;

    property Kind: TPLJSValueType read FKind write FKind;
    property Signal: TPLJSLoopSignal read FSignal write FSignal;
    property Map: TPLJSProperty read FMap write FMap;
    property Flagged: TPLBool read FFlagged write FFlagged;
    property Marked: TPLBool read FMarked write FMarked;

    property Prototype: TPLJSValue read FPrototype write FPrototype;
  end;
}
implementation
{
{ TPLJSState }

constructor TPLJSState.Create;
begin
  inherited Create;

  FGCParts := TPLJSGarbageCollectorParts.Create();
  FOptions := Default(TPLJSStateOpts);
end;

destructor TPLJSState.Destroy;
begin
  FGCParts.Free;

  inherited Destroy;
end;

{ TPLJSEvaluationState }

constructor TPLJSEvaluationState.Create;
begin
  FParent := nil;
end;

{ TPLJSValue }

constructor TPLJSValue.Create(AType: TPLJSValueType);
begin
  inherited Create;

  FKind := AType;
  FSignal := lsNone;
  FMap := nil;
  FPrototype := nil;
  FMarked := false;
  FFlagged := false;
end;

constructor TPLJSValue.CreateNumber(AValue: TPLJSNumber);
begin
  Create(vtNumber);

  FAsNumber := AValue;
  //FPrototype;
end;

constructor TPLJSValue.CreateString(AValue: TPLJSString);
begin

end;

constructor TPLJSValue.CreateBoolean(AValue: TPLJSBool);
begin

end;

constructor TPLJSValue.CreateObject;
begin

end;

constructor TPLJSValue.CreateArray;
begin

end;

constructor TPLJSValue.CreateFunction(ANode: TPLJSASTNode);
begin

end;

constructor TPLJSValue.CreateNativeFunction(AValue: TPLJSNativeFunction;
  ALength: TPLUInt);
begin

end;

constructor TPLJSValue.CreateRegExp(AValue: TPLString);
begin

end;

constructor TPLJSValue.CreateError(AName, AFormat: TPLString;
  AArgs: array of const);
begin

end;

destructor TPLJSValue.Destroy;
begin
  if Assigned(FAsObject) then FAsObject.Free;

  inherited Destroy;
end;

function TPLJSValue.ToString: TPLJSString;
begin
  case FKind of
    vtUndefined: Result := 'undefined';
    vtNull: Result := 'null';
    vtBoolean: Result := ifthen(FAsBoolean, 'true', 'false');
    vtNumber: begin
      if FAsNumber.IsNan then Result := 'NaN'
      else if FAsNumber.IsInfinity then Result := 'Infinity'
      else Result := FAsNumber.ToString(PLFormatSettingsDef);
    end;
    vtObject: Result := '0x' + GetHashCode.ToString;
  end;
end;

class function TPLJSValue.ToString(AValue: TPLJSValue): TPLJSValue;
begin
  Result := TPLJSValue.CreateString(AValue.ToString);
end;

initialization
  PLJSGlobalState := TPLJSState.Create;
  with PLJSGlobalState do begin
    GCState := gstNone;

  end;

finalization
  PLJSGlobalState.Free;
}
end.

