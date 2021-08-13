unit Pospolite.View.JS.Basics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, strutils, math, Pospolite.View.Basics;

type

  TPLJSAtom = TPLUInt;

  TPLJSMode = (jsmStrict, jsmStrip, jsmMath);

  TPLJSObject = class;

  TPLJSObjectValueKind = (ovtrObject, ovtrBigDecimal, ovtrBigInt, ovtrBigFloat,
    ovtrString, ovtrFunction, ovtrModule, ovtrSymbol, ovtNull, ovtUndefined,
    ovtUninitialized, ovtException, ovtCatch, ovtInteger, ovtFloat, ovtBoolean);

  { TPLJSObjectValue }

  TPLJSObjectValue = packed record
  private
    FKind: TPLJSObjectValueKind;
    FVPtr: Pointer;
  public
    constructor Create(AValue: Pointer; AKind: TPLJSObjectValueKind);

    class function NewString(AValue: TPLString): TPLJSObjectValue; static; inline;
    class function NewInteger(AValue: TPLInt): TPLJSObjectValue; static; inline;
    class function NewFloat(AValue: TPLFloat): TPLJSObjectValue; static; inline;
    class function NewBoolean(AValue: TPLBool): TPLJSObjectValue; static; inline;
    class function NewObject(AValue: TPLJSObject): TPLJSObjectValue; static; inline;
    class function NewNull: TPLJSObjectValue; static; inline;
    class function NewUndefined: TPLJSObjectValue; static; inline;
    class function NewUninitialized: TPLJSObjectValue; static; inline;
    class function NewException: TPLJSObjectValue; static; inline;

    function AsString: TPLString;
    function AsNumber: TPLFloat;

    property Kind: TPLJSObjectValueKind read FKind;
  end;

  PPLJSObjectValue = ^TPLJSObjectValue;

  TPLJSPropertyFlag = (pfNormal, pfGetSet, pfVarRef, pfAutoInit, pfConfigurable,
    pfWriteable, pfEnumerable, pfLength, pfHasGet, pfHasSet, pfHasValue, pfThrow,
    pfThrowStrict, pfNoAdd, pfExotic);
  TPLJSPropertyFlags = set of TPLJSPropertyFlag;

const
  CPLJSPropertyFlagFullAccess = [pfConfigurable, pfWriteable, pfEnumerable];

type

  TPLSJSEvalFlag = (efGlobal, efModule, efDirect, efIndirect, efStrict, efStrip,
    efCompileOnly);

  TPLJSClassId = (ciObject, ciNumber, ciString, ciBoolean, ciBigInt, ciBigFloat,
    ciFloatEnv, ciBigDecimal, ciOpertorSet, ciSet, ciWeakSet, ciMap, ciWeakMap,
    ciDataView, ciSetIterator, ciMapIterator, ciArrayIterator, ciStringIterator,
    ciRegularExpressionIterator, ciProxy, ciGenerator, ciPromise, ciPromiseResolveFunction,
    ciPromiseRejectFunction, ciAsyncFunction, ciAsyncFunctionResolve, ciAsyncFunctionReject,
    ciAsyncGeneratorFunction, ciAsyncGenerator, ciAsyncIterator, ciError, ciSymbol,
    ciArguments, ciMappedArguments, ciDate, ciModule, ciFreePascalFunction,
    ciFreePascalFunctionData, ciBytecodeFunction, ciBoundFunction, ciGeneratorFunction,
    ciForInIterator, ciRegularExpression, ciArray, ciArrayBuffer, ciSharedArrayBuffer,
    ciUnsignedIntegerArray, ciSignedIntegerArray, ciBigIntArray, ciBigFloatArray);

  TPLJSErrorKind = (ekInternal, ekAggregate, ekType, ekSyntax, ekURI, ekReference,
    ekRange, ekEval);

  { TPLJSClass }

  TPLJSClass = packed class
  private
    FAtom: TPLJSAtom;
    FId: TPLJSClassId;
  public
    property Id: TPLJSClassId read FId write FId;
    property Atom: TPLJSAtom read FAtom write FAtom;

  end;

  { TPLJSRuntime }

  TPLJSRuntime = packed class
  private
    FInfo: TPLString;
  public
    property Info: TPLString read FInfo write FInfo;

  end;

  TPLJSGarbageCollectorStage = (gcsNone, gcDecreaseReference, gcsDeleteCycles);
  TPLJSGarbageCollectorObjectType = (gcotObject, gcotFunctionBytecode,
    gcotFunctionAsync, gcotVariableReference, gcotShape, gcotContext);

  TPLJSGarbageCollectorObjectHeader = packed record
    ObjectType: TPLJSGarbageCollectorObjectType;
  end;

  TPLJSGarbageCollectorMarker = procedure (var ARuntime: TPLJSRuntime; AValue: TPLJSObjectValue);
  TPLJSGarbageCollectorFinalizer = procedure (var ARuntime: TPLJSRuntime; const AFunction,
    AThis: TPLJSObjectValue; const AArguments: array of TPLJSObjectValue);

  TPLJSObject = class

  end;

implementation

{ TPLJSObjectValue }

constructor TPLJSObjectValue.Create(AValue: Pointer; AKind: TPLJSObjectValueKind
  );
begin
  FVPtr := AValue;
  FKind := AKind;
end;

class function TPLJSObjectValue.NewString(AValue: TPLString): TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(@AValue, ovtrString);
end;

class function TPLJSObjectValue.NewInteger(AValue: TPLInt): TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(@AValue, ovtInteger);
end;

class function TPLJSObjectValue.NewFloat(AValue: TPLFloat): TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(@AValue, ovtFloat);
end;

class function TPLJSObjectValue.NewBoolean(AValue: TPLBool): TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(@AValue, ovtBoolean);
end;

class function TPLJSObjectValue.NewObject(AValue: TPLJSObject
  ): TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(AValue, ovtrObject);
end;

class function TPLJSObjectValue.NewNull: TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(nil, ovtNull);
end;

class function TPLJSObjectValue.NewUndefined: TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(nil, ovtUndefined);
end;

class function TPLJSObjectValue.NewUninitialized: TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(nil, ovtUninitialized);
end;

class function TPLJSObjectValue.NewException: TPLJSObjectValue;
begin
  Result := TPLJSObjectValue.Create(nil, ovtException);
end;

function TPLJSObjectValue.AsString: TPLString;
begin
  case FKind of
    ovtNull: Result := 'null';
    ovtUndefined: Result := 'undefined';
    ovtUninitialized: Result := 'uninitialized';
    ovtBoolean: Result := BoolToStr(TPLBool(FVPtr^), 'true', 'false');
    ovtInteger: Result := TPLInt(FVPtr^);
    ovtFloat: Result := TPLFloat(FVPtr^);
    ovtrString: Result := TPLString(FVPtr^);
    //ovtrObject: Result := TPLJSObject(FVPtr).AsString;
    ovtException: Result := 'exception'
    else Result := '';
  end;
end;

function TPLJSObjectValue.AsNumber: TPLFloat;
begin
  Result := 0;
  TryStrToFloat(AsString, Result, PLFormatSettingsDef);
end;

end.

