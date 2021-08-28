unit PospoliteHTML.ECMAScript5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, BESEN, BESENValue, BESENNumberUtils,
  BESENErrors, BESENObject, BESENConstants;

type

  { TPLJSInterpreter }

  TPLJSInterpreter = class(TPersistent)
  private
    FCustomAlert: TBESENNativeFunction;
    FLastError: string;
    FSourceCode: string;
    FInit: boolean;
  protected
    FInstance: TBESEN;
    procedure NativeAlert(const ThisArgument: TBESENValue;
      Arguments: PPBESENValues; CountArguments: integer; var ResultValue: TBESENValue);
    procedure Init;
  public
    constructor Create;
    destructor Destroy; override;

    function Run: boolean;

    property Instance: TBESEN read FInstance write FInstance;
    property SourceCode: string read FSourceCode write FSourceCode;
    property LastError: string read FLastError;
    property CustomAlert: TBESENNativeFunction write FCustomAlert;
  end;

implementation

{ TPLJSInterpreter }

procedure TPLJSInterpreter.NativeAlert(const ThisArgument: TBESENValue;
  Arguments: PPBESENValues; CountArguments: integer; var ResultValue: TBESENValue);
var
  i: integer;
  v: PBESENValue;
  fOutput: WideString;

  procedure writeit(s: WideString);
  begin
    fOutput := fOutput + s;
  end;

begin
  fOutput := '';
  ResultValue.ValueType := bvtUNDEFINED;
  for i := 0 to CountArguments - 1 do
  begin
    v := Arguments^[i];
    case v^.ValueType of
      bvtUNDEFINED:
      begin
        writeit('undefined');
      end;
      bvtNULL:
      begin
        writeit('null');
      end;
      bvtBOOLEAN:
      begin
        if v^.Bool then
        begin
          writeit('true');
        end
        else
        begin
          writeit('false');
        end;
      end;
      bvtNUMBER:
      begin
        writeit(BESENFloatToStr(v^.Num));
      end;
      bvtSTRING:
      begin
        writeit(v^.Str);
      end;
      bvtOBJECT:
      begin
        writeit(TBESEN(FInstance).ToStr(v^));
      end;
      bvtREFERENCE:
      begin
        writeit('reference');
      end;
    end;
  end;
  ShowMessage(fOutput);
end;

constructor TPLJSInterpreter.Create;
begin
  inherited Create;

  FInstance := TBESEN.Create(COMPAT_JS or COMPAT_SGMLCOM);
  FSourceCode := '';
  FInit := false;
end;

destructor TPLJSInterpreter.Destroy;
begin
  FInstance.Free;

  inherited Destroy;
end;

procedure TPLJSInterpreter.Init;
begin
  if FInit then exit;

  if FCustomAlert = nil then FCustomAlert := @NativeAlert;
  TBESEN(FInstance).ObjectGlobal.RegisterNativeFunction('alert', FCustomAlert, 1, []);

  FInit := true;
end;

function TPLJSInterpreter.Run: boolean;
begin
  Init;

  FLastError := '';
  Result := true;
  try
    TBESEN(FInstance).Execute(FSourceCode);
  except
    on e: Exception do begin
      FLastError := e.Message;
      Result := false;
    end;
    on e: EBESENError do begin
      FLastError := e.Name + ': ' + e.Message;
      Result := false;
    end;
  end;
end;

end.
