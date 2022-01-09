unit Pospolite.View.JS.AST.Basics;

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
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.JS.Basics;

type

  TPLJSASTNodeType = (
  // General
    ntSuper, ntCatch, ntLiteral, ntTemplateLiteral, ntCase, ntIdentifier,
    ntPrivateIdentifier, ntProgram, ntStatic, ntVariable, ntVariableDeclarator,
    ntArrowParameter, ntMethod,
  // Expressions
    nteAssignment, nteArray, nteAwait, nteBinary, nteCall, nteChain, nteClass,
    nteConditional, nteFunction, nteLogical, nteMember, nteNew, nteObject,
    nteSequence, nteThis, nteUnary, nteUpdate, nteTagged, nteYield, nteArrowFunction,
  // Statements
    ntsBlock, ntsBreak, ntsContinue, ntsDoWhile, ntsDebug, ntsEmpty,
    ntsExpression, ntsFor, ntsForIn, ntsForOf, ntsIf, ntsLabel, ntsReturn,
    ntsSwitch, ntsThrow, ntsTry, ntsWhile, ntsWith,
  // Properties
    ntProperty, ntpDeclaration, ntPropertyDefinition, ntpMeta,
  // Import and Export
    ntImport, ntiDeclaration, ntiSpecifier, ntiDefault, ntiNamespace, ntiExport,
    ntiExportSpecifier, ntiExportNamed, ntiExportAll, ntiExportDefault,
  // Class
    ntcBody, ntcDeclaration,
  // Elements
    ntlTemplate, ntlSpread, ntlRest,
  // Patterns
    ntpObject, ntpAssignment, ntpArray
  );

  { TPLJSASTRange }

  TPLJSASTRange = packed record
  private
    FHighest: TPLJSInt;
    FLowest: TPLJSInt;
  public
    constructor Create(const ALow, AHigh: TPLJSInt);

    class operator =(const a, b: TPLJSASTRange) r: TPLBool; inline;
    class operator :=(const a: TPLJSASTRange) r: TPLString; inline;

    function ToString: TPLString; inline;

    property Lowest: TPLJSInt read FLowest;
    property Highest: TPLJSInt read FHighest;
  end;

  TPLJSASTBaseVisitor = class;
  TPLJSASTNodeList = class;

  { TPLJSASTNode }

  TPLJSASTNode = class abstract(TInterfacedObject)
  private
    FLocation: TPLJSCodeLocation;
    FNodeType: TPLJSASTNodeType;
    FRange: TPLJSASTRange;
  protected
    FChildren: TPLJSASTNodeList;

    procedure Accept(AVisitor: TPLJSASTBaseVisitor); virtual; abstract;
  public
    constructor Create(const AType: TPLJSASTNodeType);
    destructor Destroy; override;

    property NodeType: TPLJSASTNodeType read FNodeType;
    property Range: TPLJSASTRange read FRange write FRange;
    property Location: TPLJSCodeLocation read FLocation write FLocation;
    property Children: TPLJSASTNodeList read FChildren;
  end;

  TPLJSASTNodeClass = class of TPLJSASTNode;
  TPLJSASTNodeArray = class(specialize TPLObjectList<TPLJSASTNode>);
  TPLJSASTFuncsOfNode = specialize TPLFuncsOfClass<TPLJSASTNode>;

  { TPLJSASTNodeList }

  TPLJSASTNodeList = class(TInterfacedObject, specialize IPLSimpleList<TPLJSASTNode>)
  private
    FNodes: array[1..5] of TPLJSASTNode;
    FNodeList1, FNodeList2: array of TPLJSASTNode;
    FCount, FStart: SizeInt;

    function GetItem(AIndex: SizeInt): TPLJSASTNode;
    procedure SetItem({%H-}AIndex: SizeInt; {%H-}AValue: TPLJSASTNode);
  private class var
    FEmpty: TPLJSASTNodeList;
  public
    type IPLJSASTNodeListEnumerator = specialize IEnumerator<TPLJSASTNode>;
    type TPLJSASTNodeListEnumerator = class(specialize TPLListEnumerator<TPLJSASTNodeList, TPLJSASTNode>, IPLJSASTNodeListEnumerator);

    function GetEnumerator: IPLJSASTNodeListEnumerator; reintroduce;
  public
    constructor Create(const A1: TPLJSASTNode = nil; const A2: TPLJSASTNode = nil;
      const A3: TPLJSASTNode = nil; const A4: TPLJSASTNode = nil; const A5: TPLJSASTNode = nil);
    constructor Create(const ACount: SizeInt);
    constructor Create(const A1: TPLJSASTNode);
    constructor Create(const A1, A2: TPLJSASTNode);
    constructor Create(const A1, A2, A3: TPLJSASTNode);
    constructor Create(const A1, A2, A3, A4: TPLJSASTNode);
    constructor Create(const A1: array of TPLJSASTNode);
    constructor Create(const A1, A2: array of TPLJSASTNode);
    constructor Create(const A1: array of TPLJSASTNode; const A2: TPLJSASTNode);
    constructor Create(const A1: TPLJSASTNode; const A2: array of TPLJSASTNode);
    constructor Create(const A1: TPLJSASTNode; const A2: array of TPLJSASTNode;
      const A3: TPLJSASTNode);

    class function Collect(const AFirst: TPLJSASTNodeArray): TPLJSASTNodeList; overload; inline;
    class function Collect(const AFirst, ASecond: TPLJSASTNodeArray): TPLJSASTNodeList; overload; inline;
    class function Collect(const AFirst: TPLJSASTNodeArray;
      const ASecond: TPLJSASTNode): TPLJSASTNodeList; overload; inline;
    class function Collect(const AFirst: TPLJSASTNode;
      const ASecond: TPLJSASTNodeArray): TPLJSASTNodeList; overload; inline;
    class function Collect(const AFirst: TPLJSASTNode;
      const ASecond: TPLJSASTNodeArray;
      const AThird: TPLJSASTNode): TPLJSASTNodeList; overload; inline;

    function Count: SizeInt; inline;

    property Item[AIndex: SizeInt]: TPLJSASTNode read GetItem; default;
    class property Empty: TPLJSASTNodeList read FEmpty;
  end;

  TPLJSASTClassBody = class;
  TPLJSASTIdentifier = class;

  { TPLJSASTBaseVisitor }

  TPLJSASTBaseVisitor = class
  public
    procedure Visit(const ANode: TPLJSASTNode); virtual;
    procedure VisitTyped(const ANode: TPLJSASTNode; const {%H-}AType: TPLJSASTNodeClass); virtual;
  end;

  { TPLJSASTClassBody }

  TPLJSASTClassBody = class sealed(TPLJSASTNode)
  private
    FBody: TPLJSASTNodeArray;
  public
    constructor Create(const ABody: TPLJSASTNodeArray);
    destructor Destroy; override;

    property Body: TPLJSASTNodeArray read FBody;
  end;

  { TPLJSASTStatementListItem }

  TPLJSASTStatementListItem = class abstract(TPLJSASTNode);

  { TPLJSASTExpression }

  TPLJSASTExpression = class abstract(TPLJSASTNode);

  { TPLJSASTIdentifier }

  TPLJSASTIdentifier = class sealed(TPLJSASTNode)
  private
    FName: TPLString;
  protected
    procedure Accept(AVisitor: TPLJSASTBaseVisitor); override;
  public
    constructor Create(const AName: TPLString);
  end;

implementation

{ TPLJSASTRange }

constructor TPLJSASTRange.Create(const ALow, AHigh: TPLJSInt);
begin
  FLowest := ALow;
  FHighest := AHigh;
end;

class operator TPLJSASTRange.=(const a, b: TPLJSASTRange) r: TPLBool;
begin
  r := (a.FLowest = b.FLowest) and (a.FHighest = b.FHighest);
end;

class operator TPLJSASTRange.:=(const a: TPLJSASTRange)r: TPLString;
begin
  r := a.ToString;
end;

function TPLJSASTRange.ToString: TPLString;
begin
  Result := '[%d..%d)'.Format([FLowest, FHighest]);
end;

{ TPLJSASTNode }

constructor TPLJSASTNode.Create(const AType: TPLJSASTNodeType);
begin
  inherited Create;

  FNodeType := AType;
  FChildren := TPLJSASTNodeList.Empty;
end;

destructor TPLJSASTNode.Destroy;
begin
  if Assigned(FChildren) and (FChildren <> TPLJSASTNodeList.Empty) then
    FChildren.Free;

  inherited Destroy;
end;

{ TPLJSASTNodeList }

function TPLJSASTNodeList.GetItem(AIndex: SizeInt): TPLJSASTNode;
begin
  if (AIndex < 0) or (AIndex >= FCount) then exit(nil);
  if AIndex < FStart then exit(FNodes[AIndex - 1]);

  AIndex -= FStart;
  if (AIndex < Length(FNodeList1)) then exit(FNodeList1[AIndex]);
  AIndex -= Length(FNodeList1);
  if (AIndex < Length(FNodeList2)) then exit(FNodeList2[AIndex]);

  Result := FNodes[5];
end;

procedure TPLJSASTNodeList.SetItem(AIndex: SizeInt; AValue: TPLJSASTNode);
begin
  //
end;

function TPLJSASTNodeList.GetEnumerator: IPLJSASTNodeListEnumerator;
begin
  Result := TPLJSASTNodeListEnumerator.Create(Self);
end;

constructor TPLJSASTNodeList.Create(const A1: TPLJSASTNode;
  const A2: TPLJSASTNode; const A3: TPLJSASTNode; const A4: TPLJSASTNode;
  const A5: TPLJSASTNode);
begin
  FCount := 0;
  FStart := 0;

  SetLength(FNodeList1, 0);
  SetLength(FNodeList2, 0);

  FNodes[1] := A1;
  FNodes[2] := A2;
  FNodes[3] := A3;
  FNodes[4] := A4;
  FNodes[5] := A5;
end;

constructor TPLJSASTNodeList.Create(const ACount: SizeInt);
begin
  Create(nil, nil, nil, nil, nil);

  FCount := ACount;
end;

constructor TPLJSASTNodeList.Create(const A1: TPLJSASTNode);
begin
  Create(A1, nil, nil, nil, nil);

  FCount := 1;
  FStart := 1;
end;

constructor TPLJSASTNodeList.Create(const A1, A2: TPLJSASTNode);
begin
  Create(A1, A2, nil, nil, nil);

  FCount := 2;
  FStart := 2;
end;

constructor TPLJSASTNodeList.Create(const A1, A2, A3: TPLJSASTNode);
begin
  Create(A1, A2, A3, nil, nil);

  FCount := 3;
  FStart := 3;
end;

constructor TPLJSASTNodeList.Create(const A1, A2, A3, A4: TPLJSASTNode);
begin
  Create(A1, A2, A3, A4, nil);

  FCount := 4;
  FStart := 4;
end;

constructor TPLJSASTNodeList.Create(const A1: array of TPLJSASTNode);
begin
  Create(nil, nil, nil, nil, nil);

  FNodeList1 := TPLJSASTFuncsOfNode.NewArray(A1);
  FCount := Length(FNodeList1);
end;

constructor TPLJSASTNodeList.Create(const A1, A2: array of TPLJSASTNode);
begin
  Create(nil, nil, nil, nil, nil);

  FNodeList1 := TPLJSASTFuncsOfNode.NewArray(A1);
  FNodeList2 := TPLJSASTFuncsOfNode.NewArray(A2);
  FCount := Length(FNodeList1) + Length(FNodeList2);
end;

constructor TPLJSASTNodeList.Create(const A1: array of TPLJSASTNode;
  const A2: TPLJSASTNode);
begin
  Create(nil, nil, nil, nil, A2);

  FNodeList1 := TPLJSASTFuncsOfNode.NewArray(A1);
  FCount := Length(FNodeList1) + 1;
end;

constructor TPLJSASTNodeList.Create(const A1: TPLJSASTNode;
  const A2: array of TPLJSASTNode);
begin
  Create(A1, nil, nil, nil, nil);

  FNodeList1 := TPLJSASTFuncsOfNode.NewArray(A2);
  FStart := 1;
  FCount := Length(FNodeList1) + 1;
end;

constructor TPLJSASTNodeList.Create(const A1: TPLJSASTNode;
  const A2: array of TPLJSASTNode; const A3: TPLJSASTNode);
begin
  Create(A1, nil, nil, nil, A3);

  FNodeList1 := TPLJSASTFuncsOfNode.NewArray(A2);
  FStart := 1;
  FCount := Length(FNodeList1) + 2;
end;

class function TPLJSASTNodeList.Collect(const AFirst: TPLJSASTNodeArray
  ): TPLJSASTNodeList;
begin
  Result := TPLJSASTNodeList.Create(TPLJSASTFuncsOfNode.Extract(AFirst));
end;

class function TPLJSASTNodeList.Collect(const AFirst, ASecond: TPLJSASTNodeArray
  ): TPLJSASTNodeList;
begin
  Result := TPLJSASTNodeList.Create(TPLJSASTFuncsOfNode.Extract(AFirst),
    TPLJSASTFuncsOfNode.Extract(ASecond));
end;

class function TPLJSASTNodeList.Collect(const AFirst: TPLJSASTNodeArray;
  const ASecond: TPLJSASTNode): TPLJSASTNodeList;
begin
  Result := TPLJSASTNodeList.Create(TPLJSASTFuncsOfNode.Extract(AFirst), ASecond);
end;

class function TPLJSASTNodeList.Collect(const AFirst: TPLJSASTNode;
  const ASecond: TPLJSASTNodeArray): TPLJSASTNodeList;
begin
  Result := TPLJSASTNodeList.Create(AFirst, TPLJSASTFuncsOfNode.Extract(ASecond));
end;

class function TPLJSASTNodeList.Collect(const AFirst: TPLJSASTNode;
  const ASecond: TPLJSASTNodeArray; const AThird: TPLJSASTNode
  ): TPLJSASTNodeList;
begin
  Result := TPLJSASTNodeList.Create(AFirst, TPLJSASTFuncsOfNode.Extract(ASecond), AThird);
end;

function TPLJSASTNodeList.Count: SizeInt;
begin
  Result := FCount;
end;

{ TPLJSASTBaseVisitor }

procedure TPLJSASTBaseVisitor.Visit(const ANode: TPLJSASTNode);
begin
  ANode.Accept(Self);
end;

procedure TPLJSASTBaseVisitor.VisitTyped(const ANode: TPLJSASTNode;
  const AType: TPLJSASTNodeClass);
begin
  Visit(ANode);
end;

{ TPLJSASTClassBody }

constructor TPLJSASTClassBody.Create(const ABody: TPLJSASTNodeArray);
begin
  inherited Create(ntcBody);

  if Assigned(ABody) then FBody := ABody else FBody := TPLJSASTNodeArray.Create(true);
end;

destructor TPLJSASTClassBody.Destroy;
begin
  FBody.Free;

  inherited Destroy;
end;

{ TPLJSASTIdentifier }

procedure TPLJSASTIdentifier.Accept(AVisitor: TPLJSASTBaseVisitor);
begin
  AVisitor.VisitTyped(Self, TPLJSASTIdentifier);
end;

constructor TPLJSASTIdentifier.Create(const AName: TPLString);
begin
  inherited Create(ntIdentifier);

  FName := AName;
end;

initialization
  TPLJSASTNodeList.FEmpty := TPLJSASTNodeList.Create(0);

finalization
  TPLJSASTNodeList.FEmpty.Free;

end.

