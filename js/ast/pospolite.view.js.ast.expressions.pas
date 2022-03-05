unit Pospolite.View.JS.AST.Expressions;

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

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.JS.Basics,
  Pospolite.View.JS.AST.Basics, Pospolite.View.JS.AST.Interfaces;

type

  { TPLJSASTArrayExpression }

  TPLJSASTArrayExpression = class sealed(TPLJSASTExpression)
  private
    FList: TPLJSASTNodeArray;

    function GetElements(const AIndex: SizeInt): TPLJSASTExpression;
    procedure SetElements(const AIndex: SizeInt; AValue: TPLJSASTExpression);
  protected
    procedure Accept(AVisitor: TPLJSASTBaseVisitor); override;
  public
    constructor Create(const AList: TPLJSASTNodeArray);
    destructor Destroy; override;

    property Elements[const AIndex: SizeInt]: TPLJSASTExpression read GetElements write SetElements;
  end;

  { TPLJSASTArrowFunctionExpression }

  TPLJSASTArrowFunctionExpression = class sealed(TPLJSASTExpression, IPLJSASTFunction)
  private
    FParams: TPLJSASTNodeArray;
    FBody: TPLJSASTNode;
    FExpression: TPLBool;
    FStrict: TPLBool;
    FAsync: TPLBool;

    function GetAsync: TPLBool;
    function GetBody: TPLJSASTNode;
    function GetExpression: TPLBool;
    function GetGenerator: TPLBool;
    function GetID: TPLJSASTIdentifier;
    function GetParams(const AIndex: SizeInt): TPLJSASTExpression;
    function GetStrict: TPLBool;
    function GetChildren: TPLJSASTNodeList;
  protected
    procedure Accept(AVisitor: TPLJSASTBaseVisitor); override;
  public
    constructor Create(const AList: TPLJSASTNodeArray; const ABody: TPLJSASTNode;
      const AExpression, AStrict, AAsync: TPLBool);
    destructor Destroy; override;

    property Params[const AIndex: SizeInt]: TPLJSASTExpression read GetParams;
    property Body: TPLJSASTNode read GetBody;
    property Expression: TPLBool read GetExpression;
    property Strict: TPLBool read GetStrict;
    property Generator: TPLBool read GetGenerator;
    property Async: TPLBool read GetAsync;
    property ID: TPLJSASTIdentifier read GetID;
  end;

implementation

{ TPLJSASTArrayExpression }

function TPLJSASTArrayExpression.GetElements(const AIndex: SizeInt
  ): TPLJSASTExpression;
begin
  Result := FList[AIndex] as TPLJSASTExpression;
end;

procedure TPLJSASTArrayExpression.SetElements(const AIndex: SizeInt;
  AValue: TPLJSASTExpression);
begin
  FList[AIndex] := AValue;
end;

procedure TPLJSASTArrayExpression.Accept(AVisitor: TPLJSASTBaseVisitor);
begin
  AVisitor.VisitTyped(Self, TPLJSASTArrayExpression);
end;

constructor TPLJSASTArrayExpression.Create(const AList: TPLJSASTNodeArray);
begin
  inherited Create(nteArray);

  if Assigned(AList) then FList := AList else FList := TPLJSASTNodeArray.Create(true);
  FChildren := TPLJSASTNodeList.Collect(FList);
end;

destructor TPLJSASTArrayExpression.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

{ TPLJSASTArrowFunctionExpression }

function TPLJSASTArrowFunctionExpression.GetAsync: TPLBool;
begin
  Result := FAsync;
end;

function TPLJSASTArrowFunctionExpression.GetBody: TPLJSASTNode;
begin
  Result := FBody;
end;

function TPLJSASTArrowFunctionExpression.GetExpression: TPLBool;
begin
  Result := FExpression;
end;

function TPLJSASTArrowFunctionExpression.GetGenerator: TPLBool;
begin
  Result := false;
end;

function TPLJSASTArrowFunctionExpression.GetID: TPLJSASTIdentifier;
begin
  Result := nil;
end;

function TPLJSASTArrowFunctionExpression.GetParams(const AIndex: SizeInt
  ): TPLJSASTExpression;
begin
  Result := FParams[AIndex] as TPLJSASTExpression;
end;

function TPLJSASTArrowFunctionExpression.GetStrict: TPLBool;
begin
  Result := FStrict;
end;

function TPLJSASTArrowFunctionExpression.GetChildren: TPLJSASTNodeList;
begin
  Result := FChildren;
end;

procedure TPLJSASTArrowFunctionExpression.Accept(AVisitor: TPLJSASTBaseVisitor);
begin
  AVisitor.VisitTyped(Self, TPLJSASTArrowFunctionExpression);
end;

constructor TPLJSASTArrowFunctionExpression.Create(
  const AList: TPLJSASTNodeArray; const ABody: TPLJSASTNode; const AExpression,
  AStrict, AAsync: TPLBool);
begin
  inherited Create(nteArrowFunction);

  FBody := ABody;
  FExpression := AExpression;
  FStrict := AStrict;
  FAsync := AAsync;

  if Assigned(AList) then FParams := AList else FParams := TPLJSASTNodeArray.Create(true);
end;

destructor TPLJSASTArrowFunctionExpression.Destroy;
begin
  FParams.Free;
  if Assigned(FBody) then FBody.Free;

  inherited Destroy;
end;

end.

