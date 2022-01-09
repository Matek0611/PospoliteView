unit Pospolite.View.JS.AST.Interfaces;

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
  Pospolite.View.JS.AST.Basics;

type

  { IPLJSASTInterfaceBase }

  IPLJSASTInterfaceBase = interface
    ['{E78D3A38-E060-47A4-8C78-737860A3CCA4}']

    function GetChildren: TPLJSASTNodeList;
    function GetID: TPLJSASTIdentifier;

    property Children: TPLJSASTNodeList read GetChildren;
    property ID: TPLJSASTIdentifier read GetID;
  end;

  { IPLJSASTClass }

  IPLJSASTClass = interface(IPLJSASTInterfaceBase)
    ['{0A88E531-9C30-4DFF-AFB3-0A6C67E5AFCE}']

    function GetClassBody: TPLJSASTClassBody;
    function GetClassSuper: TPLJSASTExpression;

    property ClassBody: TPLJSASTClassBody read GetClassBody;
    property ClassSuper: TPLJSASTExpression read GetClassSuper;
  end;

  { IPLJSASTFunction }

  IPLJSASTFunction = interface(IPLJSASTInterfaceBase)
    ['{2409CC29-B5C1-4035-9089-F7365B4CDE81}']

    function GetAsync: TPLBool;
    function GetBody: TPLJSASTNode;
    function GetExpression: TPLBool;
    function GetGenerator: TPLBool;
    function GetParams(const AIndex: SizeInt): TPLJSASTExpression;
    function GetStrict: TPLBool;

    property Params[const AIndex: SizeInt]: TPLJSASTExpression read GetParams;
    property Body: TPLJSASTNode read GetBody;
    property Expression: TPLBool read GetExpression;
    property Strict: TPLBool read GetStrict;
    property Generator: TPLBool read GetGenerator;
    property Async: TPLBool read GetAsync;
  end;

implementation

end.

