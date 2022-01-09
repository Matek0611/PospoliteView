unit Pospolite.View.JS.AST.Visitors;

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

  { TPLJSASTNormalVisitor }

  TPLJSASTNormalVisitor = class(TPLJSASTBaseVisitor)
  public
    procedure VisitTyped(const ANode: TPLJSASTNode;
      const AType: TPLJSASTNodeClass); override;
  end;

implementation

{ TPLJSASTNormalVisitor }

procedure TPLJSASTNormalVisitor.VisitTyped(const ANode: TPLJSASTNode;
  const AType: TPLJSASTNodeClass);
begin
  inherited VisitTyped(ANode, AType);
end;

end.

