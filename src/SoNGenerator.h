#pragma once

#include "AST.h"
#include "IR/Node.h"

class SoNGenerator : public VisitorBase
{
public:
    SoNGenerator(AST& ast)
    {
        Visit(ast);
    }

    void Visit(AST& node) override;
    void Visit(UnaryExpression& node) override;
    void Visit(BinaryExpression& node) override;
    void Visit(NumberLiteral& node) override;
    void Visit(StringLiteral& node) override;
    void Visit(BoolLiteral& node) override;
    void Visit(VariableAccess& node) override;
    void Visit(VariableAssignment& node) override;
    void Visit(Block& node) override;
    void Visit(IfElse& node) override;
    void Visit(WhileLoop& node) override;
    void Visit(FnCall& node) override;
    void Visit(ExpressionStatement& node) override;
    void Visit(VariableDeclaration& node) override;
    void Visit(FnDeclaration& node) override;

private:
    StartNode m_startNode;
    StopNode m_stopNode;
    ScopeNode m_scopeNode;

    Node* m_currentNode = nullptr;
};
