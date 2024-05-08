#pragma once

#include "AST.h"
#include "Type.h"

class TypeChecker : public VisitorBase
{
public:
    TypeChecker(AST& ast)
    {
        m_stack.Push("print", Types::Function{{Types::Int()}, {Types::NoneType()}});
        m_stack.Push("prints", Types::Function{{Types::String()}, {Types::NoneType()}});
        Visit(ast);

        if(m_error)
        {
            std::exit(1);
        }
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
    bool m_findFunctions = false;
    Type m_currentType;
    TypeStack m_stack;

    bool m_error = false;
};
