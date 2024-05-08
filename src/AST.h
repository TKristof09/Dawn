#pragma once
#include "Stack.h"
#include <cassert>
#include <print>
#include <vector>
#include "Location.h"
#include "Type.h"

enum class Op
{
    U_MINUS,

    PLUS,
    MINUS,
    MUL,
    DIV,

    LSH,
    RSH,
    BAND,
    BOR,

    EQUAL,
    NOT_EQUAL,
    LT,
    GT,
    LEQ,
    GEQ,
    NOT,
};

template<>
struct std::formatter<Op>
{
    constexpr auto parse(std::format_parse_context& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(Op op, FormatContext& ctx) const
    {
        std::string str;
        switch(op)
        {
        case Op::U_MINUS:
            str = "U_MINUS";
            break;
        case Op::PLUS:
            str = "PLUS";
            break;
        case Op::MINUS:
            str = "MINUS";
            break;
        case Op::MUL:
            str = "MUL";
            break;
        case Op::DIV:
            str = "DIV";
            break;
        case Op::LSH:
            str = "LSH";
            break;
        case Op::RSH:
            str = "RSH";
            break;
        case Op::BAND:
            str = "BAND";
            break;
        case Op::BOR:
            str = "BOR";
            break;
        case Op::EQUAL:
            str = "EQUAL";
            break;
        case Op::NOT_EQUAL:
            str = "NOT_EQUAL";
            break;
        case Op::LT:
            str = "LT";
            break;
        case Op::GT:
            str = "GT";
            break;
        case Op::LEQ:
            str = "LEQ";
            break;
        case Op::GEQ:
            str = "GEQ";
            break;
        case Op::NOT:
            str = "NOT";
            break;
        }
        return std::format_to(ctx.out(), "{}", str);
    }
};

template<typename... Args>
void PrintIndented(int indent, std::format_string<Args...> format_str, Args&&... args)
{
    std::print("{:{}}|", "", indent * 4);
    std::println(format_str, std::forward<Args>(args)...);
}


struct UnaryExpression;
struct BinaryExpression;
struct NumberLiteral;
struct StringLiteral;
struct BoolLiteral;
struct VariableAccess;
struct VariableAssignment;
struct Block;
struct IfElse;
struct WhileLoop;
struct FnCall;
struct ExpressionStatement;
struct VariableDeclaration;
struct FnDeclaration;

struct AST;


class VisitorBase
{
public:
    virtual void Visit(AST& node)                 = 0;
    virtual void Visit(UnaryExpression& node)     = 0;
    virtual void Visit(BinaryExpression& node)    = 0;
    virtual void Visit(NumberLiteral& node)       = 0;
    virtual void Visit(StringLiteral& node)       = 0;
    virtual void Visit(BoolLiteral& node)         = 0;
    virtual void Visit(VariableAccess& node)      = 0;
    virtual void Visit(VariableAssignment& node)  = 0;
    virtual void Visit(Block& node)               = 0;
    virtual void Visit(IfElse& node)              = 0;
    virtual void Visit(WhileLoop& node)           = 0;
    virtual void Visit(FnCall& node)              = 0;
    virtual void Visit(ExpressionStatement& node) = 0;
    virtual void Visit(VariableDeclaration& node) = 0;
    virtual void Visit(FnDeclaration& node)       = 0;
};


struct ASTNode
{
    Location loc;

    virtual void Accept(VisitorBase* visitor) = 0;
};

struct AST
{
    std::vector<ASTNode*> statements;

    void Visit(VisitorBase* visitor)
    {
        visitor->Visit(*this);
    }
};

struct Expression : ASTNode
{
    void Accept(VisitorBase* visitor) override
    {
        std::println("Accept not implemented for Expression");
        assert(false);
    };
};

struct Statement : ASTNode
{
    void Accept(VisitorBase* visitor) override
    {
        std::println("Accept not implemented for Statement");
        assert(false);
    };
};

struct UnaryExpression : Expression
{
    Op op;
    Expression* expr;

    UnaryExpression(Op op, Expression* expr) : op(op), expr(expr)
    {
    }
    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct BinaryExpression : Expression
{
    Expression* left;
    Op op;
    Expression* right;

    BinaryExpression(Expression* left, Op op, Expression* right) : left(left), op(op), right(right)
    {
    }


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct NumberLiteral : Expression
{
    int value;

    NumberLiteral(int value) : value(value)
    {
    }


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct StringLiteral : Expression
{
    std::string_view value;

    StringLiteral(std::string_view value) : value(value)
    {
    }

    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct BoolLiteral : Expression
{
    bool value;

    BoolLiteral(bool value) : value(value)
    {
    }

    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct VariableAccess : Expression
{
    std::string name;
    Expression* index = nullptr;

    VariableAccess(std::string_view name) : name(name)
    {
    }

    VariableAccess(std::string_view name, Expression* index) : name(name), index(index)
    {
    }


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct VariableAssignment : Expression
{
    std::string name;
    Expression* index = nullptr;
    Expression* value;

    VariableAssignment(std::string_view name, Expression* value) : name(name), value(value)
    {
    }

    VariableAssignment(std::string_view name, Expression* index, Expression* value) : name(name), index(index), value(value)
    {
    }


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};


struct Block : Expression
{
    Expression* expr;
    std::vector<Statement*> statements;
    Block(Expression* expr) : expr(expr) {}
    Block(Expression* expr, std::vector<Statement*>&& statements) : expr(expr), statements(std::move(statements)) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};
struct IfElse : Expression
{
    Expression* condition;
    Block* body;
    Block* elseBlock;

    IfElse(Expression* condition, Block* body, Block* elseBlock) : condition(condition), body(body), elseBlock(elseBlock) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct WhileLoop : Expression
{
    Expression* condition;
    Block* body;

    WhileLoop(Expression* condition, Block* body) : condition(condition), body(body) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct FnCall : Expression
{
    std::string name;
    // TODO: implement multiple arguments
    std::vector<Expression*> arguments;

    FnCall(std::string_view name, std::vector<Expression*>&& args) : name(name), arguments(std::move(args)) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct ExpressionStatement : Statement
{
    Expression* expr;

    ExpressionStatement(Expression* expr) : expr(expr) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct VariableDeclaration : Statement
{
    std::string name;
    size_t arraySize;
    Type type;
    Expression* value;  // nullptr if it's a declaration without initialization

    VariableDeclaration(std::string_view name, Type t, Expression* value) : name(name), arraySize(1), type(t), value(value) {}
    VariableDeclaration(std::string_view name, Type t, size_t arraySize, Expression* value) : name(name), arraySize(arraySize), type(t), value(value) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};

struct FnDeclaration : Statement
{
    std::string name;
    std::vector<std::string> parameters;
    Block* body;
    Types::Function type;

    FnDeclaration(std::string_view name, Types::Function t, std::vector<std::string>&& arguments, Block* body) : name(name), parameters(std::move(arguments)), body(body), type(t) {}


    void Accept(VisitorBase* visitor) override
    {
        visitor->Visit(*this);
    }
};


struct AstPrinter : VisitorBase
{
    AstPrinter(AST& ast)
    {
        Visit(ast);
    }

    void Visit(AST& node) override
    {
        PrintIndented(m_indent, "-------AST-------");
        for(auto* statement : node.statements)
            statement->Accept(this);
    }
    void Visit(UnaryExpression& node) override
    {
        PrintIndented(m_indent++, "UnaryExpression: {}", node.op);
        node.expr->Accept(this);
        --m_indent;
    }

    void Visit(BinaryExpression& node) override
    {
        PrintIndented(m_indent++, "BinaryExpression: {}", node.op);
        node.left->Accept(this);
        node.right->Accept(this);
        --m_indent;
    }

    void Visit(NumberLiteral& node) override
    {
        PrintIndented(m_indent, "NumberLiteral: {}", node.value);
    }
    void Visit(StringLiteral& node) override
    {
        PrintIndented(m_indent, "StringLiteral: {}", node.value);
    }

    void Visit(BoolLiteral& node) override
    {
        PrintIndented(m_indent, "BoolLiteral: {}", node.value);
    }

    void Visit(VariableAccess& node) override
    {
        PrintIndented(m_indent, "VariableAccess: {}", node.name);
        if(node.index)
        {
            node.index->Accept(this);
        }
    }

    void Visit(VariableAssignment& node) override
    {
        PrintIndented(m_indent++, "VariableAssignment: {}", node.name);
        if(node.index)
        {
            node.index->Accept(this);
        }
        node.value->Accept(this);
        --m_indent;
    }

    void Visit(Block& node) override
    {
        PrintIndented(m_indent++, "Block");
        if(node.expr)
            node.expr->Accept(this);
        for(auto* statement : node.statements)
            statement->Accept(this);
        --m_indent;
    }

    void Visit(IfElse& node) override
    {
        PrintIndented(m_indent++, "IfElse");
        node.condition->Accept(this);
        node.body->Accept(this);
        if(node.elseBlock)
            node.elseBlock->Accept(this);
        --m_indent;
    }

    void Visit(WhileLoop& node) override
    {
        PrintIndented(m_indent++, "WhileLoop");
        node.condition->Accept(this);
        node.body->Accept(this);
        --m_indent;
    }

    void Visit(FnCall& node) override
    {
        PrintIndented(m_indent++, "FnCall: {}", node.name);
        for(auto* arg : node.arguments)
            arg->Accept(this);
        --m_indent;
    }

    void Visit(ExpressionStatement& node) override
    {
        PrintIndented(m_indent++, "ExpressionStatement");
        node.expr->Accept(this);
        --m_indent;
    }

    void Visit(VariableDeclaration& node) override
    {
        PrintIndented(m_indent++, "VariableDeclaration: {}", node.name);
        if(node.value)
            node.value->Accept(this);
        --m_indent;
    }

    void Visit(FnDeclaration& node) override
    {
        PrintIndented(m_indent++, "FnDeclaration: {}", node.name);
        for(auto& name : node.parameters)
            PrintIndented(m_indent, "Parameter: {} ", name);
        node.body->Accept(this);
        --m_indent;
    }


private:
    int m_indent = 0;
};
