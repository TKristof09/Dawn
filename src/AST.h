#pragma once
#include "Stack.h"
#include <print>
#include <vector>
#include "Location.h"

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

struct ASTNode
{
    Location loc;

    virtual void Print(int indent) const                                     = 0;
    virtual void GenerateCode(Stack& stack, std::string& buffer, int indent) = 0;
    virtual void GenerateFunctions(Stack& stack, std::string& buffer, int indent) {}
};

struct AST
{
    std::vector<ASTNode*> statements;

    void Print(int indent) const
    {
        for(auto& statement : statements)
            statement->Print(indent);
    }
    void GenerateCode(Stack& stack, std::string& buffer, int indent)
    {
        for(auto& statement : statements)
            statement->GenerateCode(stack, buffer, indent);
    }

    void GenerateFunctions(Stack& stack, std::string& buffer, int indent)
    {
        for(auto& statement : statements)
            statement->GenerateFunctions(stack, buffer, indent);
    }
};

struct Expression : ASTNode
{
};

struct Statement : ASTNode
{
};

struct UnaryExpression : Expression
{
    Op op;
    Expression* expr;

    UnaryExpression(Op op, Expression* expr) : op(op), expr(expr)
    {
    }

    void Print(int indent) const override
    {
        PrintIndented(indent, "Unary expression op: {}", op);
        expr->Print(indent + 1);
    }
    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct BinaryExpression : Expression
{
    Expression* left;
    Op op;
    Expression* right;

    BinaryExpression(Expression* left, Op op, Expression* right) : left(left), op(op), right(right)
    {
    }

    void Print(int indent) const override
    {
        PrintIndented(indent, "BinaryExpression");
        left->Print(indent + 1);
        PrintIndented(indent, "Op: {}", op);
        right->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct NumberLiteral : Expression
{
    int value;

    NumberLiteral(int value) : value(value)
    {
    }

    void Print(int indent) const override
    {
        PrintIndented(indent, "NumberLiteral {} ", value);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
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

    void Print(int indent) const override
    {
        if(index == nullptr)
            PrintIndented(indent, "VariableAccess {}", name);
        else
            PrintIndented(indent, "VariableAccess {}[]", name);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
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

    void Print(int indent) const override
    {
        if(index == nullptr)
            PrintIndented(indent, "VariableAssignment {}", name);
        else
            PrintIndented(indent, "VariableAssignment {}[]", name);

        value->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};


struct Block : Expression
{
    Expression* expr;
    std::vector<Statement*> statements;
    Block(Expression* expr) : expr(expr) {}
    Block(Expression* expr, std::vector<Statement*>&& statements) : expr(expr), statements(std::move(statements)) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "Block");
        for(const auto* statement : statements)
            statement->Print(indent + 1);
        if(expr)
            expr->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};
struct IfElse : Expression
{
    Expression* condition;
    Block* body;
    Block* elseBlock;

    IfElse(Expression* condition, Block* body, Block* elseBlock) : condition(condition), body(body), elseBlock(elseBlock) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "If");
        PrintIndented(indent, "Condition:");
        condition->Print(indent + 1);
        PrintIndented(indent, "Body:");
        body->Print(indent + 1);
        if(elseBlock)
        {
            PrintIndented(indent, "Else Body:");
            elseBlock->Print(indent + 1);
        }
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct WhileLoop : Expression
{
    Expression* condition;
    Block* body;

    WhileLoop(Expression* condition, Block* body) : condition(condition), body(body) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "While loop");
        PrintIndented(indent, "Condition:");
        condition->Print(indent + 1);
        PrintIndented(indent, "Body:");
        body->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct FnCall : Expression
{
    std::string name;
    // TODO: implement multiple arguments
    std::vector<Expression*> arguments;

    FnCall(std::string_view name, std::vector<Expression*>&& args) : name(name), arguments(std::move(args)) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "Function call: {}", name);
        PrintIndented(indent, "Arguments: {}", arguments.size());
        for(const auto* argument : arguments)
            argument->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct ExpressionStatement : Statement
{
    Expression* expr;

    ExpressionStatement(Expression* expr) : expr(expr) {}

    void Print(int indent) const override
    {
        expr->Print(indent);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct VariableDeclaration : Statement
{
    std::string name;
    size_t baseSize;
    size_t arraySize;
    Expression* value;  // nullptr if it's a declaration without initialization

    VariableDeclaration(std::string_view name, size_t baseSize, Expression* value) : name(name), baseSize(baseSize), arraySize(1), value(value) {}
    VariableDeclaration(std::string_view name, size_t baseSize, size_t arraySize, Expression* value) : name(name), baseSize(baseSize), arraySize(arraySize), value(value) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "Variable declaration: {}", name);
        if(value)
            value->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override;
};

struct FnDeclaration : Statement
{
    std::string name;
    std::vector<std::pair<std::string, size_t>> parameters;
    Block* body;

    FnDeclaration(std::string_view name, std::vector<std::pair<std::string, size_t>>&& arguments, Block* body) : name(name), parameters(std::move(arguments)), body(body) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "Function declaration: {}", name);
        for(const auto& [name, size] : parameters)
            PrintIndented(indent + 1, "Argument: {} size: {}", name, size);
        body->Print(indent + 1);
    }

    void GenerateCode(Stack& stack, std::string& buffer, int indent) override {}
    void GenerateFunctions(Stack& stack, std::string& buffer, int indent) override;
};
