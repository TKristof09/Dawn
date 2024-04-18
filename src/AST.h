#pragma once
#include <print>
#include <variant>
#include <vector>

enum class Op
{
    U_MINUS,

    PLUS,
    MINUS,
    MUL,
    DIV,

    EQUAL,
    NOT_EQUAL,
    LT,
    GT,
    LEQ,
    GEQ,
    NOT,

    CALL,
};

enum class ExpressionKind
{
    BINARY,
    UNARY,
    LITERAL,
    IDENTIFIER,
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
        case Op::CALL:
            str = "CALL";
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
    virtual void Print(int indent) const                       = 0;
    virtual void GenerateCode(std::string& buffer, int indent) = 0;
};

struct AST
{
    std::vector<ASTNode*> expressions;

    void Print(int indent) const
    {
        for(auto& expr : expressions)
            expr->Print(indent);
    }
    void GenerateCode(std::string& buffer, int indent)
    {
        for(auto& expr : expressions)
            expr->GenerateCode(buffer, indent);
    }
};

struct Expression : ASTNode
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
    void GenerateCode(std::string& buffer, int indent) override;
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

    void GenerateCode(std::string& buffer, int indent) override;
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

    void GenerateCode(std::string& buffer, int indent) override;
};


struct Block : Expression
{
    Expression* body;  // TODO: this is temporary, in the fufture blocks will be able to have more than one expression in them
    Block(Expression* body) : body(body) {}

    void Print(int indent) const override
    {
        PrintIndented(indent, "Block");
        body->Print(indent + 1);
    }

    void GenerateCode(std::string& buffer, int indent) override;
};
struct If : Expression
{
    Expression* condition;
    Block* body;
    Block* elseBlock;

    If(Expression* condition, Block* body, Block* elseBlock) : condition(condition), body(body), elseBlock(elseBlock) {}

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

    void GenerateCode(std::string& buffer, int indent) override;
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

    void GenerateCode(std::string& buffer, int indent) override;
};
