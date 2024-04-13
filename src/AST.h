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
};

enum class ExpressionKind
{
    BINARY,
    UNARY,
    LITERAL,
    IDENTIFIER,
};

struct BinaryExpression;
struct NumberLiteral;
struct UnaryExpression;

using Expression = std::variant<BinaryExpression, NumberLiteral, UnaryExpression>;

struct UnaryExpression
{
    Op op;
    Expression* expr;

    UnaryExpression(Op op, Expression* expr) : op(op), expr(expr)
    {
    }
};

struct BinaryExpression
{
    Expression* left;
    Op op;
    Expression* right;

    BinaryExpression(Expression* left, Op op, Expression* right) : left(left), op(op), right(right)
    {
    }
};

struct NumberLiteral
{
    int value;

    NumberLiteral(int value) : value(value)
    {
    }
};

class AST
{
public:
    std::vector<Expression*> expressions;
};

using ASTNode = std::variant<BinaryExpression, NumberLiteral, UnaryExpression>;

inline ASTNode* MakeNode(ASTNode&& node)
{
    return new ASTNode(std::move(node));
}

template<typename... Node>
struct NodeVisitor : Node...
{
    using Node::operator()...;
};

template<>
struct std::formatter<Op>
{
    constexpr auto parse(std::format_parse_context& ctx)
    {
        return ctx.begin();
    }

    auto format(Op op, std::format_context& ctx) const
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
struct ASTPrinter
{
    ASTPrinter(uint8_t indent = 0) : m_indent(indent)
    {
    }
    void operator()(const BinaryExpression& expr)
    {
        PrintIndented(m_indent, "BinaryExpression");
        visit(expr.left);
        PrintIndented(m_indent + 1, "Op: {}", expr.op);
        visit(expr.right);
    }
    void operator()(const NumberLiteral& expr)
    {
        PrintIndented(m_indent, "NumberLiteral {} ", expr.value);
    }
    void operator()(const Expression& expr)
    {
        PrintIndented(m_indent, "Expression");
    }

private:
    void visit(ASTNode* node)
    {
        std::visit<void>(ASTPrinter(m_indent + 1), *node);
    }
    uint8_t m_indent;
};
