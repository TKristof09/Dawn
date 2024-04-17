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
struct If;
struct Block;

using Expression = std::variant<BinaryExpression, NumberLiteral, UnaryExpression, If, Block>;


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


struct If
{
    Expression* condition;
    Expression* body;
    Expression* elseBlock;

    If(Expression* condition, Expression* body, Expression* elseBlock) : condition(condition), body(body), elseBlock(elseBlock) {}
};

struct Block
{
    Expression* body;  // TODO: this is temporary, in the fufture blocks will be able to have more than one expression in them
    Block(Expression* body) : body(body) {}
};

// TODO: figure out a better way for this and the Expression type
using ASTNode = std::variant<BinaryExpression, NumberLiteral, UnaryExpression, If, Block>;

template<typename T>
concept is_ast_node = std::constructible_from<ASTNode, T>;

template<typename T>
ASTNode* MakeNode(T&& node)
    requires(is_ast_node<T>)
{
    return new ASTNode(std::move(node));
}
class AST
{
public:
    std::vector<ASTNode*> expressions;
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
        PrintIndented(m_indent, "Op: {}", expr.op);
        visit(expr.right);
    }
    void operator()(const UnaryExpression& expr)
    {
        PrintIndented(m_indent, "Unary expression op: {}", expr.op);
        visit(expr.expr);
    }
    void operator()(const NumberLiteral& expr)
    {
        PrintIndented(m_indent, "NumberLiteral {} ", expr.value);
    }
    void operator()(const Expression& expr)
    {
        PrintIndented(m_indent, "Unknown expression type");
    }
    void operator()(const If& expr)
    {
        PrintIndented(m_indent, "If");
        PrintIndented(m_indent, "Condition:");
        visit(expr.condition);
        PrintIndented(m_indent, "Body:");
        visit(expr.body);
        if(expr.elseBlock)
        {
            PrintIndented(m_indent, "Else Body:");
            visit(expr.elseBlock);
        }
    }
    void operator()(const Block& expr)
    {
        PrintIndented(m_indent, "Block");
        visit(expr.body);
    }

private:
    void visit(ASTNode* node)
    {
        std::visit<void>(ASTPrinter(m_indent + 1), *node);
    }
    uint8_t m_indent;
};
