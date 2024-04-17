#include "Parser.h"
#include <string>
#include <print>

void Parser::Parse()
{
    while(!IsEnd())
    {
        ASTNode* expr = ParseExpression();
        if(!expr)
        {
            std::println(stderr, "Skipping unexpected token {} at {}:{}", Peek().type, Peek().line, Peek().col);
            Advance();
            continue;
        }
        m_ast.expressions.push_back(expr);
    }


    for(auto* expr : m_ast.expressions)
        std::visit<void>(ASTPrinter{}, ASTNode(*expr));
}

ASTNode* Parser::ParseExpression()
{
    ASTNode* expr = ParseEquality();
    if(expr)
        return expr;

    expr = ParseIf();
    if(expr)
        return expr;

    expr = ParseBlock();
    if(expr)
        return expr;

    return nullptr;
}

ASTNode* Parser::ParseIf()
{
    if(Match(TokenType::IF))
    {
        ASTNode* condition = ParseExpression();
        if(!condition)
        {
            std::println(stderr, "{}:{}: Expected expression for if condition", Peek().line, Peek().col);
            exit(1);
        }

        ASTNode* body = ParseBlock();
        if(!body)
        {
            std::println(stderr, "{}:{}: Expected block expression for if body", Peek().line, Peek().col);
            exit(1);
        }
        ASTNode* elseBlock = nullptr;
        if(Match(TokenType::ELSE))
        {
            elseBlock = ParseBlock();

            if(!elseBlock)
            {
                std::println(stderr, "{}:{}: Expected block expression for else body", Peek().line, Peek().col);
                exit(1);
            }
        }
        return MakeNode(If{condition, body, elseBlock});
    }
    return nullptr;
}

ASTNode* Parser::ParseBlock()
{
    if(Match(TokenType::LBRACE))
    {
        ASTNode* expr = ParseExpression();
        if(!Match(TokenType::RBRACE))
        {
            std::println(stderr, "{}:{}: Expected closing brace for block expression", Peek().line, Peek().col);
            exit(1);
        }
        return MakeNode(Block(expr));
    }
    return nullptr;
}

ASTNode* Parser::ParseEquality()
{
    ASTNode* expr = ParseComparison();

    while(Match(TokenType::EQUAL) || Match(TokenType::NOT_EQUAL))
    {
        Op op          = Previous().type == TokenType::EQUAL ? Op::EQUAL : Op::NOT_EQUAL;
        ASTNode* right = ParseComparison();
        expr           = MakeNode(BinaryExpression{expr, op, right});
    }

    return expr;
}

ASTNode* Parser::ParseComparison()
{
    ASTNode* expr = ParseTerm();

    while(Match(TokenType::LT) || Match(TokenType::GT) || Match(TokenType::LEQ) || Match(TokenType::GEQ))
    {
        Op op = Op::LT;
        switch(Previous().type)
        {
        case TokenType::LT:
            op = Op::LT;
            break;
        case TokenType::GT:
            op = Op::GT;
            break;
        case TokenType::LEQ:
            op = Op::LEQ;
            break;
        case TokenType::GEQ:
            op = Op::GEQ;
            break;
        }

        ASTNode* right = ParseTerm();
        expr           = MakeNode(BinaryExpression{expr, op, right});
    }

    return expr;
}

ASTNode* Parser::ParseTerm()
{
    ASTNode* expr = ParseFactor();

    while(Match(TokenType::MINUS) || Match(TokenType::PLUS))
    {
        Op op          = Previous().type == TokenType::MINUS ? Op::MINUS : Op::PLUS;
        ASTNode* right = ParseFactor();
        expr           = MakeNode(BinaryExpression{expr, op, right});
    }

    return expr;
}

ASTNode* Parser::ParseFactor()
{
    ASTNode* expr = ParseUnary();

    while(Match(TokenType::MUL) || Match(TokenType::DIV))
    {
        Op op          = Previous().type == TokenType::MUL ? Op::MUL : Op::DIV;
        ASTNode* right = ParseUnary();
        expr           = MakeNode(BinaryExpression{expr, op, right});
    }

    return expr;
}

ASTNode* Parser::ParseUnary()
{
    if(Match(TokenType::MINUS))
    {
        ASTNode* expr = ParsePrimary();
        return MakeNode(UnaryExpression{Op::U_MINUS, expr});
    }
    if(Match(TokenType::NOT))
    {
        ASTNode* expr = ParsePrimary();
        return MakeNode(UnaryExpression{Op::NOT, expr});
    }

    return ParsePrimary();
}

ASTNode* Parser::ParsePrimary()
{
    if(Match(TokenType::NUMBER))
    {
        Token token = Previous();
        return MakeNode(NumberLiteral{std::stoi(std::string(m_src.substr(token.start, token.len)))});
    }

    if(Match(TokenType::LPAREN))
    {
        ASTNode* expr = ParseExpression();
        if(!Match(TokenType::RPAREN))
        {
            std::println(stderr, "{}:{} Expected ')'", Previous().line, Previous().col);
            exit(1);
        }
        return expr;
    }

    return nullptr;
}

bool Parser::IsEnd()
{
    return Peek().type == TokenType::END_FILE;
}

Token Parser::Peek()
{
    return m_tokens[m_current];
}
Token Parser::Advance()
{
    if(!IsEnd())
        m_current++;
    return Previous();
}

Token Parser::Previous()
{
    return m_tokens[m_current - 1];
}

bool Parser::Match(TokenType type)
{
    if(Peek().type == type)
    {
        Advance();
        return true;
    }
    return false;
}
