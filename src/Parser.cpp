#include "Parser.h"
#include <string>
#include <print>

void Parser::Parse()
{
    ASTNode* expr = ParseExpression();

    std::visit<void>(ASTPrinter{}, ASTNode(*expr));
}

ASTNode* Parser::ParseExpression()
{
    return ParseTerm();
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
            std::print("{}:{} Expected ')'", Previous().line, Previous().col);
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
