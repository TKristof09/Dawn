#pragma once

#include "AST.h"
#include "Tokens.h"
#include <span>
class Parser
{
public:
    Parser(std::string_view src, std::span<const Token> tokens)
        : m_tokens(tokens), m_src(src)
    {
    }
    void Parse();

private:
    ASTNode* ParseExpression();
    ASTNode* ParseTerm();
    ASTNode* ParseFactor();
    ASTNode* ParseUnary();
    ASTNode* ParsePrimary();

    bool IsEnd();
    Token Peek();
    Token Advance();
    bool Match(TokenType type);
    Token Previous();


    std::string_view m_src;
    std::span<const Token> m_tokens;
    size_t m_current = 0;
};
