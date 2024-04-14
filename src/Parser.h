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

    [[nodiscard]] const AST& GetAST() const
    {
        return m_ast;
    }

private:
    ASTNode* ParseExpression();
    ASTNode* ParseEquality();
    ASTNode* ParseComparison();
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
    AST m_ast;
    size_t m_current = 0;
};
