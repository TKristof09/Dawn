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

    [[nodiscard]] AST& GetAST()
    {
        return m_ast;
    }

private:
    ASTNode* ParseExpression();
    ASTNode* ParseStatement(bool reportSemicolonError = true);

    ASTNode* ParseWhile();
    ASTNode* ParseIf();
    ASTNode* ParseBlock();

    ASTNode* ParseBitwiseAnd();
    ASTNode* ParseBitwiseOr();
    ASTNode* ParseEquality();
    ASTNode* ParseComparison();
    ASTNode* ParseShift();
    ASTNode* ParseTerm();
    ASTNode* ParseFactor();

    ASTNode* ParseUnary();
    ASTNode* ParseFnCall();
    ASTNode* ParsePrimary();

    ASTNode* ParseAssignment();

    ASTNode* ParseExpressionStatement(bool reportSemicolonError = true);
    ASTNode* ParseVariableDeclaration();
    ASTNode* ParseFnDeclaration();

    size_t ParseTypeSize(Type t);
    Type ParseType();
    Expression* ParseIndex();
    size_t ParseArraySize();

    bool IsEnd();
    Token Peek();
    Token Peek2();
    Token Advance();
    bool Match(TokenType type);
    Token Previous();

    template<typename T, typename... Args>
    T* MakeNode(Location loc, Args&&... args)
        requires std::constructible_from<T, Args...>
    {
        T* res   = new T{std::forward<Args>(args)...};
        res->loc = loc;
        return res;
    }

    std::string_view m_src;
    std::span<const Token> m_tokens;
    AST m_ast;
    size_t m_current = 0;
};
