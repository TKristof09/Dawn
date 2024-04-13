#pragma once
#include <span>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "Tokens.h"

class Lexer
{
public:
    Lexer(std::string_view src) : m_src(src)
    {
    }
    void Lex();

    [[nodiscard]] std::span<const Token> Tokens() const
    {
        return m_tokens;
    }

private:
    static bool IsDigit(char c);
    static bool IsAlpha(char c);
    void SkipWhiteSpace();
    bool IsEnd();
    char Advance();
    char Peek();
    char Peek2();
    bool Match(char c);

    Token Number();
    Token Identifier();
    Token String();

    Token Next();

    inline Token MakeToken(TokenType type)
    {
        return {type, m_start, m_current - m_start, m_col, m_line};
    }

    std::string_view m_src;
    size_t m_current = 0;
    size_t m_line    = 1;
    size_t m_start   = 0;
    size_t m_col     = 0;

    std::vector<Token> m_tokens;

};
