#pragma once
#include <print>
#include "Location.h"

enum class TokenType
{
    NUMBER,
    IDENTIFIER,
    STRING,

    TRUE,
    FALSE,
    NULLPTR,

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

    ASSIGN,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    COMMA,
    COLON,
    SEMICOLON,

    IF,
    ELSE,
    WHILE,
    FOR,


    RETURN,
    BREAK,
    CONTINUE,

    LET,


    END_FILE
};
struct Token
{
    TokenType type;
    size_t start;
    size_t len;
    Location loc;
};
template<>
struct std::formatter<TokenType>
{
    constexpr auto parse(std::format_parse_context& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(const TokenType& token, FormatContext& ctx) const
    {
        std::string str;
        switch(token)
        {
        case TokenType::NUMBER:
            str = "NUMBER";
            break;
        case TokenType::IDENTIFIER:
            str = "IDENTIFIER";
            break;
        case TokenType::STRING:
            str = "STRING";
            break;
        case TokenType::TRUE:
            str = "TRUE";
            break;
        case TokenType::FALSE:
            str = "FALSE";
            break;
        case TokenType::NULLPTR:
            str = "NULLPTR";
            break;
        case TokenType::PLUS:
            str = "PLUS";
            break;
        case TokenType::MINUS:
            str = "MINUS";
            break;
        case TokenType::MUL:
            str = "MUL";
            break;
        case TokenType::DIV:
            str = "DIV";
            break;
        case TokenType::EQUAL:
            str = "EQUAL";
            break;
        case TokenType::NOT_EQUAL:
            str = "NOT_EQUAL";
            break;
        case TokenType::LT:
            str = "LT";
            break;
        case TokenType::GT:
            str = "GT";
            break;
        case TokenType::LEQ:
            str = "LEQ";
            break;
        case TokenType::GEQ:
            str = "GEQ";
            break;
        case TokenType::NOT:
            str = "NOT";
            break;
        case TokenType::ASSIGN:
            str = "ASSIGN";
            break;
        case TokenType::LPAREN:
            str = "LPAREN";
            break;
        case TokenType::RPAREN:
            str = "RPAREN";
            break;
        case TokenType::LBRACE:
            str = "LBRACE";
            break;
        case TokenType::RBRACE:
            str = "RBRACE";
            break;
        case TokenType::COMMA:
            str = "COMMA";
            break;
        case TokenType::COLON:
            str = "COLON";
            break;
        case TokenType::SEMICOLON:
            str = "SEMICOLON";
            break;
        case TokenType::IF:
            str = "IF";
            break;
        case TokenType::ELSE:
            str = "ELSE";
            break;
        case TokenType::WHILE:
            str = "WHILE";
            break;
        case TokenType::FOR:
            str = "FOR";
            break;
        case TokenType::RETURN:
            str = "RETURN";
            break;
        case TokenType::BREAK:
            str = "BREAK";
            break;
        case TokenType::CONTINUE:
            str = "CONTINUE";
            break;
        case TokenType::LET:
            str = "LET";
            break;
        case TokenType::END_FILE:
            str = "END_FILE";
            break;
        }
        return std::format_to(ctx.out(), "{}", str);
    }
};
