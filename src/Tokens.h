#pragma once
#include <iostream>
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

    IF,
    ELSE,
    WHILE,
    FOR,

    RETURN,
    BREAK,
    CONTINUE,


    END_FILE
};
struct Token
{
    TokenType type;
    size_t start;
    size_t len;
    size_t col;
    size_t line;
};

inline std::ostream& operator<<(std::ostream& os, const Token& token)
{
    os << token.line << ":" << token.col << ": ";
    switch(token.type)
    {
    case TokenType::NUMBER:
        os << "NUMBER";
        break;
    case TokenType::IDENTIFIER:
        os << "IDENTIFIER";
        break;
    case TokenType::STRING:
        os << "STRING";
        break;
    case TokenType::TRUE:
        os << "TRUE";
        break;
    case TokenType::FALSE:
        os << "FALSE";
        break;
    case TokenType::NULLPTR:
        os << "NULLPTR";
        break;
    case TokenType::PLUS:
        os << "PLUS";
        break;
    case TokenType::MINUS:
        os << "MINUS";
        break;
    case TokenType::MUL:
        os << "MUL";
        break;
    case TokenType::DIV:
        os << "DIV";
        break;
    case TokenType::EQUAL:
        os << "EQUAL";
        break;
    case TokenType::NOT_EQUAL:
        os << "NOT_EQUAL";
        break;
    case TokenType::LT:
        os << "LT";
        break;
    case TokenType::GT:
        os << "GT";
        break;
    case TokenType::LEQ:
        os << "LEQ";
        break;
    case TokenType::GEQ:
        os << "GEQ";
        break;
    case TokenType::NOT:
        os << "NOT";
        break;
    case TokenType::ASSIGN:
        os << "ASSIGN";
        break;
    case TokenType::LPAREN:
        os << "LPAREN";
        break;
    case TokenType::RPAREN:
        os << "RPAREN";
        break;
    case TokenType::IF:
        os << "IF";
        break;
    case TokenType::ELSE:
        os << "ELSE";
        break;
    case TokenType::WHILE:
        os << "WHILE";
        break;
    case TokenType::FOR:
        os << "FOR";
        break;
    case TokenType::RETURN:
        os << "RETURN";
        break;
    case TokenType::BREAK:
        os << "BREAK";
        break;
    case TokenType::CONTINUE:
        os << "CONTINUE";
        break;
    case TokenType::END_FILE:
        os << "END_FILE";
        break;
    }
    return os;
}
