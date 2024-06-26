#include "Lexer.h"
#include <print>

static const std::unordered_map<std::string_view, TokenType> reservedKeywords = {
    {    "true",     TokenType::TRUE},
    {   "false",    TokenType::FALSE},
    {    "null",  TokenType::NULLPTR},
    {      "if",       TokenType::IF},
    {    "else",     TokenType::ELSE},
    {   "while",    TokenType::WHILE},
    {     "for",      TokenType::FOR},
    {  "return",   TokenType::RETURN},
    {   "break",    TokenType::BREAK},
    {"continue", TokenType::CONTINUE},
    {     "let",      TokenType::LET},
    {     "fun",      TokenType::FUN},
};
void Lexer::Lex()
{
    do
    {
        m_tokens.push_back(Next());
    } while(m_tokens.back().type != TokenType::END_FILE);
}

Token Lexer::Next()
{
    SkipWhiteSpace();

    m_start = m_current;
    m_loc   = {m_filename, m_line, m_col};
    if(IsEnd())
        return MakeToken(TokenType::END_FILE);

    char c = Advance();
    if(IsDigit(c))
        return Number();
    if(IsAlpha(c))
        return Identifier();

    switch(c)
    {
    case '"':
        return String();
    case '+':
        return MakeToken(TokenType::PLUS);
    case '-':
        if(Match('>'))
            return MakeToken(TokenType::ARROW);
        else
            return MakeToken(TokenType::MINUS);
    case '*':
        return MakeToken(TokenType::MUL);
    case '/':
        return MakeToken(TokenType::DIV);
    case '(':
        return MakeToken(TokenType::LPAREN);
    case ')':
        return MakeToken(TokenType::RPAREN);
    case '{':
        return MakeToken(TokenType::LBRACE);
    case '}':
        return MakeToken(TokenType::RBRACE);
    case '[':
        return MakeToken(TokenType::LBRACKET);
    case ']':
        return MakeToken(TokenType::RBRACKET);
    case ',':
        return MakeToken(TokenType::COMMA);
    case ':':
        return MakeToken(TokenType::COLON);
    case ';':
        return MakeToken(TokenType::SEMICOLON);
    case '=':
        return MakeToken(Match('=') ? TokenType::EQUAL : TokenType::ASSIGN);
    case '!':
        return MakeToken(Match('=') ? TokenType::NOT_EQUAL : TokenType::NOT);
    case '<':
        if(Match('<'))
        {
            return MakeToken(TokenType::LSH);
        }
        return MakeToken(Match('=') ? TokenType::LEQ : TokenType::LT);
    case '>':
        if(Match('>'))
        {
            return MakeToken(TokenType::RSH);
        }
        return MakeToken(Match('=') ? TokenType::GEQ : TokenType::GT);
    case '&':
        return MakeToken(Match('&') ? TokenType::LAND : TokenType::BAND);
    case '|':
        return MakeToken(Match('|') ? TokenType::LOR : TokenType::BOR);

    default:
        std::print(stderr, "{}:{} Unexpected character{}: MakeToken()", m_line, m_current, c);
        exit(1);
    }
}

bool Lexer::IsDigit(char c)
{
    return c >= '0' && c <= '9';
}

bool Lexer::IsAlpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}
void Lexer::SkipWhiteSpace()
{
    if(IsEnd())
        return;

    while(true)
    {
        char c = Peek();
        switch(c)
        {
        case ' ':
        case '\t':
        case '\r':
            Advance();
            break;
        case '\n':
            Advance();
            m_line++;
            m_col = 1;
            break;

        case '/':
            if(Peek2() == '/')
            {
                while(!IsEnd() && Peek() != '\n')
                    Advance();
            }
            else
                return;
            break;

        default:
            return;
        }
    }
}

bool Lexer::IsEnd()
{
    return m_current >= m_src.length();
}

char Lexer::Advance()
{
    m_col++;
    return m_src[m_current++];
}

char Lexer::Peek()
{
    if(IsEnd()) [[unlikely]]
        return '\0';

    return m_src[m_current];
}

char Lexer::Peek2()
{
    if(m_current + 1 >= m_src.length()) [[unlikely]]
        return '\0';

    return m_src[m_current + 1];
}

bool Lexer::Match(char c)
{
    if(Peek() == c)
    {
        Advance();
        return true;
    }
    return false;
}

Token Lexer::Number()
{
    while(IsDigit(Peek()))
        Advance();
    return MakeToken(TokenType::NUMBER);
}

Token Lexer::Identifier()
{
    while(IsAlpha(Peek()) || IsDigit(Peek()))
        Advance();

    if(auto it = reservedKeywords.find(m_src.substr(m_start, m_current - m_start)); it != reservedKeywords.end())
        return MakeToken(it->second);

    return MakeToken(TokenType::IDENTIFIER);
}

Token Lexer::String()
{
    m_start = m_current;
    Advance();
    while(Peek() != '"' && !IsEnd())
    {
        Advance();
    }
    if(IsEnd())
    {
        std::print(stderr, "Unexpected end of file, unterminated string literal starting at {}:{}", m_line, m_start);
        exit(1);
    }

    Token token = MakeToken(TokenType::STRING);
    Advance();  // closing quote
    return token;
}
