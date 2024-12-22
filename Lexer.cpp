#include <vector>
#include "include/Lexer.h"

void Lexer::logError(std::string error)
{
    std::cerr << error << "\n"; 
}

bool Lexer::isEndOfText()
{
    return current >= sourceCode.length();
}

char Lexer::currentCharacter()
{
    if (isEndOfText())
        return '\0';
    return sourceCode.at(current);
}

char Lexer::nextCharacter()
{
    if (current + 1 >= sourceCode.length())
        return '\0';
    return sourceCode.at(current + 1);
}

char Lexer::getCharacter(){
    return sourceCode.at(current++);
}

void Lexer::matchEqual(char c)
{
    if (currentCharacter() == '=')
    {
        switch (c)
        {
        case '=':
            tokenList.push_back(Token(EQUAL_EQUAL, "=="));
            break;
        case '>':
            tokenList.push_back(Token(GREATER_EQUAL, ">="));

            break;
        case '<':
            tokenList.push_back(Token(LESS_EQUAL, "<="));
            break;
        case '!':
            tokenList.push_back(Token(NOT_EQUAL, "!="));
            break;
        default:
            break;
        }
        getCharacter();
    }
    else
    {
        switch (c)
        {
        case '=':
            tokenList.push_back(Token(EQUAL, "="));
            break;
        case '>':
            tokenList.push_back(Token(GREATER, ">"));
            break;
        case '<':
            tokenList.push_back(Token(LESS, "<"));
            break;
        case '!':
            tokenList.push_back(Token(NOT, "!"));
            break;
        default:
            break;
        }
    }
}

void Lexer::scanChar()
{
    char value = getCharacter();
    if (currentCharacter() != '\''){
        std::cerr << "missing terminating \' character";
    }
    getCharacter(); // Consume next '
    tokenList.push_back(Token(CHARACTER, std::string(1, value)));
}

void Lexer::scanString()
{
    while (currentCharacter() != '"' && !isEndOfText())
    {
        if (currentCharacter() == '\n'){
            std::cerr << "missing terminating \" character";
        }
        getCharacter();
    }

    if (isEndOfText()) 
    {
        std::cerr << "missing terminating \" character";
    }

    getCharacter(); // Consume next "

    std::string value = sourceCode.substr(start + 1, current - start - 2);
    tokenList.push_back(Token(STRING, value));
}

void Lexer::scanNumber()
{
    bool isFloat = false;
    while (isdigit(currentCharacter()))
        getCharacter();

    if (currentCharacter() == '.' && isdigit(nextCharacter()))
    {
        isFloat = true;
        getCharacter();
        while (isdigit(currentCharacter()))
            getCharacter();
    }
    TokenType type = NUMBER_INT;
    if (isFloat){
        type = NUMBER_FLOAT;
    }
    std::string value = sourceCode.substr(start, current - start);
    tokenList.push_back(Token(type, value));
}

void Lexer::scanIdentifiersAndKeywords()
{
    while ((isdigit(currentCharacter()) || isalpha(currentCharacter()) || currentCharacter() == '_') && !isEndOfText())
        getCharacter();

    std::string value = sourceCode.substr(start, current - start);
    if (keywords.find(value) != keywords.end())
    {
        tokenList.push_back(Token(keywords.at(value), value));
    }
    else
    {
        tokenList.push_back(Token(IDENTIFIER, value));
    }
}

void Lexer::scanToken()
{
    char c = getCharacter();
    switch (c)
    {
    case '(':
        tokenList.push_back(Token(LEFT_PAREN, "("));
        break;
    case ')':
        tokenList.push_back(Token(RIGHT_PAREN, ")"));
        break;
    case '[':
        tokenList.push_back(Token(LEFT_SQUARE, "["));
        break;
    case ']':
        tokenList.push_back(Token(RIGHT_SQUARE, "]"));
        break;
    case '{':
        tokenList.push_back(Token(LEFT_BRACE, "{"));
        break;
    case '}':
        tokenList.push_back(Token(RIGHT_BRACE, "}"));
        break;
    case ',':
        tokenList.push_back(Token(COMMA, ","));
        break;
    case '.':
        tokenList.push_back(Token(DOT, "."));
        break;
    case ';':
        tokenList.push_back(Token(SEMICOLON, ";"));
        break;
    case ':':
        tokenList.push_back(Token(COLON, ":"));
        break;
    case '+':
        tokenList.push_back(Token(PLUS, "+"));
        break;
    case '-':
        if (currentCharacter() == '>')
        {
            tokenList.push_back(Token(RETURN_TYPE, "->"));
            getCharacter();
        }
        else
        {
            tokenList.push_back(Token(MINUS, "-"));
        }
        break;
    case '*':
        tokenList.push_back(Token(MULTIPLY, "*"));
        break;
    case '/':
        tokenList.push_back(Token(DIVIDE, "/"));
        break;
    case '%':
        tokenList.push_back(Token(MODULO, "%"));
        break;
    case '=':
    case '>':
    case '<':
    case '!':
        matchEqual(c);
        break;
    case ' ':
    case '\t':
        break;
    case '\n':
        break;
    case '"':
        scanString();
        break;
    case '\'':
        scanChar();
        break;
    case '&':
        if (currentCharacter() == '&')
        {
            tokenList.push_back(Token(LOGICAL_AND, "&&"));
            getCharacter();
        }
        else
        {
            tokenList.push_back(Token(BITWISE_AND, "&"));
        }
        break;
    case '|':
        if (currentCharacter() == '|')
        {
            tokenList.push_back(Token(LOGICAL_OR, "||"));
            getCharacter();
        }
        else
        {
            tokenList.push_back(Token(BITWISE_OR, "|"));
        }
        break;
    case '^':
        tokenList.push_back(Token(BITWISE_XOR, "^"));
        break;
    default:
        if (isdigit(c))
        {
            scanNumber();
        }
        else if (isalpha(c) || c == '_')
        {
            scanIdentifiersAndKeywords();
        }
        break;
    }
}

Lexer::Lexer(const std::string& sourceCode)
{
    this->sourceCode = sourceCode;
}

std::vector<Token> Lexer::getTokenList(){
    return tokenList;
}

void Lexer::scanSourceCode()
{
    while (!isEndOfText())
    {
        start = current;
        scanToken();
    }
    tokenList.push_back(Token(EOF_TOKEN, ""));
}
