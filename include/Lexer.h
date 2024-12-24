#pragma once

#include "Token.hpp"
#include <vector>

class Lexer
{
    std::string sourceCode;
    std::vector<Token> tokenList;
    int start = 0;
    int current = 0;
    std::map<std::string, TokenType> keywords = {
        {"int", INT},
        {"bigint", BIGINT},
        {"float", FLOAT},
        {"string", STR},
        {"char", CHAR},
        {"bool", BOOL},
        {"true", TRUE},
        {"false", FALSE},
        {"void", VOID},
        {"array", ARRAY},
        {"if", IF},
        {"else", ELSE},
        {"return", RETURN},
        {"for", FOR},
        {"while", WHILE},
        {"function", FUNCTION},
        {"print", PRINT}
    };
    void logError(std::string error);
    bool isEndOfText();
    char currentCharacter();
    char nextCharacter();
    char getCharacter();
    void matchEqual(char c);
    void scanChar();
    void scanString();
    void scanNumber();
    void scanIdentifiersAndKeywords();
    void scanToken();

public:
    Lexer(const std::string& sourceCode);
    
    std::vector<Token> getTokenList();
    
    void scanSourceCode();

};
