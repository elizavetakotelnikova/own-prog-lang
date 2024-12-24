#pragma once

#include<iostream>
#include<map>

enum TokenType {
    // Single-character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_SQUARE,
    RIGHT_SQUARE,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    SEMICOLON,
    COLON,

    // Arithmetic operators
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULO,

    // Logical operators
    LOGICAL_AND, 
    LOGICAL_OR,
    NOT,

    // Bitwise operators
    BITWISE_AND,
    BITWISE_OR,
    BITWISE_XOR,

    // Assignment operator
    EQUAL,

    // Return type of function
    RETURN_TYPE,

    // Relational operators
    EQUAL_EQUAL,
    NOT_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals
    IDENTIFIER,
    STRING,
    NUMBER_INT,
    NUMBER_FLOAT,
    CHARACTER,

    // Keywords
    INT,
    BIGINT,
    FLOAT,
    STR,
    CHAR,
    BOOL,
    TRUE,
    FALSE,
    VOID,
    ARRAY,
    IF,
    ELSE,
    RETURN,
    FOR,
    WHILE,
    BREAK,
    FUNCTION,
    PRINT,

    EOF_TOKEN,
};

struct Token
{
    TokenType type;
    std::string value;
    Token(TokenType type, std::string value){
        this->type = type;
        this->value = value;
    }
};
