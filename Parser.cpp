#include "include/Parser.h"
#include <string>
#include <typeinfo>
#include "Parser.h"

using namespace std;

bool Parser::matchToken(const vector<TokenType>& tokenTypeList){
    auto currentTokenType = currentToken()->type;
    for (auto type : tokenTypeList){
        if (currentTokenType == type){  
            getToken();
            return true;
        }
    }
    return false;
}

bool Parser::isAtEnd(){
    return currentToken()->type == EOF_TOKEN;
}

unique_ptr<ASTNode> Parser::logError(string error){
    cerr << error << endl;
    return nullptr;
}

unique_ptr<Token> Parser::getToken(){
    return make_unique<Token>(tokenList.at(current++));
}

unique_ptr<Token> Parser::currentToken(){
    return make_unique<Token>(tokenList.at(current));
}

unique_ptr<Token> Parser::nextToken(){
    if (current >= tokenList.size()) return nullptr;
    return make_unique<Token>(tokenList.at(current + 1));
}

unique_ptr<Token> Parser::previousToken(){
    if (current <= 0) return nullptr;
    return make_unique<Token>(tokenList.at(current - 1));
}

std::unique_ptr<Token> Parser::consumeToken(TokenType type, std::string errorMessage)
{
    if (currentToken()->type == type){
        return getToken();
    }
    throw runtime_error(errorMessage);
}

std::unique_ptr<ASTNode> Parser::statement()
{
    if (matchToken({INT, FLOAT, STR, CHAR, BOOL})) return variableDeclaration();
    if (matchToken({PRINT})) return print();
    if (matchToken({LEFT_BRACE})) return block();
    if (matchToken({IF})) return condition();
    if (matchToken({FOR, WHILE}))
    return nullptr;
}

std::unique_ptr<ASTNode> Parser::variableDeclaration()
{
    TokenType type = previousToken()->type;
    string varName = consumeToken(IDENTIFIER, "expect variable name")->value;
    unique_ptr<ASTNode> initValue = nullptr;
    if (matchToken({EQUAL})){
        initValue = expression();
    }
    if (initValue == nullptr){
        throw runtime_error("expect primary expression");
    }
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<VariableDeclarationASTNode>(type, varName, initValue);
}

std::unique_ptr<ASTNode> Parser::condition()
{
return std::unique_ptr<ASTNode>();
}

std::unique_ptr<ASTNode> Parser::forLoop()
{
return std::unique_ptr<ASTNode>();
}

std::unique_ptr<ASTNode> Parser::whileLoop()
{
return std::unique_ptr<ASTNode>();
}

std::unique_ptr<ASTNode> Parser::function()
{
return std::unique_ptr<ASTNode>();
}

std::unique_ptr<ASTNode> Parser::print()
{
    consumeToken(LEFT_PAREN, "expect a '('");
    auto expr = logicalOR();
    consumeToken(RIGHT_PAREN, "expect a ')'");
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<PrintASTNode>(expr);
}

std::unique_ptr<ASTNode> Parser::block()
{
    vector<unique_ptr<ASTNode>> statementList;
    while (currentToken()->type != RIGHT_BRACE){
        auto stmt = statement();
        if (stmt != nullptr){
            statementList.push_back(stmt);
        }
    }
    return make_unique<BlockASTNode>(statementList);
}

unique_ptr<ASTNode> Parser::expression(){
    return logicalOR();
}

std::unique_ptr<ASTNode> Parser::assignment()
{
    auto expr = logicalOR();
    if (matchToken({EQUAL})){
        auto value = assignment();
        if (typeid(*expr) == typeid(IdentifierASTNode)){
            string varName = (static_cast<IdentifierASTNode*>(expr.release()))->getValue();
            return make_unique<AssignmentASTNode>(varName, value);
        }
        throw runtime_error("Invalid assignment target");
    }
    return expr;
}

unique_ptr<ASTNode> Parser::logicalOR()
{
    auto expr = logicalAND();
    while (matchToken({LOGICAL_OR})){
        Token _operator = *previousToken();
        auto right = logicalAND();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::logicalAND()
{
    auto expr = bitwiseOR();
    while (matchToken({LOGICAL_AND})){
        Token _operator = *previousToken();
        auto right = bitwiseOR();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::bitwiseOR()
{
    auto expr = bitwiseXOR();
    while (matchToken({BITWISE_OR})){
        Token _operator = *previousToken();
        auto right = bitwiseXOR();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::bitwiseXOR()
{
    auto expr = bitwiseAND();
    while (matchToken({BITWISE_XOR})){
        Token _operator = *previousToken();
        auto right = bitwiseAND();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::bitwiseAND()
{
    auto expr = equality();
    while (matchToken({BITWISE_AND})){
        Token _operator = *previousToken();
        auto right = equality();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::equality(){
    auto expr = comparison();
    while (matchToken({EQUAL_EQUAL, NOT_EQUAL})){
        Token _operator = *previousToken();
        auto right = comparison();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::comparison(){
    auto expr = term();
    while (matchToken({GREATER, GREATER_EQUAL, LESS, LESS_EQUAL})){
        Token _operator = *previousToken();
        auto right = term();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::term(){
    auto expr = factor();
    while (matchToken({PLUS, MINUS})){
        Token _operator = *previousToken();
        auto right = factor();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::factor(){
    auto expr = unary();
    while (matchToken({MULTIPLY, DIVIVE, MODULO})){
        Token _operator = *previousToken();
        auto right = unary();
        expr = make_unique<BinaryASTNode>(_operator, expr, right);
    }
    return expr;
}

unique_ptr<ASTNode> Parser::unary(){
    if (matchToken({NOT, MINUS})){
        Token _operator = *previousToken();
        auto right = unary();
        return make_unique<UnaryASTNode>(_operator, right);
    }
    return primary();
}

unique_ptr<ASTNode> Parser::primary(){
    if (matchToken({FALSE})) return make_unique<BoolLiteralASTNode>(false);
    if (matchToken({TRUE})) return make_unique<BoolLiteralASTNode>(true);
    if (matchToken({NUMBER_INT})) return make_unique<IntegerLiteralASTNode>(stoi(previousToken()->value));
    if (matchToken({NUMBER_FLOAT})) return make_unique<FloatLiteralASTNode>(stof(previousToken()->value));
    if (matchToken({CHARACTER})) return make_unique<CharLiteralASTNode>(previousToken()->value[0]);
    if (matchToken({STRING})) return make_unique<StringLiteralASTNode>(previousToken()->value);
    if (matchToken({IDENTIFIER})) return identifier();
    if (matchToken({LEFT_PAREN})){
        auto expr = expression();
        consumeToken(RIGHT_PAREN, "expected ')' ");
        return expr;
    }
    return nullptr;
}

std::unique_ptr<ASTNode> Parser::identifier()
{
    if (currentToken()->type == LEFT_PAREN){
        string functionName = previousToken()->value;
        vector<unique_ptr<ASTNode>> functionArgs;
        getToken();
        while (true){
            bool afterComma = false;
            auto expr = expression();
            if (expr == nullptr && afterComma){
                throw runtime_error("expected primary-expression");
            }
            if (expr != nullptr){
                functionArgs.push_back(expr);
            }
            if (matchToken({RIGHT_PAREN})){
                break;
            }
            else if (matchToken({COMMA})){
                afterComma = true;
            } else {
                throw runtime_error("expected ')' ");
            }
        }
        auto callFunc = make_unique<CallFunctionASTNode>(functionName, functionArgs);
        return callFunc;
    } else {
        return make_unique<IdentifierASTNode>(previousToken()->value);
    }
}

void Parser::parse()
{

}
