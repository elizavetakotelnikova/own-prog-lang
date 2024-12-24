#include "include/Parser.h"
#include <string>

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

// Get current token and std::move to next token
unique_ptr<Token> Parser::getToken(){
    return make_unique<Token>(tokenList.at(current++));
}

// Get current token
unique_ptr<Token> Parser::currentToken(){
    return make_unique<Token>(tokenList.at(current));
}

// Get next token
unique_ptr<Token> Parser::nextToken(){
    if (current >= tokenList.size()) return nullptr;
    return make_unique<Token>(tokenList.at(current + 1));
}

// Get previous token
unique_ptr<Token> Parser::previousToken(){
    if (current <= 0) return nullptr;
    return make_unique<Token>(tokenList.at(current - 1));
}

// Consume this token if its type match the required type, or else print out error
unique_ptr<Token> Parser::consumeToken(TokenType type, string errorMessage)
{
    if (currentToken()->type == type){
        return getToken();
    }
    std::cerr << errorMessage;
    return nullptr;
}

unique_ptr<Statement> Parser::statement()
{
    if (matchToken({INT, BIGINT, FLOAT, STR, CHAR, BOOL})) return variableDeclaration();
    if (matchToken({ARRAY})) return arrayDeclaration();
    if (matchToken({PRINT})) return print();
    if (matchToken({LEFT_BRACE})) return block();
    if (matchToken({IF})) return condition();
    if (matchToken({FOR})) return forLoop();
    if (matchToken({WHILE})) return whileLoop();
    if (matchToken({RETURN})) return returnStmt();
    return expressionStatement();
}

unique_ptr<Statement> Parser::variableDeclaration()
{
    TokenType type = previousToken()->type;
    string varName = consumeToken(IDENTIFIER, "expect variable name")->value;
    auto identifier = make_unique<Identifier>(varName);
    unique_ptr<Expression> initValue = nullptr;
    if (matchToken({EQUAL})){
        initValue = expression();
        if (initValue == nullptr){
            std::cerr << "expect primary expression";
        }
    }
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<VariableDeclaration>(type, std::move(identifier), std::move(initValue));
}

unique_ptr<Statement> Parser::arrayDeclaration()
{
    if (!matchToken({INT, BIGINT, FLOAT, STR, CHAR, BOOL})) std::cerr << "expect type";
    TokenType type = previousToken()->type;
    string varName = consumeToken(IDENTIFIER, "expect variable name")->value;
    auto identifier = make_unique<Identifier>(varName);
    consumeToken(LEFT_SQUARE, "expect a '['");
    if (currentToken()->type != NUMBER_INT){
        std::cerr << "Size requires positive int value";
    }
    int size = stoi(getToken()->value);
    if (size < 0){
        std::cerr << "Size requires positive int value";
    }
    consumeToken(RIGHT_SQUARE, "expect a ']'");

    vector<unique_ptr<Expression>> initValues;
    if (matchToken({EQUAL})){
        consumeToken(LEFT_BRACE, "expect a '{'");
        if (!matchToken({RIGHT_BRACE})){
            while (true){  
                auto initValue = expression();
                if (initValue == nullptr){
                    std::cerr << "expect primary expression";
                }
                initValues.push_back(std::move(initValue));
                if (matchToken({COMMA})){
                    continue;
                }
                else if (matchToken({RIGHT_BRACE})){
                    break;
                }
                else {
                    std::cerr << "expect a '}'";
                }
            }
        }
    }
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<ArrayDeclaration>(type, std::move(identifier), size, std::move(initValues));
}

unique_ptr<Statement> Parser::condition()
{
    consumeToken(LEFT_PAREN, "expect a '('");
    auto conditionalExpr = logicalOR();
    consumeToken(RIGHT_PAREN, "expect a ')'");
    auto ifBlock = statement(); 
    unique_ptr<Statement> elseBlock = nullptr;
    if (matchToken({ELSE})){
        elseBlock = statement();
    }
    return make_unique<Condition>(std::move(conditionalExpr), std::move(ifBlock), std::move(elseBlock));
}

unique_ptr<Statement> Parser::forLoop()
{
    consumeToken(LEFT_PAREN, "expect a '('");
    unique_ptr<Statement> initializer;
    if (matchToken({SEMICOLON})){
        initializer = nullptr;
    } else if (matchToken({INT, BIGINT, FLOAT, STR, CHAR, BOOL})) {
        initializer = variableDeclaration();
    } else {
        initializer = expressionStatement();
    }

    unique_ptr<Expression> condition = nullptr;
    if (currentToken()->type != SEMICOLON){
        condition = expression();
    } 
    consumeToken(SEMICOLON, "expect a ';'");

    unique_ptr<Expression> update =  nullptr;
    if (currentToken()->type != RIGHT_BRACE){
        update = expression();
    }
    consumeToken(RIGHT_PAREN, "expect a ')'");

    auto body = statement();
    
    return make_unique<ForLoop>(std::move(initializer), std::move(condition), std::move(update), std::move(body));
}

unique_ptr<Statement> Parser::whileLoop()
{
    consumeToken(LEFT_PAREN, "expect a '('");
    auto condition= logicalOR();
    consumeToken(RIGHT_PAREN, "expect a ')'");
    auto body = statement();
    return make_unique<WhileLoop>(std::move(condition), std::move(body));
}

std::unique_ptr<Statement> Parser::prototypeFunction()
{
    string functionName = consumeToken(IDENTIFIER, "expect an identifier")->value;
    consumeToken(LEFT_PAREN, "expect a '('");
    vector<pair<TokenType, string>> functionArgs;
    if (!matchToken({RIGHT_PAREN})){
        while (true){
            if (!matchToken({INT, BIGINT, FLOAT, STR, CHAR, BOOL})){
                std::cerr << "expect argument type";
            }
            TokenType argType = previousToken()->type;
            string argName = consumeToken(IDENTIFIER, "expect parameter name")->value;
            functionArgs.push_back({argType, argName});
            if (matchToken({COMMA})){
                continue;
            }
            else if (matchToken({RIGHT_PAREN})){
                break;
            }
            else {
                std::cerr << "expect a ')'";
            }
        }
    }
    
    consumeToken(RETURN_TYPE, "expect return type");
    if (!matchToken({INT, BIGINT, FLOAT, STR, CHAR, BOOL, VOID})){
        std::cerr << "expect return type";
    }
    auto returnType = previousToken()->type;
    return make_unique<PrototypeFunction>(functionName, functionArgs, returnType);
}

unique_ptr<Statement> Parser::function()
{
    auto proto = prototypeFunction();
    consumeToken(LEFT_BRACE, "expect a '{'");
    auto body = block();
    auto baseProto = std::unique_ptr<PrototypeFunction>(dynamic_cast<PrototypeFunction*>(proto.release()));
    auto baseBlock = std::unique_ptr<Block>(dynamic_cast<Block*>(body.release()));
    return std::make_unique<FunctionNode>(
        std::move(baseProto), std::move(baseBlock)
    );
}

std::unique_ptr<Statement> Parser::returnStmt()
{
    auto expr = expression();
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<Return>(std::move(expr));
}

unique_ptr<Statement> Parser::expressionStatement()
{
    auto expr = expression();
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<ExpressionStatement>(std::move(expr));
}

unique_ptr<Statement> Parser::print()
{
    consumeToken(LEFT_PAREN, "expect a '('");
    auto expr = expression();
    consumeToken(RIGHT_PAREN, "expect a ')'");
    consumeToken(SEMICOLON, "expect a ';'");
    return make_unique<Print>(std::move(expr));
}

unique_ptr<Statement> Parser::block()
{
    vector<unique_ptr<Statement>> statementList;
    while (currentToken()->type != RIGHT_BRACE){
        auto stmt = statement();
        if (stmt != nullptr){
            statementList.push_back(std::move(stmt));
        }
    }
    consumeToken(RIGHT_BRACE, "expect a '}'");
    return make_unique<Block>(std::move(statementList));
}

unique_ptr<Expression> Parser::expression(){
    return assignment();
}

unique_ptr<Expression> Parser::assignment()
{
    auto expr = logicalOR();
    if (matchToken({EQUAL})){
        auto value = expression();
        if (value == nullptr){
            std::cerr << "expect primary expression";
        }
        if (typeid(*expr) == typeid(Identifier) || typeid(*expr) == typeid(ArrayAccess)){
            return make_unique<Assignment>(std::move(expr), std::move(value));
        }
        std::cerr << "Invalid assignment target";
    }
    return expr;
}

unique_ptr<Expression> Parser::logicalOR()
{
    auto expr = logicalAND();
    while (matchToken({LOGICAL_OR})){
        Token _operator = *previousToken();
        auto right = logicalAND();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::logicalAND()
{
    auto expr = bitwiseOR();
    while (matchToken({LOGICAL_AND})){
        Token _operator = *previousToken();
        auto right = bitwiseOR();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::bitwiseOR()
{
    auto expr = bitwiseXOR();
    while (matchToken({BITWISE_OR})){
        Token _operator = *previousToken();
        auto right = bitwiseXOR();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::bitwiseXOR()
{
    auto expr = bitwiseAND();
    while (matchToken({BITWISE_XOR})){
        Token _operator = *previousToken();
        auto right = bitwiseAND();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::bitwiseAND()
{
    auto expr = equality();
    while (matchToken({BITWISE_AND})){
        Token _operator = *previousToken();
        auto right = equality();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::equality(){
    auto expr = comparison();
    while (matchToken({EQUAL_EQUAL, NOT_EQUAL})){
        Token _operator = *previousToken();
        auto right = comparison();
        expr = make_unique<Comparison>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::comparison(){
    auto expr = term();
    while (matchToken({GREATER, GREATER_EQUAL, LESS, LESS_EQUAL})){
        Token _operator = *previousToken();
        auto right = term();
        expr = make_unique<Comparison>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::term(){
    auto expr = factor();
    while (matchToken({PLUS, MINUS})){
        Token _operator = *previousToken();
        auto right = factor();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::factor(){
    auto expr = unary();
    while (matchToken({MULTIPLY, DIVIDE, MODULO})){
        Token _operator = *previousToken();
        auto right = unary();
        expr = make_unique<Binary>(_operator, std::move(expr), std::move(right));
    }
    return expr;
}

unique_ptr<Expression> Parser::unary(){
    if (matchToken({NOT, MINUS})){
        Token _operator = *previousToken();
        auto right = unary();
        return make_unique<Unary>(_operator, std::move(right));
    }
    return primary();
}

unique_ptr<Expression> Parser::primary(){
    if (matchToken({FALSE})) return make_unique<BoolLiteral>(false);
    if (matchToken({TRUE})) return make_unique<BoolLiteral>(true);
    if (matchToken({NUMBER_INT})) return make_unique<IntegerLiteral>(stoi(previousToken()->value));
    if (matchToken({NUMBER_FLOAT})) return make_unique<FloatLiteral>(stof(previousToken()->value));
    if (matchToken({CHARACTER})) return make_unique<CharLiteral>(previousToken()->value[0]);
    if (matchToken({STRING})) return make_unique<StringLiteral>(previousToken()->value);
    if (matchToken({IDENTIFIER})) return identifier();
    if (matchToken({LEFT_PAREN})){
        auto expr = expression();
        consumeToken(RIGHT_PAREN, "expected ')' ");
        return expr;
    }
    return nullptr;
}

unique_ptr<Expression> Parser::identifier()
{
    // Check if this is a functon call
    if (currentToken()->type == LEFT_PAREN){
        string functionName = previousToken()->value;
        vector<unique_ptr<Expression>> functionArgs;
        getToken();
        if (!matchToken({RIGHT_PAREN})){
            while (true){
                auto expr = expression();
                if (expr == nullptr){
                    std::cerr << "expected primary-expression";
                }
                functionArgs.push_back(std::move(expr));
                if (matchToken({RIGHT_PAREN})){
                    break;
                }
                else if (matchToken({COMMA})){
                    continue;
                } else {
                    std::cerr << "expected ')' ";
                }
            }
        }
        auto callFunc = make_unique<CallFunction>(std::move(functionName), std::move(functionArgs));
        return callFunc;
    }
    else if (currentToken()->type == LEFT_SQUARE){ // Access an element of array
        string identifierName = previousToken()->value;
        auto identifier = make_unique<Identifier>(identifierName);
        getToken();
        auto index = expression();
        consumeToken(RIGHT_SQUARE, "expect a ']'");
        return make_unique<ArrayAccess>(std::move(identifier), std::move(index));
    } 
    else { 
        return make_unique<Identifier>(previousToken()->value); // Just a variable
    }
}

vector<unique_ptr<ASTNode>> Parser::getASTNodeList()
{
    return std::move(ASTNodeList);
}

void Parser::parse()
{
    cout << "Size: " << tokenList.size() << endl;
    while (currentToken()->type != EOF_TOKEN){
        unique_ptr<ASTNode> node;
        if (matchToken({FUNCTION})){
            node = function();
        }
        else {
            node = statement();
        }
        ASTNodeList.push_back(std::move(node));
    }
}


