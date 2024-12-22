#pragma once

#include "AST.h"

class Visitor {
public:
    virtual void visitExpression(Expression& node) = 0;
    virtual void visitStatement(Statement& node) = 0;
    virtual void visitIntegerLiteral(IntegerLiteral& node) = 0;
    virtual void visitFloatLiteral(FloatLiteral& node) = 0;
    virtual void visitStringLiteral(StringLiteral& node) = 0;
    virtual void visitCharLiteral(CharLiteral& node) = 0;
    virtual void visitBoolLiteral(BoolLiteral& node) = 0;
    virtual void visitIdentifier(Identifier& node) = 0;
    virtual void visitUnary(Unary& node) = 0;
    virtual void visitBinary(Binary& node) = 0;
    virtual void visitComparison(Comparison& node) = 0;
    virtual void visitCallFunction(CallFunction& node) = 0;
    virtual void visitArrayAccess(ArrayAccess& node) = 0;
    virtual void visitAssignment(Assignment& node) = 0;
    virtual void visitExpressionStatement(ExpressionStatement& node) = 0;
    virtual void visitArrayDeclaration(ArrayDeclaration& node) = 0;
    virtual void visitVariableDeclaration(VariableDeclaration& node) = 0;
    virtual void visitPrint(Print& node) = 0;
    virtual void visitBlock(Block& node) = 0;
    virtual void visitCondition(Condition& node) = 0;
    virtual void visitForLoop(ForLoop& node) = 0;
    virtual void visitWhileLoop(WhileLoop& node) = 0;
    virtual void visitPrototypeFunction(PrototypeFunction& node) = 0;
    virtual void visitFunction(FunctionNode& node) = 0;
    virtual void visitReturn(Return& node) = 0;
};