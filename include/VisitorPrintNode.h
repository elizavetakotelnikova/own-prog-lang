#pragma once

#include "Visitor.h"

class VisitorPrintNode : public Visitor {
    int id;
    std::ostream& out;
public:
    VisitorPrintNode(std::ostream& outs) : out(outs) {}
    virtual ~VisitorPrintNode() = default;
    void visitExpression(Expression& node);
    void visitStatement(Statement& node);
    void visitIntegerLiteral(IntegerLiteral& node);
    void visitFloatLiteral(FloatLiteral& node);
    void visitStringLiteral(StringLiteral& node);
    void visitCharLiteral(CharLiteral& node);
    void visitBoolLiteral(BoolLiteral& node);
    void visitIdentifier(Identifier& node);
    void visitUnary(Unary& node);
    void visitBinary(Binary& node);
    void visitComparison(Comparison& node);
    void visitCallFunction(CallFunction& node);
    void visitArrayAccess(ArrayAccess& node);
    void visitAssignment(Assignment& node);
    void visitExpressionStatement(ExpressionStatement& node);
    void visitArrayDeclaration(ArrayDeclaration& node);
    void visitVariableDeclaration(VariableDeclaration& node);
    void visitPrint(Print& node);
    void visitBlock(Block& node);
    void visitCondition(Condition& node);
    void visitForLoop(ForLoop& node);
    void visitWhileLoop(WhileLoop& node);
    void visitPrototypeFunction(PrototypeFunction& node);
    void visitFunction(FunctionNode& node);
    void visitReturn(Return& node);
};