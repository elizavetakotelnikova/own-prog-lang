#include "include/Visitor.h"
#include "include/VisitorPrintNode.h"

using namespace std;

void VisitorPrintNode::visitExpression(Expression &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitStatement(Statement &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitIntegerLiteral(IntegerLiteral &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitFloatLiteral(FloatLiteral &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitStringLiteral(StringLiteral &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitCharLiteral(CharLiteral &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitBoolLiteral(BoolLiteral &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitIdentifier(Identifier &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
}

void VisitorPrintNode::visitUnary(Unary &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.operand->accept(*this);
}

void VisitorPrintNode::visitBinary(Binary &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.leftOperand->accept(*this);
    node.rightOperand->accept(*this);
}

void VisitorPrintNode::visitComparison(Comparison &node){
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.leftOperand->accept(*this);
    node.rightOperand->accept(*this);
}

void VisitorPrintNode::visitCallFunction(CallFunction &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    for (auto &arg : node.functionArgs){
        arg->accept(*this);
    }
}

void VisitorPrintNode::visitArrayAccess(ArrayAccess &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.identifier->accept(*this);
    node.index->accept(*this);
}

void VisitorPrintNode::visitAssignment(Assignment &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.identifier->accept(*this);
    node.value->accept(*this);
}

void VisitorPrintNode::visitExpressionStatement(ExpressionStatement &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.expression->accept(*this);
}

void VisitorPrintNode::visitArrayDeclaration(ArrayDeclaration &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.identifier->accept(*this);
    for (auto &initValue : node.initValues){
        initValue->accept(*this);
    }
}

void VisitorPrintNode::visitVariableDeclaration(VariableDeclaration &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.identifier->accept(*this);
    if (node.initValue != nullptr){
        node.initValue->accept(*this);
    }
}

void VisitorPrintNode::visitPrint(Print &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.expr->accept(*this);
}

void VisitorPrintNode::visitBlock(Block &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    for (auto &stmt : node.statementList){
        stmt->accept(*this);
    }
}

void VisitorPrintNode::visitCondition(Condition &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.conditionExpr->accept(*this);
    node.ifBlock->accept(*this);
    if (node.elseBlock != nullptr){
        node.elseBlock->accept(*this);
    }
}

void VisitorPrintNode::visitForLoop(ForLoop &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    if (node.initializer != nullptr){
        node.initializer->accept(*this);
    }
    if (node.condition != nullptr){
        node.condition->accept(*this);
    }
    if (node.update != nullptr){
        node.update->accept(*this);
    }
    node.body->accept(*this);
}

void VisitorPrintNode::visitWhileLoop(WhileLoop &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.condition->accept(*this);
    node.body->accept(*this);
}

void VisitorPrintNode::visitPrototypeFunction(PrototypeFunction &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    out << "Args: " << endl;
    for (auto arg : node.args){
        out << arg.first << " " << arg.second << endl;
    }
}

void VisitorPrintNode::visitFunction(FunctionNode &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.prototype->accept(*this);
    node.bodyBlock->accept(*this);
}

void VisitorPrintNode::visitReturn(Return &node)
{
    node.isChecked = true;
    out << "Create " << node.toString() << endl;
    node.expr->accept(*this);
}
