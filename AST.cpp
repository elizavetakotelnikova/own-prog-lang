#include "include/AST.h"
#include "include/CodeGenContext.h"
#include "llvm/IR/Constants.h"

llvm::Value *IntegerLiteral::codeGeneration(CodeGenContext &context)
{
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(context.llvmContext), value);
}

llvm::Value *FloatLiteral::codeGeneration(CodeGenContext &context)
{
    return llvm::ConstantFP::get(llvm::Type::getFloatTy(context.llvmContext), value);
}

llvm::Value *StringLiteral::codeGeneration(CodeGenContext &context)
{
    llvm::Constant *stringConstant = llvm::ConstantDataArray::getString(context.llvmContext, value, true);

    llvm::GlobalVariable *globalString = new llvm::GlobalVariable(
        *context.module,
        stringConstant->getType(),
        true,
        llvm::GlobalValue::PrivateLinkage,
        stringConstant,
        ".str");

    llvm::Value *stringPtr = context.builder.CreateInBoundsGEP(
        globalString->getValueType(),
        globalString,
        {context.builder.getInt32(0), context.builder.getInt32(0)},
        "str_ptr");

    context.getGCManager().addObject(this);
    return stringPtr;
}

llvm::Value *BoolLiteral::codeGeneration(CodeGenContext &context)
{
    return llvm::ConstantInt::get(llvm::Type::getInt1Ty(context.llvmContext), value);
}

llvm::Value *CharLiteral::codeGeneration(CodeGenContext &context)
{
    return llvm::ConstantInt::get(llvm::Type::getInt8Ty(context.llvmContext), static_cast<uint8_t>(value));
}

llvm::Value *Identifier::codeGeneration(CodeGenContext &context)
{
    for (auto blockIt = context.blocks.rbegin(); blockIt != context.blocks.rend(); ++blockIt)
    {
        auto &locals = (*blockIt)->locals;
        auto it = locals.find(value);
        if (it != locals.end())
        {
            llvm::Value *varPtr = it->second.first;
            llvm::Type *varType = it->second.second;
            if (!varPtr)
            {
                std::cerr << "Null pointer for variable: " << value << "\n";
                return nullptr;
            }
            return context.builder.CreateLoad(varType, varPtr, value);
        }
    }

    std::cerr << "Undefined identifier: " << value << "\n";
    return nullptr;
}

llvm::Value *Unary::codeGeneration(CodeGenContext &context)
{
    llvm::Value *operandValue = operand->codeGeneration(context);
    if (operandValue == nullptr)
        return nullptr;

    llvm::Instruction::BinaryOps instr;
    switch (_operator.type)
    {
    case MINUS:
        return context.builder.CreateNeg(operandValue, "neg");
    case NOT:
        if (!operandValue->getType()->isIntegerTy())
        {
            std::cerr << "Operand of NOT must be an integer type" << "\n";
            return nullptr;
        }
        return context.builder.CreateNot(operandValue, "not");
    default:
        std::cerr << "Unknown unary operator" << "\n";
        return nullptr;
    }
}

llvm::Value *Binary::codeGeneration(CodeGenContext &context)
{
    llvm::Value *leftValue = leftOperand->codeGeneration(context);
    llvm::Value *rightValue = rightOperand->codeGeneration(context);
    if ((leftValue == nullptr) || (rightValue == nullptr))
    {
        return nullptr;
    }
    llvm::Type *leftType = leftValue->getType();
    llvm::Type *rightType = rightValue->getType();

    if (leftType != rightType)
    {
        if (leftType->isIntegerTy() && rightType->isFloatingPointTy())
        {
            leftValue = context.builder.CreateSIToFP(leftValue, rightType, "castLeft");
        }
        else if (leftType->isFloatingPointTy() && rightType->isIntegerTy())
        {
            rightValue = context.builder.CreateSIToFP(rightValue, leftType, "castRight");
        }
        else if (leftType->isIntegerTy() && rightType->isIntegerTy())
        {
            unsigned leftBits = leftType->getIntegerBitWidth();
            unsigned rightBits = rightType->getIntegerBitWidth();
            if (leftBits > rightBits)
            {
                rightValue = context.builder.CreateSExt(rightValue, leftType, "castRight");
            }
            else if (rightBits > leftBits)
            {
                leftValue = context.builder.CreateSExt(leftValue, rightType, "castLeft");
            }
        }

        else
        {
            std::cerr << "Unsupported type combination in binary operation: " << leftType->getTypeID() << " and " << rightType->getTypeID() << "\n";
            return nullptr;
        }
    }

    bool isDoubleTy = rightValue->getType()->isFloatingPointTy();
    if (isDoubleTy && (_operator.type == LOGICAL_AND || _operator.type == LOGICAL_OR))
    {
        std::cerr << "Binary operation (AND, OR) on floating point value is not supported" << "\n";
        return nullptr;
    }

    llvm::Instruction::BinaryOps instr;
    switch (_operator.type)
    {
    case PLUS:
        isDoubleTy ? instr = llvm::Instruction::FAdd : instr = llvm::Instruction::Add;
        break;
    case MINUS:
        isDoubleTy ? instr = llvm::Instruction::FSub : instr = llvm::Instruction::Sub;
        break;
    case MULTIPLY:
        isDoubleTy ? instr = llvm::Instruction::FMul : instr = llvm::Instruction::Mul;
        break;
    case DIVIDE:
        isDoubleTy ? instr = llvm::Instruction::FDiv : instr = llvm::Instruction::SDiv;
        break;
    case LOGICAL_AND:
        instr = llvm::Instruction::And;
        break;
    case LOGICAL_OR:
        instr = llvm::Instruction::Or;
        break;
    default:
        std::cerr << "Unknown binary operator" << "\n";
        return nullptr;
    }

    context.getGCManager().addObject(this);

    return context.builder.CreateBinOp(instr, leftValue, rightValue, "mathtmp");
}

llvm::Value *Comparison::codeGeneration(CodeGenContext &context)
{
    llvm::Value *leftValue = leftOperand->codeGeneration(context);
    llvm::Value *rightValue = rightOperand->codeGeneration(context);
    if ((leftValue == nullptr) || (rightValue == nullptr))
    {
        return nullptr;
    }

    llvm::Type *leftType = leftValue->getType();
    llvm::Type *rightType = rightValue->getType();

    if (leftType != rightType)
    {
        if (leftType->isIntegerTy() && rightType->isFloatingPointTy())
        {
            leftValue = context.builder.CreateSIToFP(leftValue, rightType, "castLeft");
        }
        else if (leftType->isFloatingPointTy() && rightType->isIntegerTy())
        {
            rightValue = context.builder.CreateSIToFP(rightValue, leftType, "castRight");
        }
        else if (leftType->isIntegerTy() && rightType->isIntegerTy())
        {
            unsigned leftBits = leftType->getIntegerBitWidth();
            unsigned rightBits = rightType->getIntegerBitWidth();
            if (leftBits > rightBits)
            {
                rightValue = context.builder.CreateSExt(rightValue, leftType, "castRight");
            }
            else if (rightBits > leftBits)
            {
                leftValue = context.builder.CreateSExt(leftValue, rightType, "castLeft");
            }
        }

        else
        {
            std::cerr << "Unsupported type combination in binary operation: " << leftType->getTypeID() << " and " << rightType->getTypeID() << "\n";
            return nullptr;
        }
    }

    bool isDoubleTy = rightValue->getType()->isFloatingPointTy();

    llvm::CmpInst::Predicate predicate;
    switch (_operator.type)
    {
    case EQUAL_EQUAL:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_OEQ : llvm::CmpInst::ICMP_EQ;
        break;
    case NOT_EQUAL:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_ONE : llvm::CmpInst::ICMP_NE;
        break;
    case LESS:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_OLT : llvm::CmpInst::ICMP_SLT;
        break;
    case GREATER:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_OGT : llvm::CmpInst::ICMP_SGT;
        break;
    case LESS_EQUAL:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_OLE : llvm::CmpInst::ICMP_SLE;
        break;
    case GREATER_EQUAL:
        predicate = isDoubleTy ? llvm::CmpInst::FCMP_OGE : llvm::CmpInst::ICMP_SGE;
        break;
    default:
        std::cerr << "Unknown comparison operator" << "\n";
        return nullptr;
    }

    return isDoubleTy
               ? context.builder.CreateFCmp(predicate, leftValue, rightValue, "cmptmp")
               : context.builder.CreateICmp(predicate, leftValue, rightValue, "cmptmp");
}

llvm::Value *CallFunction::codeGeneration(CodeGenContext &context)
{
    llvm::Function *CalleeF = context.module->getFunction(functionName);
    if (!CalleeF)
    {
        std::cerr << "Unknown function referenced: " << functionName << "\n";
        return nullptr;
    }

    if (CalleeF->arg_size() != functionArgs.size())
    {
        std::cerr << "Incorrect number of arguments passed to function: " << functionName << "\n";
        return nullptr;
    }

    std::vector<llvm::Value *> ArgsV;
    for (unsigned i = 0; i < functionArgs.size(); ++i)
    {
        llvm::Value *argValue = functionArgs[i]->codeGeneration(context);
        if (!argValue)
        {
            std::cerr << "Failed to generate code for argument in function call: " << functionName << std::endl;
            return nullptr;
        }

        llvm::Type *expectedArgType = CalleeF->getArg(i)->getType();
        llvm::Type *argType = argValue->getType();

        if (argType != expectedArgType)
        {
            if (expectedArgType->isIntegerTy() && argType->isIntegerTy())
            {
                unsigned expectedBits = expectedArgType->getIntegerBitWidth();
                unsigned argBits = argType->getIntegerBitWidth();

                if (argBits < expectedBits)
                {
                    if (argBits == 1)
                    {                                                                                                      
                        argValue = context.builder.CreateIntCast(argValue, expectedArgType, true, "arg_cast_bool_to_int"); 
                    }
                    else
                    {
                        argValue = context.builder.CreateSExt(argValue, expectedArgType, "arg_cast_sext"); 
                    }
                }
                else
                {                                                                                        
                    argValue = context.builder.CreateTrunc(argValue, expectedArgType, "arg_cast_trunc"); 
                }
            }
            else if (expectedArgType->isFloatingPointTy() && argType->isIntegerTy())
            {
                argValue = context.builder.CreateSIToFP(argValue, expectedArgType, "arg_cast");
            }
            else if (expectedArgType->isIntegerTy() && argType->isFloatingPointTy())
            {
                argValue = context.builder.CreateFPToSI(argValue, expectedArgType, "arg_cast");
            }
            else
            {
                std::cerr << "Incompatible argument type" << std::endl;
                context.popBlock();
                return nullptr;
            }
        }
        ArgsV.push_back(argValue);
    }

    llvm::CallInst *callInst = context.builder.CreateCall(CalleeF, ArgsV);
    if (CalleeF->getReturnType()->isVoidTy())
    {
        return callInst;
    }

    callInst->setName("calltmp");
    return callInst;
}

llvm::Value *ArrayAccess::codeGeneration(CodeGenContext &context)
{
    llvm::Value *arrayPtr = nullptr;
    llvm::Type *arrayType = nullptr;

    for (auto blockIt = context.blocks.rbegin(); blockIt != context.blocks.rend(); ++blockIt)
    {
        auto &locals = (*blockIt)->locals;
        auto it = locals.find(identifier->value);
        if (it != locals.end())
        {
            arrayPtr = it->second.first;
            arrayType = it->second.second;
            break;
        }
    }
    if (!arrayPtr)
    {
        std::cerr << "Array " << identifier->value << " not declared." << "\n";
        return nullptr;
    }

    if (!arrayType->isArrayTy())
    {
        std::cerr << identifier->value << " is not an array." << "\n";
        return nullptr;
    }

    llvm::Type *elementType = arrayType->getArrayElementType();
    llvm::Value *indexValue = index->codeGeneration(context);
    llvm::Value *elementPtr = context.builder.CreateInBoundsGEP(
        arrayType,
        arrayPtr,
        {context.builder.getInt32(0), indexValue},
        identifier->value + "_access");

    return context.builder.CreateLoad(elementType, elementPtr, identifier->value + "_loaded");
}

llvm::Value *ExpressionStatement::codeGeneration(CodeGenContext &context)
{
    
    return expression->codeGeneration(context);
}

llvm::Value *ArrayDeclaration::codeGeneration(CodeGenContext &context)
{
    llvm::Type *elementType;
    switch (type)
    {
    case TokenType::INT:
        elementType = llvm::Type::getInt32Ty(context.llvmContext);
        break;
    case TokenType::BIGINT: // 128 bit
        elementType = llvm::Type::getInt128Ty(context.llvmContext);
        break;
    case TokenType::FLOAT:
        elementType = llvm::Type::getFloatTy(context.llvmContext);
        break;
    case TokenType::BOOL:
        elementType = llvm::Type::getInt1Ty(context.llvmContext); // 1-bit integer
        break;
    case TokenType::CHAR:
        elementType = llvm::Type::getInt8Ty(context.llvmContext); // 8-bit integer
        break;
    case TokenType::STR:
        elementType = llvm::PointerType::getInt8Ty(context.llvmContext);
        break;
    default:
        std::cerr << "Unsupported array type." << "\n";
        return nullptr;
    }
    llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, size);
    llvm::AllocaInst *arrayAlloc = context.builder.CreateAlloca(arrayType, nullptr, identifier->value);
    context.locals()[identifier->value] = {arrayAlloc, arrayType};

    if (!initValues.empty())
    {
        for (size_t i = 0; i < initValues.size(); ++i)
        {
            llvm::Value *initValue = initValues[i]->codeGeneration(context);
            llvm::Value *index = context.builder.getInt32(i);

            // Get pointer to the array element
            llvm::Value *elementPtr = context.builder.CreateInBoundsGEP(
                arrayType,
                arrayAlloc,
                {context.builder.getInt32(0), index},
                identifier->value + "_elem_ptr");

            context.builder.CreateStore(initValue, elementPtr);
        }
    }

    return arrayAlloc;
}

llvm::Value *VariableDeclaration::codeGeneration(CodeGenContext &context)
{
    llvm::Type *varType = nullptr;
    switch (type)
    {
    case TokenType::INT:
        varType = llvm::Type::getInt32Ty(context.llvmContext);
        std::cout << "Assigned type - int32" << "\n";
        break;
    case TokenType::BIGINT: // 128 bit
        varType = llvm::Type::getInt128Ty(context.llvmContext);
        std::cout << "Assigned type - int128" << "\n";
        break;
    case TokenType::FLOAT:
        varType = llvm::Type::getFloatTy(context.llvmContext);
        std::cout << "Assigned type - float" << "\n";
        break;
    case TokenType::CHAR:
        varType = llvm::Type::getInt8Ty(context.llvmContext);
        std::cout << "Assigned type - char" << "\n";
        break;
    case TokenType::STR:
        varType = llvm::PointerType::getInt8Ty(context.llvmContext);
        std::cout << "Assigned type - str" << "\n";
        break;
    case TokenType::BOOL:
        varType = llvm::Type::getInt1Ty(context.llvmContext);
        std::cout << "Assigned type - boolean" << "\n";
        break;
    }

    if (!varType)
    {
        std::cerr << "Unsupported variable type" << "\n";
        return nullptr;
    }

    llvm::AllocaInst *allocaInst = context.builder.CreateAlloca(varType, nullptr, identifier->value);

    if (initValue)
    {
        llvm::Value *initVal = initValue->codeGeneration(context);

        if (initVal->getType() != varType)
        {
            if (varType->isIntegerTy() && initVal->getType()->isIntegerTy())
            {
                initVal = context.builder.CreateIntCast(initVal, varType, true, "cast");
            }
            else if (varType->isFloatingPointTy() && initVal->getType()->isIntegerTy())
            {
                initVal = context.builder.CreateSIToFP(initVal, varType, "int_to_float");
            }
            else if (varType->isIntegerTy() && initVal->getType()->isFloatingPointTy())
            {
                initVal = context.builder.CreateFPToSI(initVal, varType, "float_to_int");
            }
            else
            {
                std::cerr << "Type mismatch in variable initialization" << "\n";
                return nullptr;
            }
        }

        context.builder.CreateStore(initVal, allocaInst);
    }


    context.locals()[identifier->value] = {allocaInst, varType};

    context.getGCManager().addRoot(this);
    return allocaInst;
}

llvm::Value *Assignment::codeGeneration(CodeGenContext &context)
{
    std::string identifierName;
    llvm::Value *varPtr;
    llvm::Type *varType;
    if (typeid(*identifier) == typeid(Identifier))
    {
        identifierName = dynamic_cast<Identifier *>(identifier.get())->value;
        for (auto blockIt = context.blocks.rbegin(); blockIt != context.blocks.rend(); ++blockIt)
        {
            auto &locals = (*blockIt)->locals;
            auto it = locals.find(identifierName);
            if (it != locals.end())
            {
                varPtr = it->second.first;
                varType = it->second.second;
                break;
            }
        }
        if (!varPtr)
        {
            std::cerr << "Undefined: " << identifierName << "\n";
            return nullptr;
        }

        llvm::Value *exprValue = value->codeGeneration(context);
        context.builder.CreateStore(exprValue, varPtr);
        return exprValue;
    }
    else if (typeid(*identifier) == typeid(ArrayAccess))
    {
        identifierName = dynamic_cast<ArrayAccess *>(identifier.get())->identifier->value;
        for (auto blockIt = context.blocks.rbegin(); blockIt != context.blocks.rend(); ++blockIt)
        {
            auto &locals = (*blockIt)->locals;
            auto it = locals.find(identifierName);
            if (it != locals.end())
            {
                varPtr = it->second.first;
                varType = it->second.second;
                break;
            }
        }
        if (!varPtr)
        {
            std::cerr << "Undefined: " << identifierName << "\n";
            return nullptr;
        }

        llvm::Value *indexValue = dynamic_cast<ArrayAccess *>(identifier.get())->index->codeGeneration(context);

        llvm::Value *elementPtr = context.builder.CreateGEP(
            varType,
            varPtr,
            {context.builder.getInt32(0), indexValue},
            "elementPtr");

        llvm::Value *exprValue = value->codeGeneration(context);
        context.builder.CreateStore(exprValue, elementPtr);

        return exprValue;
    }

    // if (exprValue->getType() != varType)
    // {
    //     if (varType->isIntegerTy() && exprValue->getType()->isIntegerTy())
    //     {
    //         exprValue = context.builder.CreateIntCast(exprValue, varType, true, "cast");
    //     }
    //     else if (varType->isFloatingPointTy() && exprValue->getType()->isIntegerTy())
    //     {
    //         exprValue = context.builder.CreateSIToFP(exprValue, varType, "int_to_float");
    //     }
    //     else if (varType->isIntegerTy() && exprValue->getType()->isFloatingPointTy())
    //     {
    //         exprValue = context.builder.CreateFPToSI(exprValue, varType, "float_to_int");
    //     }
    //     else
    //     {
    //         std::cerr << "Type mismatch in assignment" << "\n";
    //         return nullptr;
    //     }
    // }

    return nullptr;
}

llvm::Value *Print::codeGeneration(CodeGenContext &context)
{
    std::cout << "Print AST Node" << "\n";
    llvm::Value *value = expr->codeGeneration(context);
    if (!value)
    {
        return nullptr;
    }

    llvm::Type *valueType = value->getType();
    llvm::FunctionType *printfType = llvm::FunctionType::get(
        context.builder.getInt32Ty(),
        llvm::PointerType::get(context.builder.getInt8Ty(), 0),
        true);

    llvm::FunctionCallee printfFunc = context.module->getOrInsertFunction("printf", printfType);

    llvm::Value *formatStr = nullptr;
    if (valueType->isIntegerTy(32))
    {
        formatStr = context.builder.CreateGlobalStringPtr("%d\n", "formatStr");
    }
    else if (valueType->isIntegerTy(128))
    {
        formatStr = context.builder.CreateGlobalStringPtr("%lld\n", "formatStr");
    }
    else if (valueType->isFloatingPointTy())
    {
        formatStr = context.builder.CreateGlobalStringPtr("%f\n", "formatStr");
    }
    else if (valueType->isIntegerTy(1))
    {
        formatStr = context.builder.CreateGlobalStringPtr("%d\n", "formatStr");
    }
    else if (valueType->isIntegerTy(8))
    {
        formatStr = context.builder.CreateGlobalStringPtr("%c\n", "formatStr");
    }
    else if (valueType->isPointerTy())
    {
        formatStr = context.builder.CreateGlobalStringPtr("%s\n", "formatStr");
    }
    else
    {
        return nullptr;
    }

    return context.builder.CreateCall(printfFunc, {formatStr, value}, "printCall");
};

llvm::Value *Block::codeGeneration(CodeGenContext &context)
{
    llvm::Value *lastValue = nullptr;

    context.pushBlock(context.builder.GetInsertBlock());

    for (const auto &statement : statementList)
    {
        if (statement)
        {
            lastValue = statement->codeGeneration(context);    
        }
    }

    context.popBlock();

    return lastValue;
};

llvm::Value *Condition::codeGeneration(CodeGenContext &context)
{
    llvm::Value *condition = conditionExpr->codeGeneration(context);
    if (!condition)
    {
        std::cerr << "Failed to generate condition expression." << "\n";
        return nullptr;
    }

    if (!condition->getType()->isIntegerTy(1))
    {
        condition = context.builder.CreateICmpNE(
            condition,
            llvm::ConstantInt::get(condition->getType(), 0),
            "ifcond");
    }
    llvm::Function *currFunc = context.builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *thenBl = llvm::BasicBlock::Create(context.llvmContext, "then", currFunc);
    llvm::BasicBlock *elseBl = llvm::BasicBlock::Create(context.llvmContext, "else", currFunc);
    llvm::BasicBlock *mergeBl = llvm::BasicBlock::Create(context.llvmContext, "ifcont", currFunc);

    context.builder.CreateCondBr(condition, thenBl, elseBl);

    context.builder.SetInsertPoint(thenBl);
    llvm::Value *thenV = ifBlock->codeGeneration(context);
    if (!thenV)
    {
        std::cerr << "Failed to generate 'then' block." << "\n";
        return nullptr;
    }

    if (context.builder.GetInsertBlock()->getTerminator() == nullptr)
    {
        context.builder.CreateBr(mergeBl);
    }

    // if (!dynamic_cast<Return*>(ifBlock.get())) {
    //     auto block = dynamic_cast<Block*>(ifBlock.get());
    //     if (!block || !dynamic_cast<Return*>(block->statementList[0].get())) {
    //         context.builder.CreateBr(mergeBl);
    //     }
    // }

    context.builder.SetInsertPoint(elseBl);
    if (elseBlock)
    {
        llvm::Value *elseV = elseBlock->codeGeneration(context);
        if (!elseV)
        {
            std::cerr << "Failed to generate 'else' block." << "\n";
            return nullptr;
        }
        if (context.builder.GetInsertBlock()->getTerminator() == nullptr)
        {
            context.builder.CreateBr(mergeBl);
        }
    }
    else
    {
        context.builder.CreateBr(mergeBl);
    }

    context.builder.SetInsertPoint(mergeBl);

    return mergeBl;
}

llvm::Value *ForLoop::codeGeneration(CodeGenContext &context)
{
    llvm::Function *function = context.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *loopHeader = llvm::BasicBlock::Create(context.llvmContext, "loopHeader", function);
    llvm::BasicBlock *loopBody = llvm::BasicBlock::Create(context.llvmContext, "loopBody", function);
    llvm::BasicBlock *loopEnd = llvm::BasicBlock::Create(context.llvmContext, "loopEnd", function);

    context.pushBlock(loopHeader);

    if (initializer)
    {
        initializer->codeGeneration(context);
    }

    context.builder.CreateBr(loopHeader);
    context.builder.SetInsertPoint(loopHeader);

    llvm::Value *condValue = condition->codeGeneration(context);
    if (!condValue)
    {
        return nullptr;
    }

    context.builder.CreateCondBr(condValue, loopBody, loopEnd);

    context.builder.SetInsertPoint(loopBody);
    if (body)
    {
        body->codeGeneration(context);
    }

    if (update)
    {
        update->codeGeneration(context);
    }

    context.builder.CreateBr(loopHeader);
    context.builder.SetInsertPoint(loopEnd);

    context.popBlock();

    return condValue; // dump value;
}

llvm::Value *WhileLoop::codeGeneration(CodeGenContext &context)
{
    llvm::Function *parentFunction = context.builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *condBlock = llvm::BasicBlock::Create(context.llvmContext, "while.cond", parentFunction);
    llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(context.llvmContext, "while.body", parentFunction);
    llvm::BasicBlock *endBlock = llvm::BasicBlock::Create(context.llvmContext, "while.end", parentFunction);

    context.builder.CreateBr(condBlock);
    context.builder.SetInsertPoint(condBlock);
    llvm::Value *condValue = condition->codeGeneration(context);
    if (!condValue)
    {
        std::cerr << "Failed to generate condition for while loop" << "\n";
    }

    if (condValue->getType()->isIntegerTy() && condValue->getType()->getIntegerBitWidth() != 1)
    {
        condValue = context.builder.CreateICmpNE(condValue, context.builder.getInt32(0), "while.cond.bool");
    }
    else if (!condValue->getType()->isIntegerTy(1))
    {
        std::cerr << "Condition of while loop must be boolean" << "\n";
        return nullptr;
    }

    context.builder.CreateCondBr(condValue, bodyBlock, endBlock);
    context.builder.SetInsertPoint(bodyBlock);
    llvm::Value *bodyValue = body->codeGeneration(context);
    context.builder.CreateBr(condBlock);
    context.builder.SetInsertPoint(endBlock);
    return nullptr;
};

llvm::Value *PrototypeFunction::codeGeneration(CodeGenContext &context)
{
    std::cout << "Prototype AST node" << "\n";
    std::vector<llvm::Type *> argTypes(args.size());
    for (int i = 0; i < args.size(); i++)
    {
        switch (args[i].first)
        {
        case INT:
            argTypes[i] = llvm::Type::getInt32Ty(context.llvmContext);
            break;
        case BIGINT:
            argTypes[i] = llvm::Type::getInt128Ty(context.llvmContext);
            break;
        case FLOAT:
            argTypes[i] = llvm::Type::getFloatTy(context.llvmContext);
            break;
        case STR:
            argTypes[i] = llvm::PointerType::getInt8Ty(context.llvmContext);
            break;
        case CHAR:
            argTypes[i] = llvm::Type::getInt8Ty(context.llvmContext);
            break;
        case BOOL:
            argTypes[i] = llvm::Type::getInt1Ty(context.llvmContext);
            break;
        }
    }

    llvm::Type *retType = nullptr;
    switch (returnType)
    {
    case INT:
        retType = llvm::Type::getInt32Ty(context.llvmContext);
        break;
    case BIGINT:
        retType = llvm::Type::getInt128Ty(context.llvmContext);
        break;
    case FLOAT:
        retType = llvm::Type::getFloatTy(context.llvmContext);
        break;
    case STR:
        retType = llvm::PointerType::getInt8Ty(context.llvmContext);
        break;
    case CHAR:
        retType = llvm::Type::getInt8Ty(context.llvmContext);
        break;
    case BOOL:
        retType = llvm::Type::getInt1Ty(context.llvmContext);
        break;
    case VOID:
        retType = llvm::Type::getVoidTy(context.llvmContext);
        break;
    default:
        return nullptr;
    }

    std::cout << "Function args types are chosen" << "\n";
    llvm::FunctionType *funcType = llvm::FunctionType::get(retType, argTypes, false);
    
    std::cout << "Create" << "\n";
    llvm::Function *func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, context.module.get());


    std::cout << "Index" << "\n";
    unsigned index = 0;
    for (auto &arg : func->args())
        arg.setName(args[index++].second);

    return func;
}

llvm::Value *FunctionNode::codeGeneration(CodeGenContext &context)
{
    llvm::Function *function = static_cast<llvm::Function *>(prototype->codeGeneration(context));
    if (!function)
    {
        std::cerr << "Failed to generate function prototype" << "\n";
        return nullptr;
    }

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context.llvmContext, "entry", function);
    context.builder.SetInsertPoint(block);
    context.pushBlock(block);

    for (auto &arg : function->args())
    {
        llvm::Type *expectedArgType = arg.getType();
        llvm::Value *argValue = &arg;
        llvm::AllocaInst *alloc = context.builder.CreateAlloca(expectedArgType, nullptr, arg.getName());

        if (argValue->getType() != expectedArgType)
        {
            if (expectedArgType->isIntegerTy() && argValue->getType()->isIntegerTy())
            {
                unsigned expectedBits = expectedArgType->getIntegerBitWidth();
                unsigned argBits = argValue->getType()->getIntegerBitWidth();

                if (argBits < expectedBits)
                {
                    if (argBits == 1)
                    {                                                                                                      
                        argValue = context.builder.CreateIntCast(argValue, expectedArgType, true, "arg_cast_bool_to_int"); 
                    }
                    else
                    {
                        argValue = context.builder.CreateSExt(argValue, expectedArgType, "arg_cast_Sext"); 
                    }
                }
                else
                {                                                                                        
                    argValue = context.builder.CreateTrunc(argValue, expectedArgType, "arg_cast_trunc"); 
                }
            }
            else if (expectedArgType->isFloatingPointTy() && argValue->getType()->isIntegerTy())
            {
                argValue = context.builder.CreateSIToFP(argValue, expectedArgType, "arg_cast");
            }
            else if (expectedArgType->isIntegerTy() && argValue->getType()->isFloatingPointTy())
            {
                argValue = context.builder.CreateFPToSI(argValue, expectedArgType, "arg_cast");
            }
            else
            {
                std::cerr << "Incompatible argument type" << std::endl;
                context.popBlock();
                return nullptr;
            }
        }
        context.builder.CreateStore(&arg, alloc);

        context.locals()[arg.getName().str()] = {alloc, arg.getType()};
    }

    llvm::Value *returnValue = bodyBlock->codeGeneration(context);
    if (!returnValue && prototype->returnType != VOID)
    {
        std::cerr << "Failed to generate function body" << "\n";
        return nullptr;
    }

    if (prototype->returnType == VOID)
    {
        context.builder.CreateRetVoid();
    }

    context.popBlock();
    llvm::verifyFunction(*function);
    return function;
}

llvm::Value *Return::codeGeneration(CodeGenContext &context)
{
    llvm::Value *returnValue = expr->codeGeneration(context);
    if (!returnValue)
    {
        std::cerr << "Nothing to return" << "\n";
        return nullptr;
    }

    llvm::Function *currentFunction = context.builder.GetInsertBlock()->getParent();
    if (!currentFunction)
    {
        std::cerr << "Not in a function" << "\n";
        return nullptr;
    }

    llvm::Type *returnType = currentFunction->getReturnType();
    if (returnType != returnValue->getType())
    {
        if (returnType->isIntegerTy() && returnValue->getType()->isIntegerTy())
        {
            unsigned returnBits = returnType->getIntegerBitWidth();
            unsigned valueBits = returnValue->getType()->getIntegerBitWidth();

            if (valueBits < returnBits)
            {
                returnValue = context.builder.CreateSExt(returnValue, returnType, "extended_return_value");
            }
            else if (valueBits > returnBits)
            {
                returnValue = context.builder.CreateTrunc(returnValue, returnType, "truncated_return_value");
            }
        }
        else if (returnType->isFloatingPointTy() && returnValue->getType()->isIntegerTy())
        {
            returnValue = context.builder.CreateSIToFP(returnValue, returnType, "int_to_fp");
        }
        else if (returnType->isIntegerTy() && returnValue->getType()->isFloatingPointTy())
        {
            returnValue = context.builder.CreateFPToSI(returnValue, returnType, "fp_to_int");
        }

        else
        {
            std::cerr << "Incompatible return type" << std::endl;
            return nullptr;
        }
    }

    context.builder.CreateRet(returnValue);
    return returnValue;
}
