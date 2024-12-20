#include "include/AST.h"
#include "include/CodeGenContext.h"
#include "llvm/IR/Constants.h"

llvm::Value *IntegerLiteral::codeGeneration(CodeGenContext &context)
{
    return llvm::ConstantInt::get(llvm::Type::getInt64Ty(context.llvmContext), value);
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
    std::cout << "Identifier AST" << "\n";
    auto locals = context.locals();
    if (locals.find(value) == locals.end())
    {
        std::cerr << "Undefined identifier: " << value << std::endl;
        return nullptr;
    }

    std::cout << "Found variable " << value << "\n";
    llvm::Type *type = locals[value]->getType();
    type->dump();
    return locals[value];
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
            std::cerr << "Operand of NOT must be an integer type" << std::endl;
            return nullptr;
        }
        return context.builder.CreateNot(operandValue, "not");
    default:
        std::cerr << "Unknown unary operator" << std::endl;
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

    if (rightValue->getType() != leftValue->getType())
    {
        // since we only support double and int, always cast to double in case of different types.
        auto doubleTy = llvm::Type::getDoubleTy(context.llvmContext);
        rightValue = context.builder.CreateCast(llvm::Instruction::CastOps::SIToFP, rightValue, doubleTy, "castdb");
        leftValue = context.builder.CreateCast(llvm::Instruction::CastOps::SIToFP, leftValue, doubleTy, "castdb");
    }

    bool isDoubleTy = rightValue->getType()->isFloatingPointTy();
    if (isDoubleTy && (_operator.type == LOGICAL_AND || _operator.type == LOGICAL_OR))
    {
        std::cerr << "Binary operation (AND, OR) on floating point value is not supported" << std::endl;
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
        std::cerr << "Unknown binary operator" << std::endl;
        return nullptr;
    }
    return context.builder.CreateBinOp(instr, leftValue, rightValue, "mathtmp");
}

llvm::Value *CallFunction::codeGeneration(CodeGenContext &context)
{
    llvm::Function *CalleeF = context.module->getFunction(functionName);
    if (!CalleeF)
        std::cerr << "Unknown function referenced" << std::endl;
        return nullptr;

    if (CalleeF->arg_size() != functionArgs.size())
        std::cerr << "Missing arguments" << std::endl;
        return nullptr;

    std::vector<llvm::Value *> ArgsV;
    for (auto &arg : functionArgs)
    {
        ArgsV.push_back(arg->codeGeneration(context));
        if (!ArgsV.back())
            return nullptr;
    }

    return context.builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Value *ArrayAccess::codeGeneration(CodeGenContext &context)
{
    llvm::Value *arrayPtr = context.locals()[identifier->value];
    if (!arrayPtr)
    {
        std::cerr << "Array " << identifier->value << " not declared." << std::endl;
        return nullptr;
    }

    llvm::Type *arrayType = arrayPtr->getType();
    if (!arrayType->isArrayTy())
    {
        std::cerr << identifier->value << " is not an array." << std::endl;
        return nullptr;
    }

    llvm::Type *elementType = arrayType->getArrayElementType();

    llvm::Value *indexValue = context.builder.getInt32(index);
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
        std::cerr << "Unsupported array type." << std::endl;
        return nullptr;
    }
    llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, size);
    llvm::AllocaInst *arrayAlloc = context.builder.CreateAlloca(arrayType, nullptr, identifier->value);
    context.locals()[identifier->value] = arrayAlloc;

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
    llvm::Type *llvmType = nullptr;
    switch (type)
    {
    case TokenType::INT:
        llvmType = llvm::Type::getInt32Ty(context.llvmContext);
        std::cout << "Assigned type - int32" << "\n";
        break;
    case TokenType::FLOAT:
        llvmType = llvm::Type::getFloatTy(context.llvmContext);
            std::cout << "Assigned type - float" << "\n";
        break;
    case TokenType::CHAR:
        llvmType = llvm::Type::getInt8Ty(context.llvmContext);
            std::cout << "Assigned type - char" << "\n";
        break;
    case TokenType::STR:
        llvmType = llvm::PointerType::getInt8Ty(context.llvmContext);
            std::cout << "Assigned type - str" << "\n";
        break;
    case TokenType::BOOL:
        llvmType = llvm::Type::getInt1Ty(context.llvmContext);
            std::cout << "Assigned type - boolean" << "\n";
        break;
    }

    if (!llvmType)
    {
        std::cerr << "Unsupported variable type" << std::endl;
        return nullptr;
    }

    llvm::AllocaInst *allocaInst = context.builder.CreateAlloca(llvmType, nullptr, identifier->value);

    if (initValue)
    {
        llvm::Value *initVal = initValue->codeGeneration(context);

        if (initVal->getType() != llvmType)
        {
            if (llvmType->isIntegerTy() && initVal->getType()->isIntegerTy())
            {
                initVal = context.builder.CreateIntCast(initVal, llvmType, true, "cast");
            }
            else if (llvmType->isFloatingPointTy() && initVal->getType()->isIntegerTy())
            {
                initVal = context.builder.CreateSIToFP(initVal, llvmType, "int_to_float");
            }
            else if (llvmType->isIntegerTy() && initVal->getType()->isFloatingPointTy())
            {
                initVal = context.builder.CreateFPToSI(initVal, llvmType, "float_to_int");
            }
            else
            {
                std::cerr << "Type mismatch in variable initialization" << std::endl;
                return nullptr;
            }
        }

        context.builder.CreateStore(initVal, allocaInst);
    }

    context.locals()[identifier->value] = allocaInst;
    std::cout << "AllocatedInst type: ";
    allocaInst->getType()->dump();
    return allocaInst;
}

llvm::Value *Assignment::codeGeneration(CodeGenContext &context)
{
    llvm::Value *varPtr = identifier->codeGeneration(context);

    llvm::Value *exprValue = value->codeGeneration(context);

    llvm::Type *varType = varPtr->getType();
    if (exprValue->getType() != varType)
    {
        if (varType->isIntegerTy() && exprValue->getType()->isIntegerTy())
        {
            exprValue = context.builder.CreateIntCast(exprValue, varType, true, "cast");
        }
        else if (varType->isFloatingPointTy() && exprValue->getType()->isIntegerTy())
        {
            exprValue = context.builder.CreateSIToFP(exprValue, varType, "int_to_float");
        }
        else if (varType->isIntegerTy() && exprValue->getType()->isFloatingPointTy())
        {
            exprValue = context.builder.CreateFPToSI(exprValue, varType, "float_to_int");
        }
        else
        {
            std::cerr << "Type mismatch in assignment" << std::endl;
            return nullptr;
        }
    }

    context.builder.CreateStore(exprValue, varPtr);
    return exprValue;
}

llvm::Value *Print::codeGeneration(CodeGenContext &context)
{
    std::cout << "Expression in Print AST node: " << expr->toString() << "\n";
    llvm::Value *value = expr->codeGeneration(context);
    if (!value)
    {
        return nullptr;
    }
    std::cout << "Value: ";
    value->getType()->dump();
    llvm::Type *valueType = value->getType();
    llvm::FunctionType *printfType = llvm::FunctionType::get(
        context.builder.getInt32Ty(),
        llvm::PointerType::get(context.builder.getInt8Ty(), 0),
        true);

    llvm::FunctionCallee printfFunc = context.module->getOrInsertFunction("printf", printfType);

    llvm::Value *formatStr = nullptr;
    if (llvm::PointerType *pointerType = llvm::dyn_cast<llvm::PointerType>(valueType)) {
        if (pointerType->isIntegerTy(32))
        {
            std::cout << "Integer" << "\n";
            formatStr = context.builder.CreateGlobalStringPtr("%d\n", "formatStr");
        }
        else if (pointerType->isFloatingPointTy())
        {
            formatStr = context.builder.CreateGlobalStringPtr("%f\n", "formatStr");
        }
        else if (pointerType->isIntegerTy(1))
        {
            formatStr = context.builder.CreateGlobalStringPtr("%d\n", "formatStr");
        }
        else if (pointerType->isIntegerTy(8))
        {
            formatStr = context.builder.CreateGlobalStringPtr("%c\n", "formatStr");
        }
        else
        {
            std::cout << "Only ptr" << "\n";
            formatStr = context.builder.CreateGlobalStringPtr("%s\n", "formatStr");
        }
    }
    else
    {
        std::cout << "Nullptr" << "\n";
        return nullptr;
    }

    return context.builder.CreateCall(printfFunc, {formatStr, value}, "printCall");
};

llvm::Value *Block::codeGeneration(CodeGenContext &context)
{
    llvm::Value *lastValue = nullptr;
    llvm::Function *function = context.builder.GetInsertBlock()->getParent();
    std::unique_ptr<llvm::BasicBlock> basicBlock(llvm::BasicBlock::Create(context.llvmContext, "block", function, 0));
    context.pushBlock(std::move(basicBlock));

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
    llvm::Value *condition = ifBlock->codeGeneration(context);
    if (!condition)
        return nullptr;

    condition = context.builder.CreateFCmpONE(condition, context.builder.getInt32(0), "ifcond");

    llvm::Function *currFunc = context.builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *thenBl = llvm::BasicBlock::Create(context.llvmContext, "then", currFunc);
    llvm::BasicBlock *elseBl = llvm::BasicBlock::Create(context.llvmContext, "else");
    llvm::BasicBlock *mergeBl = llvm::BasicBlock::Create(context.llvmContext, "ifcont");

    context.builder.CreateCondBr(condition, thenBl, elseBl);

    context.builder.SetInsertPoint(thenBl);

    llvm::Value *thenV = ifBlock->codeGeneration(context);
    if (!thenV)
        return nullptr;

    context.builder.CreateBr(mergeBl);
    thenBl = context.builder.GetInsertBlock();

    currFunc->insert(currFunc->end(), elseBl);
    context.builder.SetInsertPoint(elseBl);

    llvm::Value *elseV = elseBlock->codeGeneration(context);
    if (!elseV)
        return nullptr;

    context.builder.CreateBr(mergeBl);
    elseBl = context.builder.GetInsertBlock();

    currFunc->insert(currFunc->end(), mergeBl);
    context.builder.SetInsertPoint(mergeBl);
    llvm::PHINode *PN = context.builder.CreatePHI(llvm::Type::getDoubleTy(context.llvmContext), 2, "iftmp");

    PN->addIncoming(thenV, thenBl);
    PN->addIncoming(elseV, elseBl);
    return PN;
}

llvm::Value *ForLoop::codeGeneration(CodeGenContext &context)
{
    llvm::Function *function = context.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *loopHeader = llvm::BasicBlock::Create(context.llvmContext, "loopHeader", function);
    llvm::BasicBlock *loopBody = llvm::BasicBlock::Create(context.llvmContext, "loopBody", function);
    llvm::BasicBlock *loopEnd = llvm::BasicBlock::Create(context.llvmContext, "loopEnd", function);

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

    return nullptr;
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
        std::cerr << "Failed to generate condition for while loop" << std::endl;
    }

    if (condValue->getType()->isIntegerTy() && condValue->getType()->getIntegerBitWidth() != 1)
    {
        condValue = context.builder.CreateICmpNE(condValue, context.builder.getInt32(0), "while.cond.bool");
    }
    else if (!condValue->getType()->isIntegerTy(1))
    {
        std::cerr << "Condition of while loop must be boolean" << std::endl;
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
    std::vector<llvm::Type *> argTypes(args.size());
    for (int i = 0; i < args.size(); i++)
    {
        switch (args[i].first){
            case INT:
                argTypes[i] = llvm::Type::getInt32Ty(context.llvmContext);
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
    switch (returnType){
        case INT:
            retType = llvm::Type::getInt32Ty(context.llvmContext);
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
    llvm::FunctionType *funcType = llvm::FunctionType::get(retType, argTypes, false);
    llvm::Function *func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, context.module.get());

    unsigned index = 0;
    for (auto &arg : func->args())
        arg.setName(args[index++].second);

    return func;
}

llvm::Value *FunctionNode::codeGeneration(CodeGenContext &context)
{
    llvm::Function* function = static_cast<llvm::Function*>(proto->codeGeneration(context));
    if (!function) {
        std::cerr << "Failed to generate function prototype" << std::endl;
        return nullptr;
    }

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context.llvmContext, "entry", function);
    context.builder.SetInsertPoint(block);

    context.pushBlock(std::unique_ptr<llvm::BasicBlock>(block));

    for (auto &arg : function->args())
    {
        llvm::AllocaInst* alloc = context.builder.CreateAlloca(arg.getType(), nullptr, arg.getName());
        
        context.builder.CreateStore(&arg, alloc);

        context.locals()[arg.getName().str()] = alloc;
    }

    if (proto->returnType == VOID) {
        context.builder.CreateRetVoid();
    } else {
        if (llvm::Value *returnValue = body->codeGeneration(context)) {
            context.builder.CreateRet(returnValue);
        } else {
            function->eraseFromParent();
            return nullptr;
        }
    }

    llvm::verifyFunction(*function);
    return function;
}

llvm::Value *Return::codeGeneration(CodeGenContext &context)
{
    llvm::Value *returnValue = expr->codeGeneration(context);
    if (!returnValue)
    {
        return nullptr;
    }

    llvm::Function *currentFunction = context.builder.GetInsertBlock()->getParent();
    if (!currentFunction)
    {
        return nullptr;
    }

    llvm::Type *returnType = currentFunction->getReturnType();
    if (returnType != returnValue->getType())
    {
        returnValue = context.builder.CreateBitCast(returnValue, returnType, "casted_return_value");
    }

    context.builder.CreateRet(returnValue);

    return nullptr;
}
