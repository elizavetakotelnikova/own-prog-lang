#include <iostream>
#include "include/CodeGenContext.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/raw_ostream.h>

void CodeGenContext::generateCode(std::vector<std::unique_ptr<ASTNode>> nodeList)
{
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(llvmContext), {}, false);
    mainFunction = std::unique_ptr<llvm::Function>(llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module.get()));

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(llvmContext, "entry", mainFunction.get());
    builder.SetInsertPoint(entry);

    pushBlock(std::unique_ptr<llvm::BasicBlock>(entry));

    int i = 0;
    for (const auto &node : nodeList)
    {
        if (!node->codeGeneration(*this))
        {
            std::cerr << "Code generation failed for AST node No." << i << std::endl;
        }
        i++;
    }
    
    builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvmContext), 0));
    module->print(llvm::outs(), nullptr);
    popBlock();
}

llvm::GenericValue CodeGenContext::runCode()
{
    std::cout << "Running code...";

    gcManager.collectGarbage();

    llvm::ExecutionEngine *ee = llvm::EngineBuilder(std::move(module)).create();
    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(mainFunction.get(), noargs);

    gcManager.collectGarbage();

    std::cout << "Code was run";
    return v;
}