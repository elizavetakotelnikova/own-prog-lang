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

    std::cout << "Code generated for AST node succeed" << "\n";
    builder.CreateRetVoid();
    module->print(llvm::outs(), nullptr);
}

llvm::GenericValue CodeGenContext::runCode()
{
    std::cout << "Running code..." << std::endl;

    if (!mainFunction) {
        std::cerr << "Main function not set." << std::endl;
        return llvm::GenericValue();
    }

    std::string errorMsg;
    llvm::raw_string_ostream errorStream(errorMsg);
    if (llvm::verifyModule(*module, &errorStream)) {
        std::cerr << "Module verification failed: " << errorStream.str() << std::endl;
        return llvm::GenericValue();
    }

    std::unique_ptr<llvm::ExecutionEngine> ee(llvm::EngineBuilder(std::move(module)).create());
    if (!ee) {
        std::cerr << "Failed to create ExecutionEngine." << std::endl;
        return llvm::GenericValue();
    }

    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(mainFunction.get(), noargs);
    std::cout << "Code was run." << std::endl;
    return v;
}