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
    pushBlock(entry);
    int i = 0;
    for (const auto &node : nodeList)
    {
        llvm::BasicBlock* prevInsertPoint = nullptr;
        if (dynamic_cast<FunctionNode*>(node.get())) {
            prevInsertPoint = builder.GetInsertBlock();
        }

        if (!node->codeGeneration(*this))
        {
            std::cout << "Code generation failed for AST node No." << i << " " << node->toString() << "\n";
        }
        else {
            std::cout << "Succeed for AST Node: " << node->toString() << "\n";
        }

        if (dynamic_cast<FunctionNode*>(node.get())) {
            builder.SetInsertPoint(prevInsertPoint);
        }

        i++;
    }

    std::cout << "Code generated for AST node succeed" << "\n";
    builder.CreateRetVoid();
    module->print(llvm::outs(), nullptr);
}

llvm::GenericValue CodeGenContext::runCode()
{
    std::cout << "Running code..." << "\n";

    if (!mainFunction) {
        std::cerr << "Main function not set." << "\n";
        return llvm::GenericValue();
    }

    if (llvm::verifyModule(*module, &llvm::errs())) {
        std::cerr << "Module verification failed." << "\n";
        return llvm::GenericValue();
    }

    std::unique_ptr<llvm::ExecutionEngine> ee(llvm::EngineBuilder(std::move(module)).create());
    if (!ee) {
        std::cerr << "Failed to create ExecutionEngine." << "\n";
        return llvm::GenericValue();
    }

    std::vector<llvm::GenericValue> noargs;
    llvm::GenericValue v = ee->runFunction(mainFunction.get(), noargs);
    std::cout << "Code was run." << "\n";
    return v;
}
