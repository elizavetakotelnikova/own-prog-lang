#include <iostream>
#include "include/CodeGenContext.h"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ExecutionEngine/Orc/Core.h>

void CodeGenContext::generateCode(std::vector<std::unique_ptr<ASTNode>> nodeList) {
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(llvmContext), {}, false);
    mainFunction = std::unique_ptr<llvm::Function>(llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module.get()));
    llvm::Function* fn = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "main", module.get());

    llvm::BasicBlock *entry = llvm::BasicBlock::Create(llvmContext, "entry", fn);
    builder.SetInsertPoint(entry);
    pushBlock(entry);
    int i = 0;
    for (const auto &node : nodeList) {
        gcManager.addObject(node.get());
        llvm::BasicBlock* prevInsertPoint = nullptr;
        if (dynamic_cast<FunctionNode*>(node.get())) {
            prevInsertPoint = builder.GetInsertBlock();
        }

        if (!node->codeGeneration(*this)) {
            std::cout << "Code generation failed for AST node No." << i << ": " << node->toString() << "\n";
        } else {
            std::cout << "Succeed for AST Node: " << node->toString() << "\n";
        }

        if (dynamic_cast<FunctionNode*>(node.get())) {
            builder.SetInsertPoint(prevInsertPoint);
        }

        i++;
    }

    popBlock();
    builder.CreateRetVoid();

    module->print(llvm::outs(), nullptr);
    auto TSM = llvm::orc::ThreadSafeModule(std::move(module), std::make_unique<llvm::LLVMContext>());

    if (auto Err = JIT->addModule(std::move(TSM))) {
        llvm::errs() << "Failed to add module to JIT: " << llvm::toString(std::move(Err)) << "\n";
        return;
    } else {
      	llvm::errs() << "Successfully loaded JIT\n";
      	return;
    }
}

void CodeGenContext::runCode() {
    std::cout << "Running code...\n";

    if (!JIT) {
        std::cerr << "JIT is not initialized.\n";
        return;
    }

    std::cout << "Available symbols in JIT:\n";
    auto& ES = *JIT->getExecutionSession();
    ES.dump(llvm::outs());

    auto Sym = JIT->lookup("main.1");
    if (!Sym) {
        llvm::errs() << "Failed to find 'main' function: " << llvm::toString(Sym.takeError()) << "\n";
        return;
    }

    using MainFuncType = void (*)();
    auto MainFunc = Sym->getAddress().toPtr<MainFuncType>();

    if (MainFunc) {
        std::cout << "Calling JIT-compiled 'main' function...\n";
        MainFunc();
        std::cout << "Code executed.\n";
    } else {
        std::cerr << "Failed to cast 'main' function pointer.\n";
    }
}