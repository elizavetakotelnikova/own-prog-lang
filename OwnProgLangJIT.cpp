#include "include/OwnProgLangJIT.h"
#include "llvm/Support/Error.h"

llvm::LLVMContext TheContext;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;

llvm::orc::ThreadSafeModule irgenAndTakeOwnership(Function &FnAST, const std::string &Suffix) {
    FnAST.codeGeneration();
    return llvm::orc::ThreadSafeModule(std::move(TheModule), std::make_unique<llvm::LLVMContext>());
}

Expected<std::unique_ptr<OwnProgLangJIT>> OwnProgLangJIT::Create() {
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
        return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    auto EPCIU = EPCIndirectionUtils::Create(*ES);
    if (!EPCIU)
        return EPCIU.takeError();

    (*EPCIU)->createLazyCallThroughManager(
        *ES, ExecutorAddr::fromPtr(&handleLazyCallThroughError));

    if (auto Err = setUpInProcessLCTMReentryViaEPCIU(**EPCIU))
        return std::move(Err);

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
        return DL.takeError();

    return std::make_unique<OwnProgLangJIT>(std::move(ES), std::move(JTMB),
                                           std::move(*DL));
}

Error OwnProgLangJIT::addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();

    return TransformLayer.add(RT, std::move(TSM));
}

Error OwnProgLangJIT::addAST(std::unique_ptr<Function> F, ResourceTrackerSP RT = nullptr) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();
    return ASTLayer.add(RT, std::move(F));
}

Expected<llvm::JITEvaluatedSymbol> OwnProgLangJIT::lookup(llvm::StringRef Name) {
    return ES->lookup({&MainJD}, Mangle(Name.str()));
}

Expected<ThreadSafeModule> OwnProgLangJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);
        FPM->add(createInstructionCombiningPass());
        FPM->add(createReassociatePass());
        FPM->add(createGVNPass());
        FPM->add(createCFGSimplificationPass());
        FPM->add(llvm::createLoopUnrollPass());
        FPM->add(llvm::createLoopSimplifyPass());
        FPM->add(llvm::createLoopDeletionPass());
        FPM->add(llvm::createFunctionInliningPass());
        FPM->add(llvm::createTailCallEliminationPass());
        FPM->doInitialization();

        for (auto &F : M)
            FPM->run(F);
    });
    return std::move(M);
}

void OwnProgLangASTMaterializationUnit::materialize(std::unique_ptr<MaterializationResponsibility> R) {
    L.emit(std::move(R), std::move(F));
}

void OwnProgLangASTLayer::emit(std::unique_ptr<MaterializationResponsibility> MR, std::unique_ptr<Function> F) {
    BaseLayer.emit(std::move(MR), irgenAndTakeOwnership(*F, ""));
}

MaterializationUnit::Interface OwnProgLangASTLayer::getInterface(Function &F) {
    MangleAndInterner Mangle(BaseLayer.getExecutionSession(), DL);
    SymbolFlagsMap Symbols;

    Symbols[Mangle(F.getName())] = JITSymbolFlags(JITSymbolFlags::Exported | JITSymbolFlags::Callable);
    return MaterializationUnit::Interface(std::move(Symbols), nullptr);
}

Error OwnProgLangASTLayer::add(ResourceTrackerSP RT, std::unique_ptr<Function> F) {
    return RT->getJITDylib().define(std::make_unique<OwnProgLangASTMaterializationUnit>(*this, std::move(F)), RT);
}

void InitializeModule() {
    TheContext = llvm::LLVMContext();
    TheModule = std::make_unique<llvm::Module>("OwnProgLang", TheContext);
    Builder = std::make_unique<llvm::IRBuilder<>>(TheContext);
}
