#include "include/OwnProgLangJIT.h"
#include "llvm/Support/Error.h"

llvm::LLVMContext TheContext;
CodeGenContext TheCodeGen;
std::unique_ptr<llvm::Module> TheModule;
std::unique_ptr<llvm::IRBuilder<>> Builder;

llvm::orc::ThreadSafeModule irgenAndTakeOwnership(FunctionNode &FnAST, const std::string &Suffix) {
    FnAST.codeGeneration(TheCodeGen);
    return llvm::orc::ThreadSafeModule(std::move(TheModule), std::make_unique<llvm::LLVMContext>());
}

llvm::Expected<std::unique_ptr<llvm::orc::OwnProgLangJIT>> llvm::orc::OwnProgLangJIT::Create() {
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

llvm::Error llvm::orc::OwnProgLangJIT::addModule(ThreadSafeModule TSM, ResourceTrackerSP RT) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();

    return TransformLayer.add(RT, std::move(TSM));
}

llvm::Error llvm::orc::OwnProgLangJIT::addAST(std::unique_ptr<FunctionNode> F, ResourceTrackerSP RT) {
    if (!RT)
        RT = MainJD.getDefaultResourceTracker();
    return ASTLayer.add(RT, std::move(F));
}

llvm::Expected<llvm::orc::ExecutorSymbolDef> llvm::orc::OwnProgLangJIT::lookup(StringRef Name) {
    return ES->lookup({&MainJD}, Mangle(Name.str()));
}

llvm::Expected<llvm::orc::ThreadSafeModule> llvm::orc::OwnProgLangJIT::optimizeModule(ThreadSafeModule TSM, const MaterializationResponsibility &R) {
    TSM.withModuleDo([](Module &M) {
        auto FPM = std::make_unique<legacy::FunctionPassManager>(&M);
        FPM->add(createInstructionCombiningPass());
        FPM->add(createReassociatePass());
        FPM->add(createGVNPass());
        FPM->add(createCFGSimplificationPass());
        FPM->add(createLoopUnrollPass());
        FPM->add(createTailCallEliminationPass());
        FPM->doInitialization();

        for (auto &F : M)
            FPM->run(F);
    });
    return std::move(TSM);
}

void llvm::orc::OwnProgLangASTMaterializationUnit::materialize(std::unique_ptr<MaterializationResponsibility> R) {
    L.emit(std::move(R), std::move(F));
}

void llvm::orc::OwnProgLangASTLayer::emit(std::unique_ptr<MaterializationResponsibility> MR, std::unique_ptr<FunctionNode> F) {
    BaseLayer.emit(std::move(MR), irgenAndTakeOwnership(*F, ""));
}

llvm::orc::MaterializationUnit::Interface llvm::orc::OwnProgLangASTLayer::getInterface(FunctionNode &F) {
    MangleAndInterner Mangle(BaseLayer.getExecutionSession(), DL);
    SymbolFlagsMap Symbols;

    Symbols[Mangle(F.proto->name)] = JITSymbolFlags(JITSymbolFlags::Exported | JITSymbolFlags::Callable);
    return MaterializationUnit::Interface(std::move(Symbols), nullptr);
}

llvm::Error llvm::orc::OwnProgLangASTLayer::add(ResourceTrackerSP RT, std::unique_ptr<FunctionNode> F) {
    return RT->getJITDylib().define(std::make_unique<OwnProgLangASTMaterializationUnit>(*this, std::move(F)), RT);
}