#ifndef OWN_PROG_LANG_JIT_H
#define OWN_PROG_LANG_JIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

namespace llvm {
namespace orc {

class OwnProgLangJIT {
private:
  ExecutionSession ES;
  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;

  DataLayout DL;
  MangleAndInterner Mangle;
  ThreadSafeContext Ctx;

  JITDylib &MainJD;

public:
  OwnProgLangJIT(JITTargetMachineBuilder JTMB, DataLayout DL)
      : ObjectLayer(ES,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
        CompileLayer(ES, ObjectLayer, ConcurrentIRCompiler(std::move(JTMB))),
        DL(std::move(DL)), Mangle(ES, this->DL),
        Ctx(std::make_unique<LLVMContext>()) {
    ES.getMainJITDylib().addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
  }
  static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
      auto EPC = SelfExecutorProcessControl::Create();
      if (!EPC)
          return EPC.takeError();

      auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

      JITTargetMachineBuilder JTMB(
          ES->getExecutorProcessControl().getTargetTriple());

      auto DL = JTMB.getDefaultDataLayoutForTarget();
      if (!DL)
          return DL.takeError();

      return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB),
                                               std::move(*DL));
  }

  void addModule(std::unique_ptr<llvm::Module> M);
  llvm::Expected<llvm::JITEvaluatedSymbol> lookup(llvm::StringRef Name);
  const llvm::DataLayout &getDataLayout() const { return DL; }



#endif
