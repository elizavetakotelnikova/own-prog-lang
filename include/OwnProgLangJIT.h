#pragma once

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/EPCIndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/Orc/Shared/ExecutorSymbolDef.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "AST.h"
#include <memory>

extern "C" void ___chkstk_ms();

namespace llvm {
    namespace orc {
        class OwnProgLangJIT {
        private:
            std::unique_ptr<ExecutionSession> ES;
            RTDyldObjectLinkingLayer ObjectLayer;
            IRCompileLayer CompileLayer;
            IRTransformLayer TransformLayer;

            DataLayout DL;
            MangleAndInterner Mangle;
            ThreadSafeContext Ctx;

            JITDylib &MainJD;

        public:
            OwnProgLangJIT(std::unique_ptr<ExecutionSession> ES, JITTargetMachineBuilder JTMB, DataLayout DL)
                : ES(std::move(ES)), DL(std::move(DL)), Mangle(*this->ES, this->DL),
                  ObjectLayer(*this->ES, []() { return std::make_unique<SectionMemoryManager>(); }),
                  CompileLayer(*this->ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
                  TransformLayer(*this->ES, CompileLayer, optimizeModule),
                  MainJD(this->ES->createBareJITDylib("<main>")) {
                MainJD.addGenerator(
                    cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(DL.getGlobalPrefix())));
                registerChkStkMsSymbol();
            }

            static Expected<std::unique_ptr<OwnProgLangJIT> > Create();

            Error addModule(ThreadSafeModule TSM, ResourceTrackerSP RT = nullptr);

            llvm::Expected<llvm::orc::ExecutorSymbolDef> lookup(llvm::StringRef Name);

            const llvm::DataLayout &getDataLayout() const { return DL; }
            JITDylib &getMainJITDylib() { return MainJD; }
            std::unique_ptr<ExecutionSession> &getExecutionSession() { return ES; }

        private:
            static Expected<ThreadSafeModule> optimizeModule(ThreadSafeModule TSM,
                                                             const MaterializationResponsibility &R);

            void registerChkStkMsSymbol() {
                llvm::orc::SymbolMap Symbols;

                Symbols[Mangle("___chkstk_ms")] = {llvm::orc::ExecutorAddr::fromPtr(&___chkstk_ms),
                                                llvm::JITSymbolFlags::Exported};

                cantFail(MainJD.define(llvm::orc::absoluteSymbols(std::move(Symbols))));
            }

        };
    }
}
