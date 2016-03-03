#pragma once

#include "subtarget.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class PRUTargetMachine : public LLVMTargetMachine {
    std::unique_ptr<TargetLoweringObjectFile> TLOF;
    PRUSubtarget Subtarget;

  public:
    PRUTargetMachine(const Target &T, const Triple &TT, StringRef CPU,
                     StringRef FS, const TargetOptions &Options,
                     Reloc::Model RM, CodeModel::Model CM,
                     CodeGenOpt::Level OL);

    const PRUSubtarget *getSubtargetImpl(const Function &F) const override {
        return &Subtarget;
    }
    TargetPassConfig *createPassConfig(PassManagerBase &PM) override;

    TargetLoweringObjectFile *getObjFileLowering() const override {
        return TLOF.get();
    }

    const DataLayout &getDataLayout() const {
        return LLVMTargetMachine::getDataLayout();
    }
};
}

namespace pru {
using namespace llvm;
FunctionPass *new_load_merger();
FunctionPass *new_pru_isel(TargetMachine &, CodeGenOpt::Level);
}
