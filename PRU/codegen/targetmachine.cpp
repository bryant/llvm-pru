#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"

#include "targetmachine.h"

using namespace llvm;

static cl::opt<bool> EnableCombiner2("bbo-combiner2", cl::Hidden,
                                     cl::desc("Merge L-/SBBO ops."),
                                     cl::init(false));

namespace pru {

class PRUPassConfig : public TargetPassConfig {
  public:
    PRUPassConfig(PRUTargetMachine *TM, PassManagerBase &PM)
        : TargetPassConfig(TM, PM) {}

    bool addInstSelector() override {
        addPass(new_pru_isel(getTM<TargetMachine>(), getOptLevel()));
        return false;
    }

    bool addPreRewrite() override {
        if (EnableCombiner2) {
            addPass(new_load_merger());
        }
        return false;
    }
};

extern Target target;
}

PRUTargetMachine::PRUTargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Reloc::Model RM, CodeModel::Model CM,
                                   CodeGenOpt::Level OL)
    : LLVMTargetMachine(T,
                        "e-S8-p:32:8:8-i32:8:8-i16:8:8-i8:8:8-a:8:8-n8:16:32",
                        TT, CPU, FS, Options, RM, CM, OL),
      TLOF(make_unique<TargetLoweringObjectFileELF>()),
      Subtarget(TT, CPU, FS, *this) {
    initAsmInfo();
}

TargetPassConfig *PRUTargetMachine::createPassConfig(PassManagerBase &PM) {
    return new pru::PRUPassConfig(this, PM);
}

extern "C" void LLVMInitializePRUTarget() {
    RegisterTargetMachine<PRUTargetMachine> _(pru::target);
}
