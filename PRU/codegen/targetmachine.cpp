#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetRegistry.h"

#include "targetmachine.h"
#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"

using namespace llvm;

static cl::opt<bool> EnableCombiner2("bbo-combiner2", cl::Hidden,
                                     cl::desc("Merge L-/SBBO ops."),
                                     cl::init(false));

namespace pru {

char omfgchrist;
class Dumd : public MachineFunctionPass {
  public:
    Dumd() : MachineFunctionPass(omfgchrist) {}

    bool runOnMachineFunction(MachineFunction &f) override {
        dbgs() << "Dumd says:\n";
        f.print(dbgs());
        dbgs() << "\n" << f.getRegInfo().getNumVirtRegs() << " virtregs\n";
        return false;
    }

    void getAnalysisUsage(AnalysisUsage &a) const override {
        a.setPreservesAll();
        return MachineFunctionPass::getAnalysisUsage(a);
    }

    const char *getPassName() const override {
        return "The Operand Printer (TM)";
    }
};

class PRUPassConfig : public TargetPassConfig {
  public:
    PRUPassConfig(PRUTargetMachine *TM, PassManagerBase &PM)
        : TargetPassConfig(TM, PM) {}

    bool addInstSelector() override {
        addPass(new_pru_isel(getTM<TargetMachine>(), getOptLevel()));
        return false;
    }

    void addPreRegAlloc() override { addPass(new_hard_loops()); }

    bool addPreRewrite() override {
        if (EnableCombiner2) {
            addPass(new_load_merger());
        }
        return false;
    }

    void addPreSched2() override { addPass(new Dumd()); }
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
