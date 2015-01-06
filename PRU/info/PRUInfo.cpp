#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

namespace pru {
llvm::Target target;
}

extern "C" void LLVMInitializePRUTargetInfo() {
    RegisterTarget<Triple::pru> _(pru::target, "pru", "PRU [experimental]");
}
