#include "llvm/Support/TargetRegistry.h"

#include "subtarget.h"

using namespace llvm;

#define DEBUG_TYPE "pru-subtarget"

#define GET_SUBTARGETINFO_TARGET_DESC
#define GET_SUBTARGETINFO_CTOR
#include "subtargetinfo.inc"

void PRUSubtarget::anchor() {}

PRUSubtarget &PRUSubtarget::initializeSubtargetDependencies(StringRef CPU,
                                                            StringRef FS) {
    ParseSubtargetFeatures("generic", FS);
    return *this;
}

PRUSubtarget::PRUSubtarget(const Triple &TT, const std::string &CPU,
                           const std::string &FS, const TargetMachine &TM)
    : PRUGenSubtargetInfo(TT, CPU, FS), FrameLowering(),
      InstrInfo(initializeSubtargetDependencies(CPU, FS)), TLInfo(TM, *this) {}
