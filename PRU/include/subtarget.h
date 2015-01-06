#pragma once

#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetSelectionDAGInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include <string>

#include "framelowering.h"
#include "targetlowering.h"
#include "instrinfo.h"
#include "registerinfo.h"

#define GET_SUBTARGETINFO_HEADER
#include "subtargetinfo.inc"

namespace llvm {
class StringRef;

class PRUSubtarget : public PRUGenSubtargetInfo {
    virtual void anchor();
    PRUFrameLowering FrameLowering;
    PRUInstrInfo InstrInfo;
    PRUTargetLowering TLInfo;
    TargetSelectionDAGInfo TSInfo;

  public:
    /// This constructor initializes the data members to match that
    /// of the specified triple.
    ///
    PRUSubtarget(const Triple &TT, const std::string &CPU,
                 const std::string &FS, const TargetMachine &TM);

    PRUSubtarget &initializeSubtargetDependencies(StringRef CPU, StringRef FS);

    /// ParseSubtargetFeatures - Parses features string setting specified
    /// subtarget options.  Definition of function is auto generated by tblgen.
    void ParseSubtargetFeatures(StringRef CPU, StringRef FS);

    const TargetFrameLowering *getFrameLowering() const override {
        return &FrameLowering;
    }
    const PRUInstrInfo *getInstrInfo() const override { return &InstrInfo; }
    const TargetRegisterInfo *getRegisterInfo() const override {
        return &InstrInfo.getRegisterInfo();
    }
    const PRUTargetLowering *getTargetLowering() const override {
        return &TLInfo;
    }
    const TargetSelectionDAGInfo *getSelectionDAGInfo() const override {
        return &TSInfo;
    }
};
} // End llvm namespace
