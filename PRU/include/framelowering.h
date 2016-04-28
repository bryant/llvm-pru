#pragma once

#include "llvm/Target/TargetFrameLowering.h"

namespace llvm {
class PRUFrameLowering : public TargetFrameLowering {
  protected:
  public:
    explicit PRUFrameLowering()
        : TargetFrameLowering(TargetFrameLowering::StackGrowsDown, 1, 0, 1) {}

    void emitPrologue(MachineFunction &, MachineBasicBlock &) const override;
    void emitEpilogue(MachineFunction &, MachineBasicBlock &) const override;
    bool hasFP(const MachineFunction &MF) const override;
};

} // End llvm namespace
