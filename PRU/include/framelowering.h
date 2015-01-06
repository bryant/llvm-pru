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

    /*void
    eliminateCallFramePseudoInstr(MachineFunction &MF, MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I) const override;

    bool
    spillCalleeSavedRegisters(MachineBasicBlock &MBB,
                              MachineBasicBlock::iterator MI,
                              const std::vector<CalleeSavedInfo> &CSI,
                              const TargetRegisterInfo *TRI) const override;
    bool
    restoreCalleeSavedRegisters(MachineBasicBlock &MBB,
                                MachineBasicBlock::iterator MI,
                                const std::vector<CalleeSavedInfo> &CSI,
                                const TargetRegisterInfo *TRI) const override;
*/

    bool hasFP(const MachineFunction &MF) const override;
    /*bool hasReservedCallFrame(const MachineFunction &MF) const override;
    void processFunctionBeforeFrameFinalized(
        MachineFunction &MF, RegScavenger *RS = nullptr) const override;
        */
};

} // End llvm namespace
