#pragma once

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "registerinfo.inc"

namespace llvm {

struct PRURegisterInfo : public PRUGenRegisterInfo {
  public:
    PRURegisterInfo();

    /// Code Generation virtual methods...
    const MCPhysReg *
    getCalleeSavedRegs(const MachineFunction *MF) const override;

    BitVector getReservedRegs(const MachineFunction &MF) const override;
    const TargetRegisterClass *
    getPointerRegClass(const MachineFunction &MF,
                       unsigned Kind = 0) const override;

    void eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                             unsigned FIOperandNum,
                             RegScavenger *RS = nullptr) const override;

    // Debug information queries.
    unsigned getFrameRegister(const MachineFunction &MF) const override;

    unsigned reg_size(unsigned regnum) const;

    unsigned find_subreg_in(unsigned reg, unsigned offset, unsigned bits) const;

    const std::vector<MCPhysReg> i8_arg_regs() const;

    const std::vector<MCPhysReg> i16_arg_regs() const;

    const std::vector<MCPhysReg> i32_arg_regs() const;
};
} // end namespace llvm
