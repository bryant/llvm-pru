#pragma once

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "registerinfo.inc"

namespace llvm {

struct PRURegisterInfo : public PRUGenRegisterInfo {
    enum RegSize { Byte = 1, Word = 2, DWord = 4 };

    struct RegInfo {
        RegSize size;
        unsigned offset;
    };

    static const std::vector<RegInfo> reginfos;

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

    unsigned find_subreg_in(unsigned reg, unsigned offset, unsigned bits) const;

    const std::vector<MCPhysReg> i8_arg_regs() const;

    const std::vector<MCPhysReg> i16_arg_regs() const;

    const std::vector<MCPhysReg> i32_arg_regs() const;

    template <typename T, size_t n>
    static constexpr size_t len(const T (&xs)[n]) {
        return n;
    }

    static std::vector<RegInfo> build_infos();

    static unsigned reg_at_pos(unsigned offset, RegSize size);

    static RegSize reg_size(unsigned reg) { return reginfos[reg].size; }
    static unsigned  reg_size_bits(unsigned reg) { return reg_size(reg) * 8; }

    static unsigned reg_offset(unsigned reg) { return reginfos[reg].offset; }

    static bool are_adjacent(unsigned reg0, unsigned reg1) {
        return reg_size(reg0) + reg_offset(reg0) == reg_offset(reg1) ||
               reg_offset(reg1) + reg_size(reg1) == reg_offset(reg0);
    }
};
}
