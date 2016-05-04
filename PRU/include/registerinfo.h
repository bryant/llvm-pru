#pragma once

#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "registerinfo.inc"

#include "targetdesc.h"

namespace llvm {

struct PRURegisterInfo : public PRUGenRegisterInfo {
    enum struct RegSize { Byte = 1, Word = 2, DWord = 4 };

    struct RegInfo {
        RegSize size;
        unsigned offset;
    };

    static const std::array<RegInfo, PRU::NUM_TARGET_REGS> reginfos;

    PRURegisterInfo();

    /// Code Generation virtual methods...
    const MCPhysReg *
    getCalleeSavedRegs(const MachineFunction *MF) const override;

    const uint32_t *getCallPreservedMask(const MachineFunction &,
                                         CallingConv::ID) const override;

    BitVector getReservedRegs(const MachineFunction &MF) const override;
    const TargetRegisterClass *
    getPointerRegClass(const MachineFunction &MF,
                       unsigned Kind = 0) const override;

    void eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                             unsigned FIOperandNum,
                             RegScavenger *RS = nullptr) const override;

    bool requiresRegisterScavenging(const MachineFunction &) const override {
        return true;
    }

    bool requiresFrameIndexScavenging(const MachineFunction &) const override {
        return true;
    }

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

    static std::array<RegInfo, PRU::NUM_TARGET_REGS> build_infos();

    static unsigned reg_at_pos(unsigned offset, RegSize size);

    static RegSize reg_size(unsigned reg) { return reginfos[reg].size; }

    static unsigned reg_size_unsigned(unsigned reg) {
        return static_cast<unsigned>(reg_size(reg));
    }

    static unsigned reg_size_bits(unsigned reg) {
        return reg_size_unsigned(reg) * 8;
    }

    static unsigned reg_offset(unsigned reg) { return reginfos[reg].offset; }

    static bool are_adjacent(unsigned reg0, unsigned reg1) {
        unsigned s0 = reg_size_unsigned(reg0);
        unsigned s1 = reg_size_unsigned(reg1);
        return reg_offset(reg0) + s0 == reg_offset(reg1) ||
               reg_offset(reg1) + s1 == reg_offset(reg0);
    }
};
}
