#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Target/TargetSubtargetInfo.h"

#include "registerinfo.h"
#include "subtarget.h"
#include "targetdesc.h"

using namespace llvm;

#define DEBUG_TYPE "pru-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "registerinfo.inc"

// TODO: Provide proper call frame setup / destroy opcodes.
PRURegisterInfo::PRURegisterInfo() : PRUGenRegisterInfo(PRU::r3_w2) {}

const MCPhysReg *
PRURegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
    return callee_saved_SaveList;
}

BitVector PRURegisterInfo::getReservedRegs(const MachineFunction &MF) const {
    BitVector reserved(getNumRegs());

    auto mark_reserved = [&](unsigned reg) {
        for (MCRegAliasIterator r(reg, this, true); r.isValid(); ++r) {
            reserved.set(*r);
        }
    };

    mark_reserved(PRU::r2);  // stack pointer
    mark_reserved(PRU::r30); // aliased to io
    mark_reserved(PRU::r31); // " " "

    if (getFrameLowering(MF)->hasFP(MF)) {
        // reserved.set(PRU::r4);
        mark_reserved(PRU::r4);
    }

    return reserved;
}

const TargetRegisterClass *
PRURegisterInfo::getPointerRegClass(const MachineFunction &MF,
                                    unsigned Kind) const {
    return &PRU::reg32RegClass;
}

void PRURegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator II,
                                          int SPAdj, unsigned FIOperandNum,
                                          RegScavenger *RS) const {
    dbgs() << "eliminateFrameIndex called\n";
    assert(SPAdj == 0 && "Unexpected");

    MachineInstr &i = *II;
    MachineFrameInfo &mf = *i.getParent()->getParent()->getFrameInfo();

    int idx = i.getOperand(FIOperandNum).getIndex();

    int offset = mf.getStackSize() + mf.getObjectOffset(idx) +
                 i.getOperand(FIOperandNum + 1).getImm();

    dbgs() << "[pru] eliminateFrameIndex offset = " << offset << "\n";

    i.getOperand(FIOperandNum).ChangeToRegister(PRU::r2, false);
    i.getOperand(FIOperandNum + 1).ChangeToImmediate(offset);
}

unsigned PRURegisterInfo::getFrameRegister(const MachineFunction &MF) const {
    const PRUFrameLowering *TFI = getFrameLowering(MF);
    return TFI->hasFP(MF) ? PRU::r4 : PRU::r2;
}

unsigned PRURegisterInfo::reg_size(unsigned regnum) const {
    for (auto cls = regclass_begin(); cls != regclass_end(); ++cls) {
        if ((*cls)->contains(regnum)) {
            return (*cls)->getSize() * 8;
        }
    }
    llvm_unreachable("invalid register!");
}

unsigned PRURegisterInfo::find_subreg_in(unsigned reg, unsigned offset,
                                         unsigned bits) const {
    if (reg_size(reg) == bits && offset == 0) {
        return reg;
    }
    for (MCSubRegIndexIterator idx(reg, this); idx.isValid(); ++idx) {
        unsigned sub = idx.getSubReg(), i = idx.getSubRegIndex();
        if (sub != 0) {
            if (getSubRegIdxSize(i) == bits &&
                getSubRegIdxOffset(i) == offset) {
                return sub;
            }
        }
    }
    return 0;
}

const std::vector<MCPhysReg> PRURegisterInfo::i8_arg_regs() const {
    size_t len = sizeof(i8_args_SaveList) / sizeof(MCPhysReg) - 1;
    static const std::vector<MCPhysReg> rv(i8_args_SaveList,
                                           i8_args_SaveList + len);
    return rv;
}

const std::vector<MCPhysReg> PRURegisterInfo::i16_arg_regs() const {
    size_t len = sizeof(i16_args_SaveList) / sizeof(MCPhysReg) - 1;
    static const std::vector<MCPhysReg> rv(i16_args_SaveList,
                                           i16_args_SaveList + len);
    return rv;
}

const std::vector<MCPhysReg> PRURegisterInfo::i32_arg_regs() const {
    size_t len = sizeof(i32_args_SaveList) / sizeof(MCPhysReg) - 1;
    static const std::vector<MCPhysReg> rv(i32_args_SaveList,
                                           i32_args_SaveList + len);
    return rv;
}
