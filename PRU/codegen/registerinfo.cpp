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
#include "targetdesc.h"
#include "subtarget.h"

using namespace llvm;

#define DEBUG_TYPE "pru-reg-info"

#define GET_REGINFO_TARGET_DESC
#include "registerinfo.inc"

// FIXME: Provide proper call frame setup / destroy opcodes.
PRURegisterInfo::PRURegisterInfo() : PRUGenRegisterInfo(PRU::r14) {}

const MCPhysReg *
PRURegisterInfo::getCalleeSavedRegs(const MachineFunction *MF) const {
    const PRUFrameLowering *fl = getFrameLowering(*MF);
    static const MCPhysReg callee_saved[] = {
        PRU::r4, PRU::r3_w2, PRU::r5,  PRU::r6,  PRU::r7,  PRU::r8,
        PRU::r9, PRU::r10,   PRU::r11, PRU::r12, PRU::r13, 0};

    if (fl->hasFP(*MF)) {
        return callee_saved;
    }

    return callee_saved + 1;
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

    MachineInstr &MI = *II;
    MachineBasicBlock &b = *MI.getParent();
    MachineFunction &f = *b.getParent();
    DebugLoc dl = MI.getDebugLoc();

    int idx = MI.getOperand(FIOperandNum).getIndex();

    // unsigned BasePtr = (TFI->hasFP(MF) ? PRU::r4 : PRU::r2);
    int offset = f.getFrameInfo()->getStackSize() +
                 f.getFrameInfo()->getObjectOffset(idx) +
                 MI.getOperand(FIOperandNum + 1).getImm();

    dbgs() << "[pru] eliminateFrameIndex offset = " << offset << "\n";

    /*
    if (!TFI->hasFP(MF))
        Offset += MF.getFrameInfo()->getStackSize();
    else
        Offset += 2; // Skip the saved FP
    */

    MI.getOperand(FIOperandNum).ChangeToRegister(PRU::r2, false);
    MI.getOperand(FIOperandNum + 1).ChangeToImmediate(offset);
}

unsigned PRURegisterInfo::getFrameRegister(const MachineFunction &MF) const {
    const PRUFrameLowering *TFI = getFrameLowering(MF);
    return TFI->hasFP(MF) ? PRU::r4 : PRU::r2;
}
