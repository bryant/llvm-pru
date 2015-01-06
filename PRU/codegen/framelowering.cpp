#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Target/TargetOptions.h"

#include "framelowering.h"
#include "instrinfo.h"
#include "subtarget.h"
#include "targetdesc.h"

using namespace llvm;

bool PRUFrameLowering::hasFP(const MachineFunction &MF) const {
    const MachineFrameInfo *MFI = MF.getFrameInfo();

    return (MF.getTarget().Options.DisableFramePointerElim(MF) ||
            MF.getFrameInfo()->hasVarSizedObjects() ||
            MFI->isFrameAddressTaken());
}

/*
bool PRUFrameLowering::hasReservedCallFrame(const MachineFunction &MF) const {
    return !MF.getFrameInfo()->hasVarSizedObjects();
}
*/

void PRUFrameLowering::emitPrologue(MachineFunction &f,
                                    MachineBasicBlock &b) const {
    if (unsigned stacksize = f.getFrameInfo()->estimateStackSize(f)) {
        const TargetInstrInfo &pruinfo = *f.getSubtarget().getInstrInfo();
        auto ins = b.begin();
        DebugLoc dl;

        if (stacksize <= 255) {
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_sub_reg32_reg32_i32imm))
                .addReg(PRU::r2)
                .addReg(PRU::r2)
                .addImm(stacksize)
                .setMIFlag(MachineInstr::FrameSetup);
        } else {
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_mov_reg32_i32), PRU::r0)
                .addImm(stacksize);
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_sub_reg32_reg32_reg32))
                .addReg(PRU::r2)
                .addReg(PRU::r2)
                .addReg(PRU::r0)
                .setMIFlag(MachineInstr::FrameSetup);
        }
    }
}

void PRUFrameLowering::emitEpilogue(MachineFunction &f,
                                    MachineBasicBlock &b) const {
    if (unsigned stacksize = f.getFrameInfo()->estimateStackSize(f)) {
        const TargetInstrInfo &pruinfo = *f.getSubtarget().getInstrInfo();
        auto ins = b.getLastNonDebugInstr();
        DebugLoc dl;

        if (stacksize <= 255) {
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_add_reg32_reg32_i32imm))
                .addReg(PRU::r2)
                .addReg(PRU::r2)
                .addImm(stacksize)
                .setMIFlag(MachineInstr::FrameSetup);
        } else {
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_mov_reg32_i32), PRU::r0)
                .addImm(stacksize);
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_add_reg32_reg32_reg32))
                .addReg(PRU::r2)
                .addReg(PRU::r2)
                .addReg(PRU::r0)
                .setMIFlag(MachineInstr::FrameSetup);
        }
    }
}
// TODO: impl call frame adj pseudos elimination
