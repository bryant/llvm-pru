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

static void batched_csr_ops(MachineBasicBlock &b,
                            MachineBasicBlock::iterator ii,
                            const std::vector<CalleeSavedInfo> &csi,
                            const TargetRegisterInfo *tri, unsigned opcode,
                            unsigned regstate) {
    auto &tii = *b.getParent()->getSubtarget().getInstrInfo();

    for (auto c = csi.cbegin(); c != csi.cend();) {
        dbgs() << "caught " << tri->getName(c->getReg()) << " assigned to "
               << c->getFrameIdx() << "\n";
        auto cc = c;
        unsigned store_size = PRURegisterInfo::reg_size_unsigned(cc->getReg());
        ++c;
        while (c != csi.cend() && PRURegisterInfo::are_adjacent(
                                      c->getReg(), std::prev(c)->getReg())) {
            dbgs() << "adjacent " << tri->getName(c->getReg())
                   << " assigned to " << c->getFrameIdx() << "\n";
            store_size += PRURegisterInfo::reg_size_unsigned(c->getReg());
            ++c;
        }
        const MachineInstrBuilder &batch =
            BuildMI(b, ii, b.findDebugLoc(ii), tii.get(opcode))
                .addFrameIndex(cc->getFrameIdx())
                .addImm(0)
                .addImm(store_size);
        for (; cc != c; ++cc) {
            batch.addReg(cc->getReg(), regstate);
        }
    }
}

void PRUFrameLowering::eliminateCallFramePseudoInstr(
    MachineFunction &f, MachineBasicBlock &mbb,
    MachineBasicBlock::iterator i) const {
    mbb.erase(i);
}

bool PRUFrameLowering::spillCalleeSavedRegisters(
    MachineBasicBlock &b, MachineBasicBlock::iterator ii,
    const std::vector<CalleeSavedInfo> &csi,
    const TargetRegisterInfo *tri) const {

    batched_csr_ops(b, ii, csi, tri, PRU::sbbo_multiple, getKillRegState(true));
    return true;
}

bool PRUFrameLowering::restoreCalleeSavedRegisters(
    MachineBasicBlock &b, MachineBasicBlock::iterator ii,
    const std::vector<CalleeSavedInfo> &csi,
    const TargetRegisterInfo *tri) const {

    batched_csr_ops(b, ii, csi, tri, PRU::lbbo_multiple, getDefRegState(true));
    return true;
}

void PRUFrameLowering::determineCalleeSaves(MachineFunction &f,
                                            BitVector &saved,
                                            RegScavenger *regscav) const {
    TargetFrameLowering::determineCalleeSaves(f, saved, regscav);
    if (saved[PRU::r3_w2] &&
        (saved[PRU::r5] || saved[PRU::r5_w2] || saved[PRU::r5_b0])) {
        saved.set(PRU::r4);
    }
}

bool PRUFrameLowering::assignCalleeSavedSpillSlots(
    MachineFunction &f, const TargetRegisterInfo *tri,
    std::vector<CalleeSavedInfo> &csi) const {

    auto sort_by_reg_offset = [](const CalleeSavedInfo &a,
                                 const CalleeSavedInfo &b) {
        return PRURegisterInfo::reg_offset(a.getReg()) <
               PRURegisterInfo::reg_offset(b.getReg());
    };

    std::sort(csi.begin(), csi.end(),
              [&](const CalleeSavedInfo &a, const CalleeSavedInfo &b) {
                  return !sort_by_reg_offset(a, b);
              });

    MachineFrameInfo &mfi = *f.getFrameInfo();

    for (CalleeSavedInfo &c : csi) {
        c.setFrameIdx(mfi.CreateStackObject(
            PRURegisterInfo::reg_size_unsigned(c.getReg()), 1, true));
    }

    std::sort(csi.begin(), csi.end(), sort_by_reg_offset);
    return true;
}

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
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_ldi32), PRU::r0)
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
            BuildMI(b, ins, dl, pruinfo.get(PRU::pru_ldi32), PRU::r0)
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
