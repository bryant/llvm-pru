#include "instrinfo.h"
//#include "machinefunctioninfo.h"
#include "targetdesc.h"
#include "targetmachine.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define GET_INSTRINFO_CTOR_DTOR
#include "instrinfo.inc"

PRUInstrInfo::PRUInstrInfo(PRUSubtarget &STI)
    : PRUGenInstrInfo() //(PRU::ADJCALLSTACKDOWN, PRU::ADJCALLSTACKUP)
      ,
      RI() {}

bool PRUInstrInfo::areLoadsFromSameBasePtr(SDNode *load0, SDNode *load1,
                                           int64_t &off0, int64_t &off1) const {
    dbgs() << "PRUInstrInfo::areLoadsFromSameBasePtr called with:\n";
    load0->print(dbgs());
    dbgs() << "\n";
    load1->print(dbgs());
    dbgs() << "\n";

    if (load0->isMachineOpcode() && load1->isMachineOpcode() &&
        is_load(load0->getMachineOpcode()) &&
        is_load(load1->getMachineOpcode()) &&
        load0->getOperand(0) == load1->getOperand(0) &&
        isa<ConstantSDNode>(load0->getOperand(1)) &&
        isa<ConstantSDNode>(load1->getOperand(1))) {

        off0 = cast<ConstantSDNode>(load0->getOperand(1))->getSExtValue();
        off1 = cast<ConstantSDNode>(load1->getOperand(1))->getSExtValue();

        dbgs() << "returning true\n";
        return true;
    }
    return false;
}

bool PRUInstrInfo::shouldScheduleLoadsNear(SDNode *l0, SDNode *l1, int64_t off0,
                                           int64_t off1,
                                           unsigned num_loads) const {
    dbgs() << "PRUInstrInfo::shouldScheduleLoadsNear called with idk\n";
    l0->print(dbgs());
    dbgs() << "\n";
    l1->print(dbgs());
    dbgs() << "\n";
    return true;
}

bool PRUInstrInfo::shouldClusterLoads(MachineInstr *l0, MachineInstr *l1,
                                      unsigned num_loads) const {
    dbgs() << "PRUInstrInfo::shouldClusterLoads called:\n";
    l0->print(dbgs());
    dbgs() << "\n";
    l1->print(dbgs());
    dbgs() << "\n";
    return true;
}

bool PRUInstrInfo::shouldScheduleAdjacent(MachineInstr *i0,
                                          MachineInstr *i1) const {
    dbgs() << "PRUInstrInfo::shouldScheduleAdjacent called:\n";
    i0->print(dbgs());
    dbgs() << "\n";
    i1->print(dbgs());
    dbgs() << "\n";

    return false;
}

bool PRUInstrInfo::getMemOpBaseRegImmOfs(MachineInstr *i, unsigned &basereg,
                                         unsigned &offset,
                                         const TargetRegisterInfo *) const {
    dbgs() << "PRUInstrInfo::getMemOpBaseRegImmOfs called:\n";
    i->print(dbgs());
    dbgs() << "\n";

    if ((is_load(i->getOpcode()) || is_store(i->getOpcode())) &&
        i->getOperand(1).isReg() && i->getOperand(2).isImm()) {
        basereg = i->getOperand(1).getReg();
        offset = i->getOperand(2).getImm();
        dbgs() << "returning true\n";
        return true;
    }

    return false;
}

bool PRUInstrInfo::is_load(unsigned opc) {
    return opc == PRU::lbbo_r32 || opc == PRU::lbbo_r16 || opc == PRU::lbbo_r8;
}

unsigned PRUInstrInfo::mem_op_size(unsigned opc) {
    switch (opc) {
    case PRU::lbbo_r32:
    case PRU::sbbo_r32:
        return 4;
    case PRU::lbbo_r16:
    case PRU::sbbo_r16:
        return 2;
    case PRU::lbbo_r8:
    case PRU::sbbo_r8:
        return 1;
    default:
        llvm_unreachable("unknown load opcode");
    }
}

bool PRUInstrInfo::is_store(unsigned opc) {
    return opc == PRU::sbbo_r32 || opc == PRU::sbbo_r16 || opc == PRU::sbbo_r8;
}

void PRUInstrInfo::storeRegToStackSlot(MachineBasicBlock &b,
                                       MachineBasicBlock::iterator insert_point,
                                       unsigned reg, bool kill, int frameindex,
                                       const TargetRegisterClass *regclass,
                                       const TargetRegisterInfo *) const {
    unsigned opcode;
    if (PRU::reg32RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::sbbo_r32;
    } else if (PRU::reg16RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::sbbo_r16;
    } else if (PRU::reg8RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::sbbo_r8;
    } else {
        dbgs() << "got register size " << regclass->getSize() << "\n";
        llvm_unreachable("unknown register class");
    }

    BuildMI(b, insert_point, insert_point->getDebugLoc(), get(opcode))
        .addReg(reg, getKillRegState(kill))
        .addFrameIndex(frameindex)
        .addImm(0);
}

void PRUInstrInfo::loadRegFromStackSlot(MachineBasicBlock &b,
                                        MachineBasicBlock::iterator i,
                                        unsigned reg, int frameindex,
                                        const TargetRegisterClass *regclass,
                                        const TargetRegisterInfo *) const {
    unsigned opcode;
    if (PRU::reg32RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::lbbo_r32;
    } else if (PRU::reg16RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::lbbo_r16;
    } else if (PRU::reg8RegClass.hasSubClassEq(regclass)) {
        opcode = PRU::lbbo_r8;
    } else {
        dbgs() << "got register size " << regclass->getSize() << "\n";
        llvm_unreachable("unknown register class");
    }

    BuildMI(b, i, i->getDebugLoc(), get(opcode))
        .addReg(reg)
        .addFrameIndex(frameindex)
        .addImm(0);
}

void PRUInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                               MachineBasicBlock::iterator I, DebugLoc DL,
                               unsigned DestReg, unsigned SrcReg,
                               bool KillSrc) const {
    unsigned Opc;

    if (PRU::reg8RegClass.contains(DestReg, SrcReg)) {
        Opc = PRU::pru_mov_reg8_reg8;
    } else if (PRU::reg16RegClass.contains(DestReg, SrcReg)) {
        Opc = PRU::pru_mov_reg16_reg16;
    } else if (PRU::reg32RegClass.contains(DestReg, SrcReg)) {
        Opc = PRU::pru_mov_reg32_reg32;
    } else {
        llvm_unreachable("unknown register size");
    }

    BuildMI(MBB, I, DL, get(Opc), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
}

static void pack_into(SmallVectorImpl<MachineOperand> &condops,
                      MachineInstr &instr) {
    assert(instr.isConditionalBranch() || "expected a conditional branch!");

    condops.push_back(MachineOperand::CreateImm(instr.getOpcode()));
    condops.push_back(instr.getOperand(1));
    condops.push_back(instr.getOperand(2));
}

unsigned PRUInstrInfo::RemoveBranch(MachineBasicBlock &mbb) const {
    unsigned removed = 0;

    for (auto instr = mbb.getLastNonDebugInstr();
         mbb.size() > 0 && instr != mbb.end() && instr->isBranch();
         ++removed, instr = mbb.getLastNonDebugInstr()) {

        dbgs() << "PRUInstrInfo::RemoveBranch: " << mbb.size() << "; " << instr
               << " " << mbb.begin() << " " << (instr - 1) << "\n";
        mbb.dump();

        instr->eraseFromParent();
    }
    return removed;
}

unsigned PRUInstrInfo::InsertBranch(MachineBasicBlock &mbb,
                                    MachineBasicBlock *t, MachineBasicBlock *f,
                                    ArrayRef<MachineOperand> condops,
                                    DebugLoc dl) const {
    assert(t && "PRUInstrInfo::InsertBranch given fall-through block");

    // cond br operands encoded into condops
    if (condops.size() > 0) {
        BuildMI(&mbb, dl, get(condops[0].getImm()))
            .addMBB(t) // order matters here
            .addOperand(condops[1])
            .addOperand(condops[2]);

        if (f == nullptr) {
            return 1;
        }

        BuildMI(&mbb, dl, get(PRU::pru_jmp_jump_target)).addMBB(f);
        return 2;

    } else if (f == nullptr) {
        // terminate with single uncond
        BuildMI(&mbb, dl, this->get(PRU::pru_jmp_jump_target)).addMBB(t);
        return 1;
    }

    return 0;
}

unsigned reverse_branch_condition(unsigned opcode) {
    switch (opcode) {
    default:
        llvm_unreachable("reverse_branch_condition: invalid branch op code");
#include "reversed_branches.h"
    }
}

bool PRUInstrInfo::ReverseBranchCondition(
    SmallVectorImpl<MachineOperand> &ccs) const {
    // ccs comes from pack_into
    ccs[0].setImm(reverse_branch_condition(ccs[0].getImm()));
    return false;
}

// non, non, non => t: null, f: null, condops: []
// non, non, uncond => t: uncond.getMBB, f: null, condops: []
// non, non, cond => t: cond.getMBB, f: null, condops: [cond.ops]
// non, cond, uncond => t: cond.getMBB, f: uncond.getMBB, condops:
// [cond.ops]
// otherwise => true (indeterminate)
bool PRUInstrInfo::AnalyzeBranch(MachineBasicBlock &mbb, MachineBasicBlock *&t,
                                 MachineBasicBlock *&f,
                                 SmallVectorImpl<MachineOperand> &condops,
                                 bool allowmodify) const {

    MachineBasicBlock::iterator last_pos = mbb.getLastNonDebugInstr();

    if (last_pos != mbb.end()) {
        MachineInstr *last = last_pos;
        MachineInstr *sndlast =
            (last_pos == mbb.begin()) ? nullptr : --last_pos;
        MachineInstr *thdlast =
            (last_pos == mbb.begin()) ? nullptr : --last_pos;

        if (thdlast && thdlast->isBranch()) {
            // looks like a switch table
            return true;
        }

        if (sndlast && sndlast->isConditionalBranch() && last &&
            last->isUnconditionalBranch()) {
            pack_into(condops, *sndlast);
            t = sndlast->getOperand(0).getMBB();
            f = last->getOperand(0).getMBB();
            return false;

        } else if (last && last->isUnconditionalBranch()) {
            t = last->getOperand(0).getMBB();
            f = nullptr;
            return false;

        } else if (last && last->isConditionalBranch()) {
            pack_into(condops, *last);
            t = last->getOperand(0).getMBB();
            f = nullptr;
            return false;

        } else if (last && !isUnpredicatedTerminator(last)) {
            // always falls through (isUnpredicatedTerminator checks ret
            // also)
            t = f = nullptr;
            return false;
        }
    }

    return true;
}

/// GetInstSize - Return the number of bytes of code the specified
/// instruction may be.  This returns the maximum number of bytes.
///
unsigned PRUInstrInfo::GetInstSizeInBytes(const MachineInstr *MI) const {
    /*
    const MCInstrDesc &Desc = MI->getDesc();

    switch (Desc.TSFlags & PRUII::SizeMask) {
    default:
        switch (Desc.getOpcode()) {
        default:
            llvm_unreachable("Unknown instruction size!");
        case TargetOpcode::CFI_INSTRUCTION:
        case TargetOpcode::EH_LABEL:
        case TargetOpcode::IMPLICIT_DEF:
        case TargetOpcode::KILL:
        case TargetOpcode::DBG_VALUE:
            return 0;
        case TargetOpcode::INLINEASM: {
            const MachineFunction *MF = MI->getParent()->getParent();
            const TargetInstrInfo &TII = *MF->getSubtarget().getInstrInfo();
            return TII.getInlineAsmLength(MI->getOperand(0).getSymbolName(),
                                          *MF->getTarget().getMCAsmInfo());
        }
        }
    case PRUII::SizeSpecial:
        switch (MI->getOpcode()) {
        default:
            llvm_unreachable("Unknown instruction size!");
        case PRU::SAR8r1c:
        case PRU::SAR16r1c:
            return 4;
        }
    case PRUII::Size2Bytes:
        return 2;
    case PRUII::Size4Bytes:
        return 4;
    case PRUII::Size6Bytes:
        return 6;
    }
    */
    return 4;
}
