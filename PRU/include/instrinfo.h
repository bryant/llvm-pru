#pragma once

#include "registerinfo.h"
#include "llvm/Target/TargetInstrInfo.h"

#define GET_INSTRINFO_HEADER
#include "instrinfo.inc"

namespace llvm {

class PRUSubtarget;

class PRUInstrInfo : public PRUGenInstrInfo {
    const PRURegisterInfo RI;

  public:
    explicit PRUInstrInfo(PRUSubtarget &STI);

    const TargetRegisterInfo &getRegisterInfo() const { return RI; }

    void copyPhysReg(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     DebugLoc DL, unsigned DestReg, unsigned SrcReg,
                     bool KillSrc) const override;

    void storeRegToStackSlot(MachineBasicBlock &, MachineBasicBlock::iterator,
                             unsigned reg, bool kill, int frameindex,
                             const TargetRegisterClass *,
                             const TargetRegisterInfo *) const override;

    void loadRegFromStackSlot(MachineBasicBlock &, MachineBasicBlock::iterator,
                              unsigned reg, int, const TargetRegisterClass *,
                              const TargetRegisterInfo *) const override;

    unsigned GetInstSizeInBytes(const MachineInstr *MI) const;

    bool AnalyzeBranch(MachineBasicBlock &mbb, MachineBasicBlock *&tbb,
                       MachineBasicBlock *&FBB,
                       SmallVectorImpl<MachineOperand> &Cond,
                       bool AllowModify) const override;

    unsigned RemoveBranch(MachineBasicBlock &) const override;

    unsigned InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                          MachineBasicBlock *FBB, ArrayRef<MachineOperand> Cond,
                          DebugLoc DL) const override;

    bool
    ReverseBranchCondition(SmallVectorImpl<MachineOperand> &) const override;

    bool areLoadsFromSameBasePtr(SDNode *, SDNode *, int64_t &,
                                 int64_t &) const override;

    bool shouldScheduleLoadsNear(SDNode *, SDNode *, int64_t, int64_t,
                                 unsigned) const override;

    bool enableClusterLoads() const override { return true; }

    bool shouldClusterLoads(MachineInstr *, MachineInstr *,
                            unsigned) const override;

    bool shouldScheduleAdjacent(MachineInstr *, MachineInstr *) const override;

    bool getMemOpBaseRegImmOfs(MachineInstr *, unsigned &, unsigned &,
                               const TargetRegisterInfo *) const override;

    static bool is_load(unsigned);

    static bool is_load_multiple(unsigned opc);

    static bool is_store_multiple(unsigned opc);

    static bool is_store(unsigned opc);
};
}
