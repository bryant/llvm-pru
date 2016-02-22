#pragma once

#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/Target/TargetLowering.h"

namespace llvm {
namespace PRUISD {
enum NodeType : unsigned {
    FIRST_NUMBER = ISD::BUILTIN_OP_END,

    /// Return with a flag operand. Operand 0 is the chain operand.
    RET_FLAG,

    /// CALL - These operations represent an abstract call
    /// instruction, which includes a bunch of information.
    CALL,

    TargetConst,
};
}

class PRUSubtarget;
class PRUTargetLowering : public TargetLowering {
  public:
    explicit PRUTargetLowering(const TargetMachine &TM,
                               const PRUSubtarget &STI);

    /// LowerOperation - Provide custom lowering hooks for some operations.
    SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const override;

    /// getTargetNodeName - This method returns the name of a target specific
    /// DAG node.
    const char *getTargetNodeName(unsigned Opcode) const override;

    SDValue PerformDAGCombine(SDNode *, DAGCombinerInfo &) const override;

    bool isTruncateFree(Type *, Type *) const override;

    bool isTruncateFree(EVT, EVT) const override;

    bool isZExtFree(Type *src, Type *larger) const override;

    bool isZExtFree(EVT src, EVT larger) const override;

    bool functionArgumentNeedsConsecutiveRegisters(Type *, CallingConv::ID,
                                                   bool) const override;

    MachineBasicBlock *
    EmitInstrWithCustomInserter(MachineInstr *i,
                                MachineBasicBlock *mbb) const override;

    bool allowsMisalignedMemoryAccesses(EVT, unsigned, unsigned,
                                        bool *) const override {
        return true;
    }

  private:
    SDValue
    LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                         const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc dl,
                         SelectionDAG &DAG,
                         SmallVectorImpl<SDValue> &InVals) const override;
    SDValue LowerCall(TargetLowering::CallLoweringInfo &CLI,
                      SmallVectorImpl<SDValue> &InVals) const override;

    bool CanLowerReturn(CallingConv::ID, MachineFunction &, bool,
                        const SmallVectorImpl<ISD::OutputArg> &,
                        LLVMContext &) const override;

    SDValue LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals, SDLoc dl,
                        SelectionDAG &DAG) const override;

    /*
    bool getPostIndexedAddressParts(SDNode *N, SDNode *Op, SDValue &Base,
                                    SDValue &Offset, ISD::MemIndexedMode &AM,
                                    SelectionDAG &DAG) const override;
    */
};
}
