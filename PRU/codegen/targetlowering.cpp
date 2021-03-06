#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "abi/ti.h"
#include "instrinfo.h"
#include "subtarget.h"
#include "targetdesc.h"
#include "targetlowering.h"
#include "targetmachine.h"

using namespace llvm;

#define DEBUG_TYPE "pru-lower"

// TODO: figure out if isr procs need special handling

PRUTargetLowering::PRUTargetLowering(const TargetMachine &targ,
                                     const PRUSubtarget &subtarg)
    : TargetLowering(targ) {

    addRegisterClass(MVT::i8, &PRU::reg8RegClass);
    addRegisterClass(MVT::i16, &PRU::reg16RegClass);
    addRegisterClass(MVT::i32, &PRU::reg32RegClass);

    computeRegisterProperties(subtarg.getRegisterInfo());

    setStackPointerRegisterToSaveRestore(PRU::r2);
    setBooleanContents(ZeroOrOneBooleanContent);
    setBooleanVectorContents(ZeroOrOneBooleanContent);

    for (MVT valty : MVT::integer_valuetypes()) {
        for (MVT memty : {MVT::i1, MVT::i8, MVT::i16, MVT::i32}) {
            // TODO: trunc-/extloads can be done unexpanded via mvi{b,w,d}

            setLoadExtAction(ISD::EXTLOAD, valty, memty, Expand);
            setLoadExtAction(ISD::SEXTLOAD, valty, memty, Expand);
            setLoadExtAction(ISD::ZEXTLOAD, valty, memty, Expand);

            setTruncStoreAction(valty, memty, Expand);
        }

        // TODO: lowr sign extension. can be done with 'fill'
    }

    // Provide all sorts of operation actions

    // Division is expensive
    // setIntDivIsCheap(false);

    // TODO: does brcond need to be expanded?
    // TODO: signed ops: cmp, alu
    // setOperationAction(ISD::BR_JT, MVT::Other, Expand);
    // setOperationAction(ISD::BR_CC, MVT::i8, Expand);
    // setOperationAction(ISD::BR_CC, MVT::i16, Expand);
    // setOperationAction(ISD::BRCOND, MVT::Other, Expand);
    // setOperationAction(ISD::SETCC, MVT::i8, Expand);
    // setOperationAction(ISD::SETCC, MVT::i16, Expand);

    // TODO: handle selectcc. handled.
    for (MVT valty : MVT::integer_valuetypes()) {
        setOperationAction(ISD::SELECT, valty, Expand);
    }

    setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
    setOperationAction(ISD::ExternalSymbol, MVT::i32, Custom);
    setOperationAction(ISD::BlockAddress, MVT::i32, Custom);

    //// FIXME: Implement efficiently multiplication by a constant
    //// TODO: varargs support

    setMinFunctionAlignment(1);
    setPrefFunctionAlignment(1);
}

SDValue PRUTargetLowering::LowerOperation(SDValue op, SelectionDAG &dag) const {
    dbgs() << "LowerOperation called for ";
    op.dump();
    dbgs() << "\n";

    MVT ptrvt = getPointerTy(dag.getDataLayout());
    auto get_target_const = [&](SDValue inner) {
        return dag.getNode(PRUISD::TargetConst, SDLoc(op), ptrvt, inner);
    };

    switch (op.getOpcode()) {
    case ISD::GlobalAddress: {
        const auto *gop = cast<GlobalAddressSDNode>(op);
        return get_target_const(dag.getTargetGlobalAddress(
            gop->getGlobal(), SDLoc(op), ptrvt, gop->getOffset()));
    }

    case ISD::BlockAddress: {
        const auto *bop = cast<BlockAddressSDNode>(op);
        return get_target_const(dag.getTargetBlockAddress(
            bop->getBlockAddress(), ptrvt, bop->getOffset()));
    }

    case ISD::ExternalSymbol: {
        const auto *ext = cast<ExternalSymbolSDNode>(op);
        return get_target_const(dag.getTargetExternalSymbol(
            ext->getSymbol(), ptrvt, ext->getTargetFlags()));
    }
    }

    dbgs() << "LowerOperation: op was " << op.getNode()->getOperationName()
           << "\n";
    llvm_unreachable("unimplemented operand");
}

bool PRUTargetLowering::functionArgumentNeedsConsecutiveRegisters(
    Type *ty, CallingConv::ID cc, bool) const {
    const auto &dl =
        reinterpret_cast<const PRUTargetMachine *>(&getTargetMachine())
            ->getDataLayout();
    return ty->isStructTy() || ty->isArrayTy() || dl.getTypeSizeInBits(ty) > 32;
}

SDValue PRUTargetLowering::LowerFormalArguments(
    SDValue chain, CallingConv::ID conv, bool vararg,
    const SmallVectorImpl<ISD::InputArg> &Ins, SDLoc dl, SelectionDAG &DAG,
    SmallVectorImpl<SDValue> &InVals) const {

    if (vararg) {
        llvm_unreachable("TODO: write var arg lowering");
    }

    if (conv != CallingConv::C && conv != CallingConv::Fast) {
        llvm_unreachable("Unsupported calling convention");
    }

    TexasCC cc(DAG, vararg);
    auto vals = cc.lower_formal_args(chain, dl, Ins);
    InVals.insert(InVals.end(), vals.begin(), vals.end());
    return chain;
}

static SDValue lower_call_addr(SDValue addr, SDLoc dl, SelectionDAG &sdag) {
    MVT ptrvt = MVT::getIntegerVT(sdag.getDataLayout().getPointerSizeInBits(0));
    if (GlobalAddressSDNode *callee = dyn_cast<GlobalAddressSDNode>(addr)) {
        return sdag.getTargetGlobalAddress(callee->getGlobal(), dl, ptrvt,
                                           callee->getOffset());
    } else if (ExternalSymbolSDNode *s = dyn_cast<ExternalSymbolSDNode>(addr)) {
        return sdag.getTargetExternalSymbol(s->getSymbol(), ptrvt, 0);
    } else {
        dbgs() << "Unknown call address node type\n";
        addr->print(dbgs());
        return addr;
    }
}

SDValue PRUTargetLowering::LowerCall(CallLoweringInfo &call,
                                     SmallVectorImpl<SDValue> &retvals) const {
    assert(call.CallConv == CallingConv::Fast ||
           call.CallConv == CallingConv::C);

    // TODO: impl
    call.IsTailCall = false;
    call.IsVarArg = false;

    TexasCC cc(call.DAG, call.IsVarArg);
    SDValue space = call.DAG.getIntPtrConstant(cc.compute_call_stack(call.Outs),
                                               call.DL, true);
    SDValue ch = call.DAG.getCALLSEQ_START(call.Chain, space, call.DL);
    SDValue glue;

    auto callops = cc.lower_call(ch, glue, call.DL, call.Outs, call.OutVals);
    callops.insert(callops.begin(),
                   lower_call_addr(call.Callee, call.DL, call.DAG));
    callops.insert(callops.begin(), ch);
    callops.push_back(call.DAG.getRegisterMask(
        call.DAG.getSubtarget().getRegisterInfo()->getCallPreservedMask(
            call.DAG.getMachineFunction(), call.CallConv)));

    if (glue) {
        callops.push_back(glue);
    }
    ch = call.DAG.getNode(PRUISD::CALL, call.DL,
                          call.DAG.getVTList(MVT::Other, MVT::Glue), callops);
    glue = ch.getValue(1);

    ch = call.DAG.getCALLSEQ_END(
        ch, space, call.DAG.getIntPtrConstant(0, call.DL, true), glue, call.DL);
    glue = ch.getValue(1);

    auto rvs = cc.lower_call_result(ch, glue, call.DL, call.Ins);
    retvals.insert(retvals.end(), rvs.begin(), rvs.end());
    return ch;
}

// sdag builder implicitly switches to using sret if below returns false
bool PRUTargetLowering::CanLowerReturn(
    CallingConv::ID cc, MachineFunction &f, bool vararg,
    const SmallVectorImpl<ISD::OutputArg> &outs, LLVMContext &ctx) const {
    return !TexasCC::needs_sret(outs);
}

SDValue
PRUTargetLowering::LowerReturn(SDValue ch, CallingConv::ID cc, bool vararg,
                               const SmallVectorImpl<ISD::OutputArg> &outs,
                               const SmallVectorImpl<SDValue> &outvals,
                               SDLoc dl, SelectionDAG &sdag) const {
    TexasCC cc_(sdag, vararg);
    SDValue glue;
    auto retops = cc_.lower_return(ch, glue, dl, outs, outvals);
    if (retops.size() > 0) {
        retops.push_back(glue);
    }
    retops.insert(retops.begin(), ch);
    return sdag.getNode(PRUISD::RET_FLAG, dl, MVT::Other, retops);
}

const char *PRUTargetLowering::getTargetNodeName(unsigned Opcode) const {
    switch ((PRUISD::NodeType)Opcode) {
    case PRUISD::FIRST_NUMBER:
        break;
    case PRUISD::RET_FLAG:
        return "PRUISD::RET_FLAG";
    case PRUISD::CALL:
        return "PRUISD::CALL";
    case PRUISD::TargetConst:
        return "PRUISD::TargetConst";
    }
    return nullptr;
}

SDValue PRUTargetLowering::PerformDAGCombine(SDNode *n,
                                             DAGCombinerInfo &comb) const {
    // placeholder
    return SDValue();
}

bool is_register_sized(unsigned n) { return n == 8 || n == 16 || n == 32; }

bool PRUTargetLowering::isTruncateFree(Type *src, Type *smaller) const {
    if (src->isIntegerTy() && smaller->isIntegerTy()) {
        unsigned srcb = src->getPrimitiveSizeInBits();
        unsigned smallerb = smaller->getPrimitiveSizeInBits();

        bool rv = smallerb < srcb &&
                  is_register_sized(smaller->getPrimitiveSizeInBits()) &&
                  is_register_sized(src->getPrimitiveSizeInBits());
        dbgs() << "[pru] isTruncateFree called on " << srcb << ", " << smallerb
               << ", returning " << rv << "\n";
        return rv;
    }
    return false;
}

bool PRUTargetLowering::isTruncateFree(EVT src, EVT smaller) const {
    if (src.isInteger() && smaller.isInteger()) {
        unsigned srcb = src.getSizeInBits();
        unsigned smallerb = smaller.getSizeInBits();

        bool rv = smallerb < srcb && is_register_sized(smallerb) &&
                  is_register_sized(srcb);
        dbgs() << "[pru] isTruncateFree called on " << srcb << ", " << smallerb
               << ", returning " << rv << "\n";
        return rv;
    }
    return false;
}

bool PRUTargetLowering::isZExtFree(Type *src, Type *larger) const {
    if (src->isIntegerTy() && larger->isIntegerTy()) {
        unsigned srcb = src->getPrimitiveSizeInBits();
        unsigned largerb = larger->getPrimitiveSizeInBits();

        bool rv = largerb > srcb && is_register_sized(largerb) &&
                  is_register_sized(srcb);
        dbgs() << "[pru] isZExt called on " << srcb << ", " << largerb
               << ", returning " << rv << "\n";
        return rv;
    }
    return false;
}

bool PRUTargetLowering::isZExtFree(EVT src, EVT larger) const {
    if (src.isInteger() && larger.isInteger()) {
        unsigned srcb = src.getSizeInBits();
        unsigned largerb = larger.getSizeInBits();
        bool rv = largerb > srcb && is_register_sized(largerb) &&
                  is_register_sized(srcb);
        dbgs() << "[pru] isZExtFree called on " << srcb << ", " << largerb
               << ", returning " << rv << "\n";
        return rv;
    }
    return false;
}

unsigned select_cc_to_branch_code(MachineInstr &i) {
    switch (i.getOpcode()) {
    default:
        llvm_unreachable("invalid select_cc op code");
    case PRU::pru_selectne:
        return PRU::pru_qbne;
    case PRU::pru_selecteq:
        return PRU::pru_qbeq;
    case PRU::pru_selectgt:
        return PRU::pru_qbgt;
    case PRU::pru_selectge:
        return PRU::pru_qbge;
    case PRU::pru_selectlt:
        return PRU::pru_qblt;
    case PRU::pru_selectle:
        return PRU::pru_qble;
    }
}

MachineBasicBlock *
PRUTargetLowering::EmitInstrWithCustomInserter(MachineInstr *i,
                                               MachineBasicBlock *mbb) const {
    // TODO: add explicit check for selectcc opcode
    // expand select_cc pseudos.
    unsigned brop = select_cc_to_branch_code(*i);

    MachineFunction *f = mbb->getParent();
    auto tblock = f->CreateMachineBasicBlock(mbb->getBasicBlock());
    auto fblock = f->CreateMachineBasicBlock(mbb->getBasicBlock());

    auto blk_insert = ++mbb->getIterator();
    f->insert(blk_insert, fblock);
    f->insert(blk_insert, tblock);

    tblock->splice(tblock->begin(), mbb, ++MachineBasicBlock::iterator(i),
                   mbb->end());
    tblock->transferSuccessorsAndUpdatePHIs(mbb);
    mbb->addSuccessor(fblock);
    mbb->addSuccessor(tblock);

    auto *instrs = reinterpret_cast<const PRUInstrInfo *>(
        f->getSubtarget().getInstrInfo());
    MachineOperand &dest = i->getOperand(0), &lhs = i->getOperand(1),
                   &rhs = i->getOperand(2), &tval = i->getOperand(3),
                   fval = i->getOperand(4);

    auto add_reg_imm = [](MachineInstrBuilder &ib, MachineOperand &op) {
        op.isImm() ? ib.addImm(op.getImm()) : ib.addReg(op.getReg());
    };

    auto ib = BuildMI(mbb, i->getDebugLoc(), instrs->get(brop)).addMBB(tblock);
    add_reg_imm(ib, lhs);
    add_reg_imm(ib, rhs);

    fblock->addSuccessor(tblock);
    BuildMI(*tblock, tblock->begin(), i->getDebugLoc(), instrs->get(PRU::PHI),
            dest.getReg())
        .addReg(tval.getReg())
        .addMBB(mbb)
        .addReg(fval.getReg())
        .addMBB(fblock);

    i->eraseFromParent();
    return tblock;
}
