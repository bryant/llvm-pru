#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "instrinfo.h"
#include "subtarget.h"
#include "targetlowering.h"
#include "targetmachine.h"
#include "targetdesc.h"

using namespace llvm;

#define DEBUG_TYPE "pru-lower"

typedef enum { NoHWMult, HWMultIntr, HWMultNoIntr } HWMultUseMode;

static cl::opt<HWMultUseMode> HWMultMode(
    "pru-hwmult-mode", cl::Hidden, cl::desc("Hardware multiplier use mode"),
    cl::init(HWMultNoIntr),
    cl::values(
        clEnumValN(NoHWMult, "no", "Do not use hardware multiplier"),
        clEnumValN(HWMultIntr, "interrupts",
                   "Assume hardware multiplier can be used inside interrupts"),
        clEnumValN(
            HWMultNoIntr, "use",
            "Assume hardware multiplier cannot be used inside interrupts"),
        clEnumValEnd));

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
        // setOperationAction(ISD::SIGN_EXTEND, valty, Custom);
    }

    // Provide all sorts of operation actions

    // Division is expensive
    // setIntDivIsCheap(false);

    // We have post-incremented loads / stores.
    // setIndexedLoadAction(ISD::POST_INC, MVT::i8, Legal);
    // setIndexedLoadAction(ISD::POST_INC, MVT::i16, Legal);

    // We don't have any truncstores
    // setTruncStoreAction(MVT::i16, MVT::i8, Expand);

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
        // setOperationAction(ISD::SELECT_CC, valty, Expand);
    }

    setOperationAction(ISD::GlobalAddress, MVT::i32, Custom);
    setOperationAction(ISD::ExternalSymbol, MVT::i32, Custom);
    setOperationAction(ISD::BlockAddress, MVT::i32, Custom);

    // setOperationAction(ISD::SIGN_EXTEND, MVT::i16, Expand);
    // setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i8, Expand);
    // setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i16, Expand);

    //// FIXME: Implement efficiently multiplication by a constant
    // setOperationAction(ISD::MUL, MVT::i8, Expand);
    // setOperationAction(ISD::MULHS, MVT::i8, Expand);
    // setOperationAction(ISD::MULHU, MVT::i8, Expand);
    // setOperationAction(ISD::SMUL_LOHI, MVT::i8, Expand);
    // setOperationAction(ISD::UMUL_LOHI, MVT::i8, Expand);
    // setOperationAction(ISD::MUL, MVT::i16, Expand);
    // setOperationAction(ISD::MULHS, MVT::i16, Expand);
    // setOperationAction(ISD::MULHU, MVT::i16, Expand);
    // setOperationAction(ISD::SMUL_LOHI, MVT::i16, Expand);
    // setOperationAction(ISD::UMUL_LOHI, MVT::i16, Expand);

    // setOperationAction(ISD::UDIV, MVT::i8, Expand);
    // setOperationAction(ISD::UDIVREM, MVT::i8, Expand);
    // setOperationAction(ISD::UREM, MVT::i8, Expand);
    // setOperationAction(ISD::SDIV, MVT::i8, Expand);
    // setOperationAction(ISD::SDIVREM, MVT::i8, Expand);
    // setOperationAction(ISD::SREM, MVT::i8, Expand);
    // setOperationAction(ISD::UDIV, MVT::i16, Expand);
    // setOperationAction(ISD::UDIVREM, MVT::i16, Expand);
    // setOperationAction(ISD::UREM, MVT::i16, Expand);
    // setOperationAction(ISD::SDIV, MVT::i16, Expand);
    // setOperationAction(ISD::SDIVREM, MVT::i16, Expand);
    // setOperationAction(ISD::SREM, MVT::i16, Expand);

    //// varargs support
    // setOperationAction(ISD::VASTART, MVT::Other, Expand);
    // setOperationAction(ISD::VAARG, MVT::Other, Expand);
    // setOperationAction(ISD::VAEND, MVT::Other, Expand);
    // setOperationAction(ISD::VACOPY, MVT::Other, Expand);
    // setOperationAction(ISD::JumpTable, MVT::i16, Expand);

    // setTargetDAGCombine(ISD::ANY_EXTEND);
    // setTargetDAGCombine(ISD::ZERO_EXTEND);

    setMinFunctionAlignment(1);
    setPrefFunctionAlignment(1);
}

SDValue PRUTargetLowering::LowerOperation(SDValue op, SelectionDAG &dag) const {
    dbgs() << "LowerOperation called for ";
    op.dump();
    dbgs() << "\n";

    switch (op.getOpcode()) {
    case ISD::GlobalAddress:
        return dag.getNode(PRUISD::TargetConst, SDLoc(op),
                           getPointerTy(dag.getDataLayout()), op);

    case ISD::BlockAddress:
        return dag.getNode(PRUISD::TargetConst, SDLoc(op),
                           getPointerTy(dag.getDataLayout()), op);

    case ISD::ExternalSymbol:
        return dag.getNode(PRUISD::TargetConst, SDLoc(op),
                           getPointerTy(dag.getDataLayout()), op);
    }

    dbgs() << "LowerOperation: op was " << op.getNode()->getOperationName()
           << "\n";
    llvm_unreachable("unimplemented operand");
}

//===----------------------------------------------------------------------===//
//                      Calling Convention Implementation
//===----------------------------------------------------------------------===//

bool PRUTargetLowering::functionArgumentNeedsConsecutiveRegisters(
    Type *ty, CallingConv::ID cc, bool) const {

    auto &tm = static_cast<const PRUTargetMachine &>(getTargetMachine());
    SmallVector<EVT, 4> vts;
    ComputeValueVTs(*this, const_cast<DataLayout &>(tm.getDataLayout()), ty,
                    vts);
    unsigned total_regs = 0;

    for (EVT &vt : vts) {
        total_regs += this->getNumRegisters(ty->getContext(), vt);
    }

    return total_regs > 1;
    // return ty->isStructTy() || ty->isArrayTy() || (ty->isIntegerTy() && ty->
}

static bool arg_reg_block(unsigned &argnum, MVT &valty, MVT &locty,
                          CCValAssign::LocInfo &loc, ISD::ArgFlagsTy &flags,
                          CCState &cc) {
    cc.getPendingLocs().push_back(
        CCValAssign::getPending(argnum, valty, locty, loc));

    if (flags.isInConsecutiveRegsLast()) {
    }

    return true;

    /*
    auto prucc = static_cast<PRUCCState>(cc);

    if (prucc.enough_regs) {
        static const SmallVector<MCPhysReg, 15> avail{
            PRU::r14, PRU::r15, PRU::r16, PRU::r17, PRU::r18, PRU::r19,
            PRU::r20, PRU::r21, PRU::r22, PRU::r23, PRU::r24, PRU::r25,
            PRU::r26, PRU::r27, PRU::r28, PRU::r29};
        unsigned firstreg = cc.getFirstUnallocated(avail);

        if (firstreg == avail.size() || firstreg == PRU::r29) {
            prucc.enough_regs = false;
            return false; // signal that we can't alloc to reg
        }
        */
    return false;
    // c.AllocateReg(avail[firstreg]);
}

#include "callingconv.inc"

static const TargetRegisterClass *register_class_for(EVT vartype) {
    switch (vartype.getSimpleVT().SimpleTy) {
    default:
#ifndef NDEBUG
        errs() << "no register class for type "
               << vartype.getSimpleVT().SimpleTy << "\n";
#endif
        llvm_unreachable(nullptr);
        return nullptr;

    case MVT::i1:
        return &PRU::reg32RegClass;

    case MVT::i8:
        return &PRU::reg8RegClass;

    case MVT::i16:
        return &PRU::reg16RegClass;

    case MVT::i32:
        return &PRU::reg32RegClass;

        // TODO: other types.
    }
}

void dump_assigns(ArrayRef<CCValAssign> args) {
    dbgs() << "dump_assigns:\n";
    for (unsigned n = 0; n < args.size(); ++n) {
        auto &arg = args[n];
        dbgs() << "arg " << n << ": ";
        if (arg.isRegLoc()) {
            dbgs() << "reg " << arg.getLocReg() << "\n";
        } else {
            dbgs() << "mem #" << arg.getLocMemOffset() << ", "
                   << arg.getLocVT().getStoreSize() << " bytes\n";
        }
    }
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

    MachineFunction &MF = DAG.getMachineFunction();
    MachineFrameInfo *MFI = MF.getFrameInfo();
    // Machinefunctioninfo *FuncInfo = MF.getInfo<machinefunctioninfo>();

    // Assign locations to all of the incoming arguments.
    SmallVector<CCValAssign, 16> args;
    CCState CCInfo(conv, vararg, DAG.getMachineFunction(), args,
                   *DAG.getContext());
    CCInfo.AnalyzeFormalArguments(Ins, pru_call_conv);
    for (unsigned n = 0; n < Ins.size(); ++n) {
        dbgs() << "arg " << n << " " << Ins[n].Flags.isSplit() << " "
               << Ins[n].Flags.getByValSize() << " "
               << Ins[n].ArgVT.getEVTString() << "\n";
    }
    dump_assigns(args);

    for (CCValAssign &arg : args) {
        if (arg.isRegLoc()) {
            EVT vartype = arg.getLocVT();
            auto virtreg =
                MF.addLiveIn(arg.getLocReg(), register_class_for(vartype));
            auto copy_node = DAG.getCopyFromReg(chain, dl, virtreg, vartype);

            dbgs() << "processed a reg loc:\n";
            copy_node.dump();
            InVals.push_back(copy_node);

        } else if (arg.isMemLoc()) {
            unsigned argsize = arg.getLocVT().getStoreSize();
            int frameidx =
                MFI->CreateFixedObject(argsize, arg.getLocMemOffset(), true);

            SDValue getframe =
                DAG.getFrameIndex(frameidx, getPointerTy(DAG.getDataLayout()));
            SDValue load_node =
                DAG.getLoad(arg.getValVT(), dl, chain, getframe,
                            MachinePointerInfo(), false, false, false, 0);

            dbgs() << "processed a mem loc:\n";
            load_node.dump();

            InVals.push_back(load_node);

        } else {
            llvm_unreachable("while lowering arguments, encountered an arg "
                             "that was neither reg nor mem!\n");
        }
    }

    return chain;
}

std::pair<SmallVector<CCValAssign, 16>, SmallVector<CCValAssign, 16>>
partition_locs(ArrayRef<CCValAssign> locs) {
    auto rv = std::make_pair(SmallVector<CCValAssign, 16>(),
                             SmallVector<CCValAssign, 16>());
    for (auto loc : locs) {
        if (loc.isMemLoc()) {
            rv.first.push_back(loc);
        } else {
            rv.second.push_back(loc);
        }
    }
    return rv;
}

SDValue PRUTargetLowering::LowerCall(CallLoweringInfo &call,
                                     SmallVectorImpl<SDValue> &retvals) const {
    // TODO: impl
    assert(!call.IsVarArg);
    assert(call.CallConv == CallingConv::Fast ||
           call.CallConv == CallingConv::C);

    SelectionDAG &sdag = call.DAG;
    SmallVector<CCValAssign, 16> call_locs;
    CCState abi(call.CallConv, call.IsVarArg, sdag.getMachineFunction(),
                call_locs, *call.DAG.getContext());
    abi.AnalyzeCallOperands(call.Outs, pru_call_conv);

    SDValue extrastack =
        sdag.getIntPtrConstant(abi.getNextStackOffset(), call.DL, true);
    auto ptrty = getPointerTy(sdag.getDataLayout());

    auto store_to_mem_locs = [&](ArrayRef<CCValAssign> locs, SDValue ch) {
        SDValue stacktop = sdag.getCopyFromReg(ch, call.DL, PRU::r2, ptrty);
        SmallVector<SDValue, 2> chs;

        for (unsigned n = 0; n < locs.size() && n < call.OutVals.size(); ++n) {
            auto loc = locs[n];
            auto addr = sdag.getNode(
                ISD::ADD, call.DL, ptrty, stacktop,
                sdag.getIntPtrConstant(loc.getLocMemOffset(), call.DL));

            chs.push_back(sdag.getStore(
                ch, call.DL, call.OutVals[n], addr,
                MachinePointerInfo::getStack(sdag.getMachineFunction(),
                                             loc.getLocMemOffset()),
                false, false, 0));
        }

        return sdag.getNode(ISD::TokenFactor, call.DL, MVT::Other, chs);
    };

    auto store_to_reg_locs = [&](ArrayRef<CCValAssign> locs, SDValue ch) {
        SDValue glue;
        for (unsigned n = 0; n < locs.size() && n < call.OutVals.size(); ++n) {
            ch = sdag.getCopyToReg(ch, call.DL, locs[n].getLocReg(),
                                   call.OutVals[n], glue);
            glue = ch.getValue(1);
        }
        return ch;
    };

    SDValue ch = sdag.getCALLSEQ_START(call.Chain, extrastack, call.DL);
    // lower call args
    auto ls = partition_locs(call_locs);
    if (ls.first.size() > 0) {
        ch = store_to_mem_locs(ls.first, ch);
    }
    ch = store_to_reg_locs(ls.second, ch);
    SDValue glue = ch.getValue(1);

    // lower call addr
    GlobalAddressSDNode *callee = dyn_cast<GlobalAddressSDNode>(call.Callee);
    assert(callee);
    SDValue calladdr =
        sdag.getTargetGlobalAddress(callee->getGlobal(), call.DL, ptrty, 0);

    // emit call node
    ch = sdag.getNode(PRUISD::CALL, call.DL,
                      sdag.getVTList(MVT::Other, MVT::Glue), {ch, calladdr});
    glue = ch.getValue(1);

    // restore stack pointer
    ch = sdag.getCALLSEQ_END(ch, extrastack,
                             sdag.getIntPtrConstant(0, call.DL, true), glue,
                             call.DL);
    glue = ch.getValue(1);

    // hook up ret vals
    SmallVector<CCValAssign, 16> rv_locs;
    CCState rvabi(call.CallConv, call.IsVarArg, sdag.getMachineFunction(),
                  rv_locs, *call.DAG.getContext());
    rvabi.AnalyzeCallResult(call.Ins, pru_return_conv);

    for (auto loc : rv_locs) {
        assert(loc.isRegLoc()); // TODO: anything returned in memory?

        SDValue retval = sdag.getCopyFromReg(ch, call.DL, loc.getLocReg(),
                                             loc.getValVT(), glue);
        retvals.push_back(retval);

        ch = retval.getValue(1);
        glue = retval.getValue(2);
    }

    return ch;
}

// sdag builder implicitly switches to using sret if below returns false
bool PRUTargetLowering::CanLowerReturn(
    CallingConv::ID cc, MachineFunction &f, bool vararg,
    const SmallVectorImpl<ISD::OutputArg> &outs, LLVMContext &llmess) const {

    SmallVector<CCValAssign, 16> tmp;
    return CCState(cc, vararg, f, tmp, llmess)
        .CheckReturn(outs, pru_return_conv);
}

SDValue
PRUTargetLowering::LowerReturn(SDValue ch, CallingConv::ID cc, bool vararg,
                               const SmallVectorImpl<ISD::OutputArg> &outs,
                               const SmallVectorImpl<SDValue> &outvals,
                               SDLoc dl, SelectionDAG &sdag) const {

    SmallVector<CCValAssign, 16> locs;
    CCState CCInfo(cc, vararg, sdag.getMachineFunction(), locs,
                   *sdag.getContext());
    CCInfo.AnalyzeReturn(outs, pru_return_conv);

    SmallVector<SDValue, 4> operands;
    SDValue glue;

    if (locs.size() > 0) {
        for (unsigned n = 0; n < locs.size(); ++n) {
            auto loc = locs[n];
            assert(loc.isRegLoc());
            ch = sdag.getCopyToReg(ch, dl, loc.getLocReg(), outvals[n], glue);
            glue = ch.getValue(1);
            operands.push_back(
                sdag.getRegister(loc.getLocReg(), loc.getLocVT()));
        }
        operands.push_back(glue);
    }

    operands.insert(operands.begin(), ch);

    return sdag.getNode(PRUISD::RET_FLAG, dl, MVT::Other, operands);
}

/*
/// getPostIndexedAddressParts - returns true by value, base pointer and
/// offset pointer and addressing mode by reference if this node can be
/// combined with a load / store to form a post-indexed load / store.
bool PRUTargetLowering::getPostIndexedAddressParts(SDNode *N, SDNode *Op,
                                                   SDValue &Base,
                                                   SDValue &Offset,
                                                   ISD::MemIndexedMode &AM,
                                                   SelectionDAG &DAG) const {

    LoadSDNode *LD = cast<LoadSDNode>(N);
    if (LD->getExtensionType() != ISD::NON_EXTLOAD)
        return false;

    EVT VT = LD->getMemoryVT();
    if (VT != MVT::i8 && VT != MVT::i16)
        return false;

    if (Op->getOpcode() != ISD::ADD)
        return false;

    if (ConstantSDNode *RHS = dyn_cast<ConstantSDNode>(Op->getOperand(1))) {
        uint64_t RHSC = RHS->getZExtValue();
        if ((VT == MVT::i16 && RHSC != 2) || (VT == MVT::i8 && RHSC != 1))
            return false;

        Base = Op->getOperand(0);
        Offset = DAG.getConstant(RHSC, SDLoc(N), VT);
        AM = ISD::POST_INC;
        return true;
    }

    return false;
}
*/

const char *PRUTargetLowering::getTargetNodeName(unsigned Opcode) const {
    switch ((PRUISD::NodeType)Opcode) {
    case PRUISD::FIRST_NUMBER:
        break;
    case PRUISD::RET_FLAG:
        return "PRUISD::RET_FLAG";
    case PRUISD::RETI_FLAG:
        return "PRUISD::RETI_FLAG";
    case PRUISD::CALL:
        return "PRUISD::CALL";
    case PRUISD::TargetConst:
        return "PRUISD::TargetConst";
    case PRUISD::BR_CC:
        return "PRUISD::BR_CC";
    case PRUISD::SELECT_CC:
        return "PRUISD::SELECT_CC";
    }
    return nullptr;
}

SDValue PRUTargetLowering::PerformDAGCombine(SDNode *n,
                                             DAGCombinerInfo &comb) const {
    switch (n->getOpcode()) {
        /*case ISD::ZERO_EXTEND:
            dbgs() << "[pru] PerformDAGCombine: zext -> anyext\n";
            return comb.DAG.getNode(ISD::ANY_EXTEND, SDLoc(n),
           n->getValueType(0),
                                    n->getOperand(0));*/
    }

    return SDValue();
}

bool is_register_sized(unsigned n) { return n == 8 || n == 16 || n == 32; }

bool PRUTargetLowering::isTruncateFree(Type *src, Type *smaller) const {
    bool rv = is_register_sized(smaller->getPrimitiveSizeInBits()) &&
              is_register_sized(src->getPrimitiveSizeInBits());
    dbgs() << "[pru] isTruncateFree called on " << src->getPrimitiveSizeInBits()
           << ", " << smaller->getPrimitiveSizeInBits() << ", returning " << rv
           << "\n";
    return rv;
}

bool PRUTargetLowering::isTruncateFree(EVT src, EVT smaller) const {
    bool rv = is_register_sized(smaller.getSizeInBits()) &&
              is_register_sized(src.getSizeInBits());
    dbgs() << "[pru] isTruncateFree called on " << src.getSizeInBits() << ", "
           << smaller.getSizeInBits() << ", returning " << rv << "\n";
    return rv;
}

bool PRUTargetLowering::isZExtFree(Type *src, Type *larger) const {
    bool rv = is_register_sized(larger->getPrimitiveSizeInBits()) &&
              is_register_sized(src->getPrimitiveSizeInBits());
    dbgs() << "[pru] isZExt called on " << src->getPrimitiveSizeInBits() << ", "
           << larger->getPrimitiveSizeInBits() << ", returning " << rv << "\n";
    return rv;
}

bool PRUTargetLowering::isZExtFree(EVT src, EVT larger) const {
    bool rv = is_register_sized(larger.getSizeInBits()) &&
              is_register_sized(src.getSizeInBits());
    dbgs() << "[pru] isZExtFree called on " << src.getSizeInBits() << ", "
           << larger.getSizeInBits() << ", returning " << rv << "\n";
    return rv;
}

unsigned select_cc_to_branch_code(MachineInstr &i) {
    switch (i.getOpcode()) {
    default:
        llvm_unreachable("invalid select_cc op code");
#include "select_to_branch.h"
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
