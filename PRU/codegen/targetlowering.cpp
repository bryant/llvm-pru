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

bool PRUTargetLowering::functionArgumentNeedsConsecutiveRegisters(
    Type *ty, CallingConv::ID cc, bool) const {
    const auto &dl =
        reinterpret_cast<const PRUTargetMachine *>(&getTargetMachine())
            ->getDataLayout();
    return ty->isStructTy() || ty->isArrayTy() || dl.getTypeSizeInBits(ty) > 32;
}

class PruCCState : public CCState {
  public:
    const PRURegisterInfo &reginfo;
    std::map<unsigned, std::vector<unsigned>> frags;

    PruCCState(const PRURegisterInfo &r, CallingConv::ID conv, bool vararg,
               MachineFunction &f, SmallVectorImpl<CCValAssign> &locs,
               LLVMContext &ctx)
        : CCState(conv, vararg, f, locs, ctx), reginfo(r), frags() {}

    void assign_locs_within(unsigned reg, SmallVectorImpl<CCValAssign> &locs) {
        dbgs() << "assigning to parent " << reginfo.getName(reg) << "\n";

        unsigned offset = 0;
        auto inc_offset = [](unsigned &off, unsigned bits, unsigned &parentreg,
                             const PRURegisterInfo &r) {
            off += bits;
            if (off >= r.reg_size(parentreg)) {
                off -= r.reg_size(parentreg);
                parentreg += 1;
            }
        };

        for (CCValAssign &p : locs) {
            unsigned bits = p.getLocVT().getSizeInBits();
            unsigned subreg = reginfo.find_subreg_in(reg, offset, bits);

            if (subreg == 0) {
                dbgs() << "couldn't find a subreg\n";
                // chomp byte-by-byte
                for (unsigned left = 0; left < bits; left += 8) {
                    unsigned subreg = reginfo.find_subreg_in(reg, offset, 8);
                    dbgs() << "collecting bytesub @ " << offset << " of reg "
                           << reginfo.getName(reg) << ": ";
                    assert(subreg && "expected free subreg");
                    dbgs() << reginfo.getName(subreg) << "\n";

                    frags[p.getValNo()].push_back(subreg);
                    inc_offset(offset, 8, reg, reginfo);
                }
                addLoc(CCValAssign::getCustomReg(p.getValNo(), p.getValVT(),
                                                 frags[p.getValNo()][0],
                                                 p.getLocVT(), p.getLocInfo()));
            } else {
                dbgs() << "found a subreg! " << reginfo.getName(subreg) << "\n";
                p.convertToReg(subreg);
                addLoc(p);
                inc_offset(offset, bits, reg, reginfo);
            }
        }
    }

    void analyze_return(const SmallVectorImpl<ISD::OutputArg> &outgoing) {
        SmallVector<CCValAssign, 8> pending;
        unsigned total_bytes = 0;

        for (unsigned n = 0; n < outgoing.size(); ++n) {
            const auto &out = outgoing[n];
            pending.push_back(
                CCValAssign::getPending(n, out.VT, out.VT, CCValAssign::Full));
            total_bytes += out.VT.getStoreSize();
        }
        // if ret size were > 8, llvm should have given us an sret pointer
        assert(total_bytes <= 8);
        assign_locs_within(PRU::r14, pending);
    }
};

static bool alloc_reg_block(unsigned &argnum, MVT &valty, MVT &locty,
                            CCValAssign::LocInfo &loc, ISD::ArgFlagsTy &flags,
                            CCState &cc_) {
    SmallVectorImpl<CCValAssign> &pending = cc_.getPendingLocs();
    pending.push_back(CCValAssign::getPending(argnum, valty, locty, loc));

    if (!flags.isInConsecutiveRegsLast()) {
        return true;
    }

    unsigned total_bytes = 0;
    for (const CCValAssign &p : pending) {
        total_bytes += p.getLocVT().getStoreSize();
    }

    PruCCState &cc = *reinterpret_cast<PruCCState *>(&cc_);

    if (total_bytes == 1) {
        if (unsigned reg = cc.AllocateReg(cc.reginfo.i8_arg_regs())) {
            pending[0].convertToReg(reg);
            cc.addLoc(pending[0]);
            goto fin;
        }
    } else if (total_bytes == 2) {
        if (unsigned reg = cc.AllocateReg(cc.reginfo.i16_arg_regs())) {
            cc.assign_locs_within(reg, pending);
            goto fin;
        }
    } else if (total_bytes <= 4) {
        // TODO: last slot in case 3 is supposed to be free for other args
        if (unsigned reg = cc.AllocateReg(cc.reginfo.i32_arg_regs())) {
            cc.assign_locs_within(reg, pending);
            goto fin;
        }
    } else if (total_bytes == 8) {
        // clpru stores i64s to consecutive reg pairs (if available).
        if (unsigned reg = cc.AllocateRegBlock(cc.reginfo.i32_arg_regs(), 2)) {
            cc.assign_locs_within(reg, pending);
            goto fin;
        }
    }

    // assign to stack
    for (CCValAssign &arg : pending) {
        arg.convertToMem(cc.AllocateStack(arg.getLocVT().getStoreSize(), 1));
        cc.addLoc(arg);
    }
fin:
    pending.clear();
    return true;
}

#include "callingconv.inc"

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

    SmallVector<CCValAssign, 16> args;
    PruCCState CCInfo(*reinterpret_cast<const PRURegisterInfo *>(
                          MF.getSubtarget().getRegisterInfo()),
                      conv, vararg, DAG.getMachineFunction(), args,
                      *DAG.getContext());
    CCInfo.AnalyzeFormalArguments(Ins, pru_call_conv);
    for (unsigned n = 0; n < Ins.size(); ++n) {
        dbgs() << "arg " << n << " " << Ins[n].Flags.isSplit() << " "
               << Ins[n].Flags.getByValSize() << " "
               << Ins[n].ArgVT.getEVTString() << "\n";
    }
    dump_assigns(args);

    auto attach_to_regs = [&](unsigned reg, MVT vt) {
        const TargetRegisterClass *regcls =
            MF.getSubtarget().getRegisterInfo()->getMinimalPhysRegClass(
                reg, vt.SimpleTy);
        unsigned virtreg = MF.addLiveIn(reg, regcls);
        return DAG.getCopyFromReg(chain, dl, virtreg, vt);
    };

    using std::vector;
    using PairBuilderFn =
        std::function<SDValue(unsigned, unsigned, vector<SDValue> &)>;

    PairBuilderFn copy_parts_rec = [&](unsigned s, unsigned e,
                                       vector<SDValue> &ps) {
        assert((ps.size() & (ps.size() - 1)) == 0);
        EVT vt = EVT::getIntegerVT(*DAG.getContext(), (e - s) * 8);
        SDValue low, high;
        dbgs() << "copy_parts_rec: (" << s << ", " << e << ")\n";
        if (e - s > 2) {
            unsigned m = (e - s) / 2;
            low = copy_parts_rec(s, m, ps);
            high = copy_parts_rec(m, e, ps);
        } else {
            low = ps[s];
            high = ps[s + 1];
        }
        return DAG.getNode(ISD::BUILD_PAIR, dl, vt, low, high);
    };

    for (unsigned i = 0; i < args.size(); ++i) {
        CCValAssign &arg = args[i];

        if (arg.isRegLoc() && !arg.needsCustom()) {
            SDValue copy_node = attach_to_regs(arg.getLocReg(), arg.getLocVT());

            dbgs() << "processed a reg loc:\n";
            copy_node.dump();

            InVals.push_back(copy_node);
        } else if (arg.isRegLoc() && arg.needsCustom()) {
            vector<unsigned> &bs = CCInfo.frags[arg.getValNo()];
            vector<SDValue> ps;
            for (auto b = bs.begin(); b != bs.end(); ++b) {
                ps.push_back(attach_to_regs(*b, MVT::getIntegerVT(8)));
            }
            InVals.push_back(copy_parts_rec(0, ps.size(), ps));
        } else if (arg.isMemLoc() && Ins[i].Flags.isByVal()) {
            unsigned argsize = Ins[i].Flags.getByValSize();
            int frameidx =
                MFI->CreateFixedObject(argsize, arg.getLocMemOffset(), true);
            SDValue getframe =
                DAG.getFrameIndex(frameidx, getPointerTy(DAG.getDataLayout()));
            InVals.push_back(getframe);

            dbgs() << "processed a byval loc of size " << argsize << ":\n";
            getframe.dump();
        } else if (arg.isMemLoc()) {
            unsigned argsize = arg.getLocVT().getStoreSize();
            int frameidx =
                MFI->CreateFixedObject(argsize, arg.getLocMemOffset(), true);
            SDValue getframe =
                DAG.getFrameIndex(frameidx, getPointerTy(DAG.getDataLayout()));
            SDValue load_node =
                DAG.getLoad(arg.getValVT(), dl, chain, getframe,
                            MachinePointerInfo(), false, false, false, 0);

            InVals.push_back(load_node);

            dbgs() << "processed a mem loc of size " << argsize << ":\n";
            load_node.dump();
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
    const SmallVectorImpl<ISD::OutputArg> &outs, LLVMContext &ctx) const {
    unsigned total_bytes = 0;
    for (const auto &out : outs) {
        total_bytes += out.VT.getStoreSize();
    }
    if (total_bytes <= 4 || total_bytes == 8) {
        return true;
    } else {
        // instruct llvm to use sret
        return false;
    }
}

SDValue
PRUTargetLowering::LowerReturn(SDValue ch, CallingConv::ID cc, bool vararg,
                               const SmallVectorImpl<ISD::OutputArg> &outs,
                               const SmallVectorImpl<SDValue> &outvals,
                               SDLoc dl, SelectionDAG &sdag) const {

    SmallVector<CCValAssign, 16> locs;
    auto reginfo = reinterpret_cast<const PRURegisterInfo *>(
        sdag.getSubtarget().getRegisterInfo());
    PruCCState CCInfo(*reginfo, cc, vararg, sdag.getMachineFunction(), locs,
                      *sdag.getContext());
    CCInfo.analyze_return(outs);

    if (locs.size() == 0) {
        return sdag.getNode(PRUISD::RET_FLAG, dl, MVT::Other, {ch});
    }

    using I8SplitterFn =
        std::function<void(SmallVector<SDValue, 4> &, SDValue, unsigned)>;

    I8SplitterFn extract_i8s = [&](SmallVector<SDValue, 4> &i8s, SDValue reg,
                                   unsigned width) {
        if (width == 8) {
            i8s.push_back(reg);
        } else {
            MVT vt = MVT::getIntegerVT(width / 2);
            SDValue low = sdag.getNode(ISD::EXTRACT_ELEMENT, dl, vt, reg,
                                       sdag.getIntPtrConstant(0, dl));
            SDValue high = sdag.getNode(ISD::EXTRACT_ELEMENT, dl, vt, reg,
                                        sdag.getIntPtrConstant(1, dl));
            dbgs() << "pushing low " << width / 2 << "\n";
            extract_i8s(i8s, low, width / 2);
            dbgs() << "pushing high " << width / 2 << "\n";
            extract_i8s(i8s, high, width / 2);
        }
    };

    SmallVector<SDValue, 4> operands;
    SDValue glue;

    auto copy_to_reg = [&](unsigned reg, SDValue val, MVT vt) {
        ch = sdag.getCopyToReg(ch, dl, reg, val, glue);
        glue = ch.getValue(1);
        operands.push_back(sdag.getRegister(reg, vt));
    };

    for (unsigned n = 0; n < locs.size(); ++n) {
        CCValAssign &loc = locs[n];
        assert(loc.isRegLoc());
        if (loc.needsCustom()) {
            SmallVector<SDValue, 4> i8s;
            extract_i8s(i8s, outvals[n], loc.getValVT().getSizeInBits());
            dbgs() << "#" << loc.getValNo() << ": extracted " << i8s.size()
                   << " pieces, corresponding to "
                   << CCInfo.frags[loc.getValNo()].size() << " i8 frags\n";
            for (unsigned i = 0; i < i8s.size(); ++i) {
                copy_to_reg(CCInfo.frags[loc.getValNo()][i], i8s[i], MVT::i8);
            }
        } else {
            copy_to_reg(loc.getLocReg(), outvals[n], loc.getLocVT());
        }
    }

    operands.push_back(glue);
    operands.insert(operands.begin(), ch);

    return sdag.getNode(PRUISD::RET_FLAG, dl, MVT::Other, operands);
}

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
    // placeholder
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
