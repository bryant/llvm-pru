#pragma once

#include "abi/cc.h"
#include "registerinfo.h"
#include "targetdesc.h"

using namespace llvm;

using ISD::InputArg;
using ISD::OutputArg;
using std::function;

template <typename ArgTy> unsigned arg_size(ArrayRef<ArgTy> m) {
    return std::accumulate(m.begin(), m.end(), 0, [](unsigned tot, ArgTy b) {
        return tot + b.VT.getStoreSize();
    });
}

template <typename ArgTy>
unsigned allocate_reg(CCState &alloc, ArrayRef<ArgTy> members,
                      const PRURegisterInfo &reginfo) {
    switch (arg_size(members)) {
    case 1:
        return alloc.AllocateReg(reginfo.i8_arg_regs());
    case 2:
        return alloc.AllocateReg(reginfo.i16_arg_regs());
    case 3:
    case 4:
        // TODO: last slot in case 3 is supposed to be free for other args
        return alloc.AllocateReg(reginfo.i32_arg_regs());
    case 8:
        // clpru stores i64s to consecutive reg pairs (if available).
        return alloc.AllocateRegBlock(reginfo.i32_arg_regs(), 2);
    }
    return 0;
}

template <typename ArgTy>
vector<vector<ArgTy>> collect_members(ArrayRef<ArgTy> args) {
    vector<vector<ArgTy>> rv;
    for (auto arg = args.begin(); arg != args.end(); ++arg) {
        if (arg->Flags.isInConsecutiveRegs()) {
            vector<ArgTy> members;
            while (!arg->Flags.isInConsecutiveRegsLast() && arg != args.end()) {
                members.push_back(*arg);
                ++arg;
            }
            if (arg != args.end()) {
                members.push_back(*arg);
            }
            rv.push_back(members);
        } else {
            rv.push_back({*arg});
        }
    }
    return rv;
}

template <typename ArgTy>
vector<vector<unsigned>> alloc_subregs(unsigned reg, ArrayRef<ArgTy> members,
                                       const PRURegisterInfo &reginfo) {
    vector<vector<unsigned>> rv;
    unsigned offset = 0;
    auto inc_offset = [](unsigned &off, unsigned bits, unsigned &parentreg,
                         const PRURegisterInfo &r) {
        off += bits;
        if (off >= r.reg_size_bits(parentreg)) {
            off -= r.reg_size_bits(parentreg);
            parentreg += 1;
        }
    };
    for (ArgTy m : members) {
        unsigned bits = m.VT.getSizeInBits();
        if (unsigned subreg = reginfo.find_subreg_in(reg, offset, bits)) {
            dbgs() << "found a subreg! " << reginfo.getName(subreg) << "\n";
            rv.push_back({subreg});
            inc_offset(offset, bits, reg, reginfo);
        } else {
            // accumulate i8s and assemble
            vector<unsigned> reg8s;
            for (unsigned left = 0; left < bits; left += 8) {
                unsigned reg8 = reginfo.find_subreg_in(reg, offset, 8);
                dbgs() << "collecting bytesub @ " << offset << " of reg "
                       << reginfo.getName(reg) << ": ";
                assert(reg8 && "expected free subreg");
                dbgs() << reginfo.getName(reg8) << "\n";
                reg8s.push_back(reg8);
                inc_offset(offset, 8, reg, reginfo);
            }
            rv.push_back(reg8s);
        }
    }
    return rv;
}

struct TexasCC : public CC {
    bool vararg;
    SelectionDAG &sdag;
    MachineFunction &func;
    const PRURegisterInfo &reginfo;
    std::map<unsigned, std::vector<unsigned>> frags;
    const MVT ptrvt;

    // TODO: var arg support
    TexasCC(SelectionDAG &sdag_, bool vararg_)
        : vararg(vararg_), sdag(sdag_), func(sdag.getMachineFunction()),
          reginfo(*reinterpret_cast<const PRURegisterInfo *>(
              sdag.getSubtarget().getRegisterInfo())),
          ptrvt(
              MVT::getIntegerVT(sdag.getDataLayout().getPointerSizeInBits(0))) {
    }

    CCState mk_alloc() const {
        SmallVector<CCValAssign, 1> none;
        return CCState(CallingConv::C, false, func, none, *sdag.getContext());
    }

    SDValue build_from_reg8s(SDLoc dl,
                             function<SDValue(unsigned, MVT)> get_copy,
                             vector<unsigned> reg8s) {
        assert((reg8s.size() & (reg8s.size() - 1)) == 0);
        function<SDValue(unsigned, unsigned)> rec = [&](unsigned s,
                                                        unsigned e) {
            EVT vt(MVT::getIntegerVT((e - s) * 8));
            if (e - s > 2) {
                unsigned m = (e - s) / 2;
                return sdag.getNode(ISD::BUILD_PAIR, dl, vt, rec(s, m),
                                    rec(m, e));
            } else {
                return sdag.getNode(ISD::BUILD_PAIR, dl, vt,
                                    get_copy(reg8s[s], MVT::i8),
                                    get_copy(reg8s[s + 1], MVT::i8));
            }
        };
        return rec(0, reg8s.size());
    }

    // decomposes a `val` into reg8s
    SDVals extract_to_reg8s(SDValue &ch, SDValue &glue, SDLoc dl, SDValue val,
                            MVT vt, ArrayRef<unsigned> reg8s) {
        assert((vt.getSizeInBits() & (vt.getSizeInBits() - 1)) == 0);
        assert(vt.getStoreSize() == reg8s.size());
        vector<SDValue> rv;
        function<void(SDValue, unsigned, unsigned)> rec = [&](
            SDValue val, unsigned width, unsigned regidx) {
            if (width == 8) {
                ch = sdag.getCopyToReg(ch, dl, reg8s[regidx], val, glue);
                glue = ch.getValue(1);
                rv.push_back(sdag.getRegister(reg8s[regidx], MVT::i8));
            } else {
                MVT vt = MVT::getIntegerVT(width / 2);
                SDValue low = sdag.getNode(ISD::EXTRACT_ELEMENT, dl, vt, val,
                                           sdag.getIntPtrConstant(0, dl));
                SDValue high = sdag.getNode(ISD::EXTRACT_ELEMENT, dl, vt, val,
                                            sdag.getIntPtrConstant(1, dl));
                rec(low, width / 2, regidx);
                rec(high, width / 2, regidx + width / (8 * 2));
            }
        };
        rec(val, vt.getSizeInBits(), 0);
        return rv;
    }

    SDVals lower_formal_args(SDValue &ch, SDLoc dl,
                             ArrayRef<InputArg> args) override {
        CCState alloc = mk_alloc();
        vector<SDValue> rv;
        MachineFrameInfo &fr = *func.getFrameInfo();

        auto get_copy_node = [&](unsigned reg, MVT vt) {
            unsigned virtreg = func.addLiveIn(
                reg, reginfo.getMinimalPhysRegClass(reg, vt.SimpleTy));
            return sdag.getCopyFromReg(ch, dl, virtreg, vt);
        };
        for (ArrayRef<InputArg> members : collect_members(args)) {
            if (members[0].Flags.isByVal()) {
                unsigned size = members[0].Flags.getByValSize();
                unsigned offset = alloc.AllocateStack(size, 1);
                int fi = fr.CreateFixedObject(size, offset, true);
                rv.push_back(sdag.getFrameIndex(fi, ptrvt));
            } else if (unsigned reg = allocate_reg(alloc, members, reginfo)) {
                unsigned i = 0;
                for (const auto &subs : alloc_subregs(reg, members, reginfo)) {
                    if (subs.size() > 1) {
                        rv.push_back(build_from_reg8s(dl, get_copy_node, subs));
                    } else {
                        rv.push_back(get_copy_node(subs[0], members[i].VT));
                    }
                    i += 1;
                }
            } else {
                // assign to stack
                for (const auto &m : members) {
                    unsigned size = m.VT.getStoreSize();
                    unsigned offset = alloc.AllocateStack(size, 1);
                    int fi = fr.CreateFixedObject(size, offset, true);
                    rv.push_back(sdag.getLoad(
                        m.VT, dl, ch, sdag.getFrameIndex(fi, ptrvt),
                        MachinePointerInfo::getFixedStack(func, fi), false,
                        false, false, 0));
                }
            }
        }
        return rv;
    }

    // like formal args, except one arg = retval.
    SDVals lower_call_result(SDValue &ch, SDValue &glue, SDLoc dl,
                             ArrayRef<InputArg> args) override {
        // larger than two reg32s should have been passed to sret
        assert(arg_size(args) <= 8);
        CCState alloc = mk_alloc();
        vector<SDValue> rv;
        if (args.size() > 0) {
            unsigned reg = allocate_reg(alloc, args, reginfo);
            assert(reg);
            auto get_copy_node = [&](unsigned reg, MVT vt) {
                SDValue val = sdag.getCopyFromReg(ch, dl, reg, vt, glue);
                ch = val.getValue(1);
                glue = val.getValue(2);
                return val;
            };
            unsigned i = 0;
            for (const auto &subs : alloc_subregs(reg, args, reginfo)) {
                if (subs.size() > 1) {
                    rv.push_back(build_from_reg8s(dl, get_copy_node, subs));
                } else {
                    rv.push_back(get_copy_node(subs[0], args[i].VT));
                }
                i += 1;
            }
        }
        return rv;
    }

    // returns an array of operands for the ret node
    SDVals lower_return(SDValue &ch, SDValue &glue, SDLoc dl,
                        ArrayRef<OutputArg> args,
                        const ArrayRef<SDValue> &vals) override {
        assert(arg_size(args) <= 8);
        CCState alloc = mk_alloc();
        vector<SDValue> rv;
        if (args.size() > 0) {
            unsigned reg = allocate_reg(alloc, args, reginfo);
            assert(reg);
            unsigned i = 0;
            for (const auto &subs : alloc_subregs(reg, args, reginfo)) {
                if (subs.size() > 1) {
                    auto i8s = extract_to_reg8s(ch, glue, dl, vals[i],
                                                args[i].VT, subs);
                    rv.insert(rv.end(), i8s.begin(), i8s.end());
                } else {
                    ch = sdag.getCopyToReg(ch, dl, subs[0], vals[i], glue);
                    glue = ch.getValue(1);
                    rv.push_back(sdag.getRegister(subs[0], args[i].VT));
                }
                i += 1;
            }
        }
        return rv;
    }

    struct Three {
        unsigned validx;
        ArrayRef<OutputArg> members;
        unsigned reg;
    };

    // returns an array of operands for call node
    SDVals lower_call(SDValue &ch, SDValue &glue, SDLoc dl,
                      ArrayRef<ISD::OutputArg> args,
                      const ArrayRef<SDValue> &vals) override {
        CCState alloc = mk_alloc();
        vector<SDValue> rv;
        SmallVector<Three, 16> later;
        SDValue basereg = sdag.getCopyFromReg(ch, dl, PRU::r2, ptrvt);

        unsigned v = 0;
        for (ArrayRef<OutputArg> members : collect_members(args)) {
            if (members[0].Flags.isByVal()) {
                assert(members.size() == 1);
                unsigned size = members[0].Flags.getByValSize();
                unsigned align = members[0].Flags.getByValAlign();
                unsigned offset = alloc.AllocateStack(size, 1);
                SDValue addr = sdag.getNode(ISD::ADD, dl, ptrvt, basereg,
                                            sdag.getIntPtrConstant(offset, dl));
                rv.push_back(sdag.getMemcpy(
                    ch, dl, addr, vals[v], sdag.getConstant(size, dl, MVT::i32),
                    align, false, false, false,
                    MachinePointerInfo::getStack(func, offset),
                    MachinePointerInfo()));
            } else if (unsigned reg = allocate_reg(alloc, members, reginfo)) {
                // handle reg args only after stack args have been passed.
                later.push_back(Three{v, members, reg});
            } else {
                // assign to stack
                for (const auto &m : members) {
                    unsigned offset =
                        alloc.AllocateStack(m.VT.getStoreSize(), 1);
                    SDValue addr =
                        sdag.getNode(ISD::ADD, dl, ptrvt, basereg,
                                     sdag.getIntPtrConstant(offset, dl));
                    rv.push_back(sdag.getStore(
                        ch, dl, vals[v], addr,
                        MachinePointerInfo::getStack(func, offset), false,
                        false, 0));
                }
            }
            v += members.size();
        }

        if (!rv.empty()) {
            ch = sdag.getNode(ISD::TokenFactor, dl, MVT::Other, rv);
        }
        rv.clear();

        for (const Three t : later) {
            unsigned i = t.validx;
            for (const auto &subs : alloc_subregs(t.reg, t.members, reginfo)) {
                if (subs.size() > 1) {
                    auto i8s = extract_to_reg8s(ch, glue, dl, vals[i],
                                                args[i].VT, subs);
                    rv.insert(rv.end(), i8s.begin(), i8s.end());
                } else {
                    ch = sdag.getCopyToReg(ch, dl, subs[0], vals[i], glue);
                    glue = ch.getValue(1);
                    rv.push_back(sdag.getRegister(subs[0], args[i].VT));
                }
                i += 1;
            }
        }
        return rv;
    }

    unsigned compute_call_stack(ArrayRef<OutputArg> args) override {
        CCState alloc = mk_alloc();
        for (ArrayRef<OutputArg> members : collect_members(args)) {
            if (members[0].Flags.isByVal()) {
                assert(members.size() == 1);
                unsigned size = members[0].Flags.getByValSize();
                alloc.AllocateStack(size, 1);
            } else if (allocate_reg(alloc, members, reginfo)) {
            } else {
                for (const auto &m : members) {
                    alloc.AllocateStack(m.VT.getStoreSize(), 1);
                }
            }
        }
        return alloc.getNextStackOffset();
    }

    static bool needs_sret(ArrayRef<OutputArg> args) {
        unsigned s = arg_size(args);
        return !(s <= 4 || s == 8);
    }
};
