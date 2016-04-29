#include <algorithm>
#include <functional>
#include <utility>
#include <vector>

#include "llvm/CodeGen/LiveIntervalAnalysis.h"
#include "llvm/CodeGen/LiveRegMatrix.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/VirtRegMap.h"
#include "llvm/Support/Debug.h"

#include "instrinfo.h"
#include "targetdesc.h"
#include "targetmachine.h"

#define DEBUG_TYPE "mem-op-combiner"

namespace pru {

using namespace llvm;
using std::pair;
using std::vector;
using std::function;

using BaseReg = unsigned;
using Offset = int64_t;
using RegSize = PRURegisterInfo::RegSize;

template <typename T> bool intervals_intersect(T s0, T e0, T s1, T e1) {
    bool rv = !(s0 > e1 || e0 < s1);
    return rv;
}

struct MemLoc {
    enum Kind { BaseOffset, FrameSlot, AlwaysAliases };
    enum OpSize { Byte = 1, Word = 2, DWord = 4 };

    Kind kind;
    MachineInstr *i;
    Offset start;
    Offset end;
    OpSize size;

    union {
        BaseReg base;
        int fi;
    };

    static OpSize op_size(const MachineInstr &i) {
        auto r = static_cast<Offset>((*i.memoperands_begin())->getSize());
        switch (r) {
        case 1:
            return Byte;
        case 2:
            return Word;
        case 4:
            return DWord;
        default:
            llvm_unreachable("invalid load size encountered.");
        }
    }

    static MemLoc from_mi(MachineInstr &ii) {
        MemLoc rv;
        if (ii.getOperand(1).isReg() && ii.getOperand(2).isImm()) {
            rv.kind = BaseOffset;
            rv.base = ii.getOperand(1).getReg();
        } else if (ii.getOperand(1).isFI() && ii.getOperand(2).isImm()) {
            rv.fi = ii.getOperand(1).getIndex();
            rv.kind = FrameSlot;
        } else {
            return {AlwaysAliases, &ii};
        }

        rv.i = &ii;
        rv.start = ii.getOperand(2).getImm();
        rv.size = op_size(ii);
        rv.end = rv.start + rv.size - 1;
        return rv;
    }

    bool could_alias(const MemLoc &other) const {
        if (kind == AlwaysAliases || other.kind == AlwaysAliases ||
            kind != other.kind) {
            return true;
        }

        if (kind == BaseOffset) {
            if (base == other.base) {
                return intervals_intersect(start, end, other.start, other.end);
            }
        } else if (kind == FrameSlot) {
            if (fi == other.fi) {
                return intervals_intersect(start, end, other.start, other.end);
            }
            return false;
        }

        return true;
    }

    bool compat_with(const MemLoc &other) const {
        if (kind == FrameSlot && other.kind == FrameSlot && other.fi != fi) {
            return false;
        }
        return !could_alias(other);
    }

    bool operator<(const MemLoc &other) {
        // assert(kind == other.kind);
        if (kind == MemLoc::BaseOffset) {
            return start < other.start;
        } else if (kind == MemLoc::FrameSlot) {
            return start < other.start;
        }
        return true;
    }

    friend raw_ostream &operator<<(raw_ostream &o, const MemLoc &l) {
        switch (l.kind) {
        case MemLoc::BaseOffset:
            o << "BaseOffset(" << l.base << ", " << l.start << ", " << l.end
              << ") " << *l.i;
            return o;
        case MemLoc::FrameSlot:
            o << "FrameSlot(" << l.fi << ", " << l.start << ", " << l.end
              << ") " << *l.i;
            return o;
        default:
            o << "AlwaysAliases " << *l.i;
            return o;
        }
    }
};

struct Cluster {
    vector<MemLoc> memops;
    unsigned index;

    bool can_accept(const MemLoc &loc) const {
        return all_of(memops.begin(), memops.end(), [&](MemLoc l) {
            bool rv = loc.compat_with(l);
            return rv;
        });
    }

    void sort() { std::sort(std::next(memops.begin()), memops.end()); }

    friend raw_ostream &operator<<(raw_ostream &o, const Cluster &c) {
        o << "cluster anchored at " << c.index << ":\n";
        for (auto i : c.memops) {
            o << "\t" << i;
        }
        return o;
    }
};

struct Collector {
    using Classifier = function<bool(const MachineInstr &)>;

    vector<Cluster> clusters;
    vector<pair<MemLoc, unsigned>> aliasables;
    unsigned curidx;
    Classifier is_clusterable;
    Classifier could_alias_clusters;

    Collector(Classifier c, Classifier a)
        : curidx(0), is_clusterable(c), could_alias_clusters(a) {}

    bool has_previous_collision(MemLoc loc, unsigned &idx) const {
        for (auto k = aliasables.crbegin(); k != aliasables.crend(); ++k) {
            const pair<MemLoc, unsigned> &seen = *k;
            if (loc.could_alias(seen.first)) {
                idx = seen.second;
                return true;
            }
        }
        return false;
    }

    void add_to_clusters(const MemLoc &loc,
                         function<bool(Cluster &)> extra_check) {
        for (Cluster &c : clusters) {
            if (extra_check(c) && c.can_accept(loc)) {
                c.memops.push_back(loc);
                return;
            }
        }
        // couldn't find one
        clusters.push_back({{loc}, curidx});
    }

    void update(MachineInstr &i) {
        if (is_clusterable(i)) {
            const auto loc = MemLoc::from_mi(i);
            unsigned mrc;
            if (has_previous_collision(loc, mrc)) {
                add_to_clusters(loc,
                                [mrc](Cluster &c) { return c.index > mrc; });
            } else {
                add_to_clusters(loc, [](Cluster &c) { return true; });
            }
        }
        if (could_alias_clusters(i)) {
            const auto loc = MemLoc::from_mi(i);
            aliasables.push_back(std::make_pair(loc, curidx));
        }
        ++curidx;
    }

    void operator()(MachineInstr &i) { return update(i); }
};

bool clusterable_load(const MachineInstr &i) {
    if (PRUInstrInfo::is_load(i.getOpcode()) && !i.hasOrderedMemoryRef()) {
        return true;
    }
    return false;
}

bool clusterable_store(const MachineInstr &i) {
    if (PRUInstrInfo::is_store(i.getOpcode()) && !i.hasOrderedMemoryRef()) {
        return true;
    }
    return false;
};

bool is_load_or_store(const MachineInstr &i) {
    if (PRUInstrInfo::is_load(i.getOpcode()) ||
        PRUInstrInfo::is_store(i.getOpcode())) {
        return true;
    }
    return false;
}

static constexpr size_t REG_FILE_BYTES = 32 * 4;

using RegBits = std::bitset<REG_FILE_BYTES>;

// one bit = one possible starting position
using FreePlaces = RegBits;

raw_ostream &operator<<(raw_ostream &o, const FreePlaces &l) {
    for (unsigned n = 0; n < l.size(); n += 1) {
        o << (l[n] ? "1" : "0");
        if (n % 4 == 3)
            o << " ";
    }
    return o;
}

template <typename T, size_t n> static constexpr size_t len(const T (&xs)[n]) {
    return n;
}

// TODO: fix this clunky shit.
#include "register_lists.h"

struct RegMask {
    unsigned shift; // position of parent 4-byte
    unsigned mask32;
    unsigned mask16;
    unsigned mask8;
};

unsigned reg_at_pos(unsigned offset, MemLoc::OpSize size) {
    switch (size) {
    case MemLoc::Byte:
        return all_reg8s[offset];
    case MemLoc::Word:
        return all_reg16s[(offset / 4) * 3 + (offset % 4)];
    case MemLoc::DWord:
        return all_reg32s[offset / 4];
    }
}

static std::map<unsigned, RegMask> build_masks() {
    std::map<unsigned, RegMask> rv;
    for (unsigned i = 0; i < len(all_reg8s); ++i) {
        static const unsigned mask16s[] = {0x01, 0x03, 0x06, 0x04};
        rv[all_reg8s[i]] = {i / 4, 1, mask16s[i % 4],
                            static_cast<unsigned>(1 << (i % 4))};
    }
    for (unsigned w = 0, i = 0; i < len(all_reg16s); ++w) {
        static const unsigned mask16s[] = {0x03, 0x07, 0x06};
        if (w % 4 != 3) { // w0, w1, w2, but not "w3"
            rv[all_reg16s[i]] = {w / 4, 1, mask16s[w % 4],
                                 static_cast<unsigned>(0x03 << (w % 4))};
            i += 1;
        }
    }
    for (unsigned i = 0; i < len(all_reg32s); ++i) {
        rv[all_reg32s[i]] = {i, 1, 0x07, 0x0f};
    }
    return rv;
}

static const std::map<unsigned, RegMask> reg_mask = build_masks();

template <size_t n> constexpr RegBits bitpat(RegBits pat) {
    return pat << (n * 4) | bitpat<n - 1>(pat);
}

template <> constexpr RegBits bitpat<0>(RegBits pat) { return pat; }

FreePlaces mask_for(MemLoc::OpSize size) {
    switch (size) {
    case MemLoc::Byte:
        return bitpat<32>(RegBits("1111"));
    case MemLoc::Word:
        return bitpat<32>(RegBits("0111"));
    case MemLoc::DWord:
        return bitpat<32>(RegBits("0001"));
    }
}

// one bit = one free reg8
struct FreeRegs {
    FreePlaces free32;
    FreePlaces free16;
    FreePlaces free8;
    const PRURegisterInfo &tri;

    FreeRegs(const MachineFunction &f, const LiveRegMatrix &lrm)
        : free32(mask_for(MemLoc::DWord)), free16(mask_for(MemLoc::Word)),
          free8(mask_for(MemLoc::Byte)),
          tri(*reinterpret_cast<const PRURegisterInfo *>(
              f.getSubtarget().getRegisterInfo())) {
        BitVector reserved = tri.getReservedRegs(f);
        for (unsigned reg = PRU::NoRegister + 1; reg < PRU::NUM_TARGET_REGS;
             reg += 1) {
            if (reserved.test(reg)) {
                dbgs() << "reserved reg: " << tri.getName(reg) << "\n";
                add_live_one(reg);
            }
        }

        BitVector usable_cs;
        f.getSubtarget().getFrameLowering()->determineCalleeSaves(
            const_cast<MachineFunction &>(f), usable_cs, nullptr);
        const MCPhysReg *csregs = tri.getCalleeSavedRegs(&f);
        for (unsigned i = 0; csregs[i] != 0; i += 1) {
            if (!lrm.isPhysRegUsed(csregs[i])) {
                dbgs() << "unusable callee saved reg: "
                       << tri.getName(csregs[i]) << "\n";
                add_live(csregs[i]);
            }
        }
    }

    void add_live_one(unsigned physreg) {
        switch (tri.reg_size(physreg)) {
        case RegSize::Byte:
            free8 &= ~(RegBits(1) << tri.reg_offset(physreg));
            break;
        case RegSize::Word:
            free16 &= ~(RegBits(1) << tri.reg_offset(physreg));
            break;
        case RegSize::DWord:
            free32 &= ~(RegBits(1) << tri.reg_offset(physreg));
            break;
        }
    }

    void add_live(unsigned preg) {
        for (MCRegAliasIterator r(preg, &tri, true); r.isValid(); ++r) {
            add_live_one(*r);
        }
    }

    FreePlaces places_for(const MemLoc &m) const {
        switch (m.size) {
        case MemLoc::Byte:
            return free8 & mask_for(m.size);
        case MemLoc::Word:
            return free16 & mask_for(m.size);
        case MemLoc::DWord:
            return free32 & mask_for(m.size);
        }
    }

    FreePlaces fit(const vector<MemLoc> &mems) const {
        assert(mems.size() > 0);
        FreePlaces rv = this->places_for(mems[0]);
        unsigned offset = mems[0].size;
        for (MemLoc m : make_range(std::next(mems.begin()), mems.end())) {
            rv &= this->places_for(m) >> offset;
            if (rv.none()) {
                return rv;
            } else {
                offset += m.size;
            }
        }
        return rv;
    }

    friend raw_ostream &operator<<(raw_ostream &o, const FreeRegs &f) {
        o << "\n\t" << f.free32 << "\n\t" << f.free16 << "\n\t" << f.free8;
        return o;
    }
};

struct SlotRange {
    SlotIndex s;
    SlotIndex e;
    SlotRange convex(SlotRange b) const {
        return {std::min(s, b.s), std::max(e, b.e)};
    }

    friend raw_ostream &operator<<(raw_ostream &o, const SlotRange &s) {
        o << "[" << s.s << ", " << s.e << ")";
        return o;
    }
};

struct Segment {
    vector<MemLoc> ops;
    size_t len_bytes;
    FreePlaces starts;
    SlotRange live;
};

struct LoadMerger : public MachineFunctionPass {
    struct IntervalIter {
        LiveIntervals &li;
        unsigned cur;
        bool operator!=(IntervalIter other) const { return cur != other.cur; }
        IntervalIter operator++() {
            ++cur;
            return *this;
        }
        LiveInterval &operator*() {
            return li.getInterval(TargetRegisterInfo::index2VirtReg(cur));
        }
    };

    static char id;
    LiveIntervals *li;
    VirtRegMap *vmap;
    LiveRegMatrix *lrm;
    const MachineRegisterInfo *mri;
    const TargetRegisterInfo *tri;

    LoadMerger() : MachineFunctionPass(id) {}

    const char *getPassName() const override { return "PRU LBBO Merger"; }

    iterator_range<IntervalIter> intervals() const {
        return make_range(IntervalIter{*li, 0}, {*li, mri->getNumVirtRegs()});
    }

    pair<FreePlaces, bool> place_next(const MachineFunction &f,
                                      const MemLoc &cand, const Segment &seg) {
        dbgs() << "placing " << cand;
        bool at_msb = cand.start == seg.ops.back().end + 1;
        bool at_lsb = cand.end + 1 == seg.ops.front().start;

        // check for placement
        vector<MemLoc> new_cluster = seg.ops;
        if (at_msb) {
            dbgs() << " belongs to end\n";
            new_cluster.push_back(cand);
        } else if (at_lsb) {
            dbgs() << " belongs at start\n";
            new_cluster.insert(new_cluster.begin(), cand);
        } else {
            dbgs() << " doesn't fit.\n";
            return make_pair(FreePlaces(), false);
        }

        vector<unsigned> ks;
        for (const MemLoc m : new_cluster) {
            ks.push_back(li->getInterval(m.i->getOperand(0).getReg()).reg);
        }
        SlotRange newlive = covering(cand).convex(seg.live);
        dbgs() << "current live: " << seg.live << " `convex` " << covering(cand)
               << " = " << newlive << "\n";
        FreeRegs free = free_within(f, newlive.s, newlive.e, ks);
        dbgs() << "free slots: " << free << "\n";
        return make_pair(free.fit(new_cluster), at_msb);
    }

    FreeRegs free_within(const MachineFunction &f, SlotIndex s, SlotIndex e,
                         vector<unsigned> ks) const {
        FreeRegs rv(f, *lrm);
        dbgs() << "free_within: free = " << rv << "\n";
        for (LiveInterval &iv : intervals()) {
            if (vmap->hasPhys(iv.reg) && iv.overlaps(s, e) &&
                std::find(ks.begin(), ks.end(), iv.reg) == ks.end()) {
                dbgs() << "found live range that intersects: " << iv << "\n";
                dbgs() << "clearing " << tri->getName(vmap->getPhys(iv.reg))
                       << " with range of " << iv.beginIndex() << ", "
                       << iv.endIndex() << "\n";
                rv.add_live(vmap->getPhys(iv.reg));
                dbgs() << "updated free slots to: " << rv << "\n";
            }
        }
        return rv;
    }

    // live range of the loadee
    SlotRange covering(MemLoc m) {
        const auto &iv = li->getInterval(m.i->getOperand(0).getReg());
        return SlotRange{iv.beginIndex(), iv.endIndex()};
    }

    bool runOnMachineFunction(MachineFunction &f) override {
        li = &getAnalysis<LiveIntervals>();
        vmap = &getAnalysis<VirtRegMap>();
        mri = &f.getRegInfo();
        tri = f.getSubtarget().getRegisterInfo();
        const TargetInstrInfo &tii = *f.getSubtarget().getInstrInfo();
        lrm = &getAnalysis<LiveRegMatrix>();

        for (MachineBasicBlock &b : f) {
            auto c = for_each(b.begin(), b.end(),
                              Collector(clusterable_load, is_load_or_store));
            for (Cluster &cluster : c.clusters) {
                dbgs() << "cluster:\n" << cluster << "\n";
                vector<MemLoc> &memops = cluster.memops;
                while (memops.size() > 1) {
                    Segment s = {{memops[0]},
                                 memops[0].size,
                                 mask_for(memops[0].size),
                                 covering(memops[0])};
                    memops.erase(memops.begin());
                    dbgs() << "starting segment with " << *s.ops[0].i;
                    for (auto m = memops.begin(); m != memops.end();) {
                        auto canplace = place_next(f, *m, s);
                        if (canplace.first.none()) {
                            ++m;
                        } else {
                            if (canplace.second) {
                                s.ops.push_back(*m);
                            } else {
                                s.ops.insert(s.ops.begin(), *m);
                            }
                            s.len_bytes += m->size;
                            s.starts = canplace.first;
                            s.live = s.live.convex(covering(*m));
                            memops.erase(m);
                            m = memops.begin();
                            dbgs() << "worked: " << s.starts << "\n";
                        }
                    }
                    if (s.ops.size() > 1) {
                        dbgs() << "success!\n";
                        for (MemLoc ss : s.ops) {
                            dbgs() << ss << "\n";
                        }
                        dbgs() << "places: " << s.starts << "\n";

                        MachineInstr &ins =
                            *std::min_element(
                                 s.ops.begin(), s.ops.end(),
                                 [&](const MemLoc &a, const MemLoc &b) {
                                     return li->getInstructionIndex(a.i) <
                                            li->getInstructionIndex(b.i);
                                 })
                                 ->i;
                        const MachineInstrBuilder &batch =
                            BuildMI(b, &ins, ins.getDebugLoc(),
                                    tii.get(PRU::lbbo_multiple))
                                .addOperand(s.ops[0].i->getOperand(1)) // base
                                .addOperand(s.ops[0].i->getOperand(2)) // offset
                                .addImm(s.len_bytes);

                        unsigned pos;
                        for (size_t n = 0; n < s.starts.size(); n += 1) {
                            if (s.starts[n] == 1) {
                                pos = n;
                                break;
                            }
                        }
                        vector<unsigned> destvregs;
                        // already sorted by addr
                        for (MemLoc &memop : s.ops) {
                            // lbbo dest, base, offset, #
                            // => lbbo {d0, d1, ..}, base, offset, #
                            const MachineOperand &dest = memop.i->getOperand(0);
                            batch.addOperand(dest); // CHECK: op flags
                            assert(dest.isReg());
                            unsigned destreg = dest.getReg();
                            destvregs.push_back(destreg);

                            unsigned preg = reg_at_pos(pos, memop.size);
                            dbgs() << "reassigning " << dest << ": "
                                   << tri->getName(vmap->getPhys(destreg))
                                   << " => " << tri->getName(preg) << "\n";
                            vmap->clearVirt(destreg);
                            vmap->assignVirt2Phys(destreg, preg);

                            memop.i->removeFromParent();
                            pos += memop.size;
                        }
                        li->repairIntervalsInRange(&b, b.begin(), b.end(),
                                                   destvregs);
                    }
                }
            }
        }
        return false;
    }

    void getAnalysisUsage(AnalysisUsage &a) const override {
        a.addRequired<LiveIntervals>();
        a.addRequired<LiveRegMatrix>();
        a.addRequired<VirtRegMap>();
        a.setPreservesAll();
        return MachineFunctionPass::getAnalysisUsage(a);
    }
};

char LoadMerger::id = 0;

FunctionPass *new_load_merger() { return new LoadMerger(); }
}
