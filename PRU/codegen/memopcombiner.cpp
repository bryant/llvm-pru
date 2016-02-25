#include <algorithm>
#include <functional>
#include <utility>
#include <vector>

#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineMemOperand.h"
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

template <typename T> bool intervals_intersect(T s0, T e0, T s1, T e1) {
    bool rv = !(s0 > e1 || e0 < s1);
    return rv;
}

struct MemLoc {
    enum Kind { BaseOffset, FrameSlot, AlwaysAliases };
    Kind kind;
    MachineInstr *i;

    struct _B {
        BaseReg base;
        Offset start;
        Offset end;
    };

    struct _S {
        Offset start;
        Offset end;
    };

    union {
        _B b; // baseoffset
        _S s; // frameslot
    };

  private:
    MemLoc(MachineInstr &ii, Kind k) : kind(k), i(&ii) {}

  public:
    static MemLoc from_base_offset(MachineInstr &ii) {
        auto rv = MemLoc(ii, BaseOffset);
        rv.b.base = ii.getOperand(1).getReg();
        rv.b.start = ii.getOperand(2).getImm();
        rv.b.end = static_cast<Offset>((*ii.memoperands_begin())->getSize()) +
                   rv.b.start - 1;
        return rv;
    }

    static MemLoc from_fi(MachineInstr &ii) {
        auto rv = MemLoc(ii, FrameSlot);
        // TODO: this duplicates logic in PRURegisterInfo::eliminateFrameIndex
        int fi = ii.getOperand(1).getIndex();
        MachineFrameInfo &frinfo = *ii.getParent()->getParent()->getFrameInfo();
        rv.s.start = frinfo.getStackSize() + frinfo.getObjectOffset(fi) +
                     ii.getOperand(2).getImm();
        rv.s.end = static_cast<Offset>((*ii.memoperands_begin())->getSize()) +
                   rv.s.start - 1;
        return rv;
    }

    static MemLoc from_mi(MachineInstr &ii) {
        if (ii.getOperand(1).isReg() && ii.getOperand(2).isImm()) {
            return MemLoc::from_base_offset(ii);
        } else if (ii.getOperand(1).isFI()) {
            return MemLoc::from_fi(ii);
        } else {
            return MemLoc(ii, AlwaysAliases);
        }
    }

    bool could_alias(const MemLoc &other) const {
        if (kind == AlwaysAliases || other.kind == AlwaysAliases) {
            return true;
        }

        if (kind != other.kind) {
            return true;
        }

        if (kind == BaseOffset) {
            if (b.base == other.b.base &&
                !intervals_intersect(b.start, b.end, other.b.start,
                                     other.b.end)) {
                return false;
            }
        } else if (kind == FrameSlot &&
                   !intervals_intersect(s.start, s.end, other.s.start,
                                        other.s.end)) {
            return false;
        }

        return true;
    }

    bool operator<(const MemLoc &other) {
        // assert(kind == other.kind);
        if (kind == MemLoc::BaseOffset) {
            return b.start < other.b.start;
        } else if (kind == MemLoc::FrameSlot) {
            return s.start < other.s.start;
        }
        return true;
    }
};

raw_ostream &operator<<(raw_ostream &o, const MemLoc &l) {
    switch (l.kind) {
    case MemLoc::BaseOffset:
        o << "BaseOffset(" << l.b.base << ", " << l.b.start << ", " << l.b.end
          << ")";
        break;
    case MemLoc::FrameSlot:
        o << "FrameSlot(" << l.s.start << ", " << l.s.end << ")";
        break;
    default:
        o << "AlwaysAliases";
    }
    return o;
}

struct Cluster {
    vector<MemLoc> memops;
    unsigned index;

    bool can_accept(const MemLoc &loc) const {
        return none_of(memops.begin(), memops.end(), [&](MemLoc l) {
            bool rv = loc.could_alias(l);
            return rv;
        });
    }

    void sort() { std::sort(std::next(memops.begin()), memops.end()); }
};

raw_ostream &operator<<(raw_ostream &o, const Cluster &c) {
    o << "cluster anchored at " << c.index << ":\n";
    for (auto i : c.memops) {
        o << "\t" << i << "\n";
    }
    return o;
}

template <typename ClassifyClusterable, typename ClassifyAliasing>
struct Collector {
    vector<Cluster> clusters;
    vector<pair<MemLoc, unsigned>> aliasables;
    unsigned curidx;
    ClassifyClusterable is_clusterable;
    ClassifyAliasing could_alias_clusters;

    Collector(ClassifyClusterable c, ClassifyAliasing a)
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
            auto loc = MemLoc::from_mi(i);
            unsigned mrc;
            if (has_previous_collision(loc, mrc)) {
                add_to_clusters(loc,
                                [mrc](Cluster &c) { return c.index > mrc; });
            } else {
                add_to_clusters(loc, [](Cluster &c) { return true; });
            }
        }
        if (could_alias_clusters(i)) {
            auto loc = MemLoc::from_mi(i);
            aliasables.push_back(std::make_pair(loc, curidx));
        }
        ++curidx;
    }

    void operator()(MachineInstr &i) { return update(i); }

    bool cluster(MachineBasicBlock &blk) {
        bool rv = false;
        DEBUG(dbgs() << clusters.size() << " clusters:\n");
        for (Cluster &c : clusters) {
            if (c.memops.size() > 1) {
                c.sort();
            }
            DEBUG(dbgs() << c << "\n");
            if (c.memops.size() > 1) {
                rv = true;

                auto loc = std::next(c.memops.begin());
                for (; loc != c.memops.end() && *loc < c.memops[0]; ++loc) {
                    blk.splice(c.memops[0].i, &blk, loc->i);
                }

                auto insert =
                    std::next(MachineBasicBlock::instr_iterator(c.memops[0].i));

                for (; loc != c.memops.end(); ++loc) {
                    blk.splice(insert, &blk, loc->i);
                    insert =
                        std::next(MachineBasicBlock::instr_iterator(loc->i));
                }
            }
        }
        return rv;
    }
};

template <typename F, typename G> Collector<F, G> gen_collector(F f, G g) {
    return Collector<F, G>(f, g);
}

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

bool is_load_or_store(MachineInstr &i) {
    if (PRUInstrInfo::is_load(i.getOpcode()) ||
        PRUInstrInfo::is_store(i.getOpcode())) {
        return true;
    }
    return false;
}

struct MemOpClusterer : public MachineFunctionPass {
    static char id;

    MemOpClusterer() : MachineFunctionPass(id) {}

    const char *getPassName() const override {
        return "PRU LBBO/SBBO Clusterer";
    }

    bool runOnMachineFunction(MachineFunction &f) override {
        dbgs() << "MemOpClusterer launching!!!!!!!!!!!!!!!\n";
        bool rv = false;

        for (auto &basicblock : f) {
            dbgs() << "before:\n";
            basicblock.print(dbgs());

            auto c =
                for_each(basicblock.begin(), basicblock.end(),
                         gen_collector(clusterable_load, is_load_or_store));
            rv |= c.cluster(basicblock);

            dbgs() << "result is:\n";
            basicblock.print(dbgs());

            auto s =
                for_each(basicblock.rbegin(), basicblock.rend(),
                         gen_collector(clusterable_store, is_load_or_store));
            rv |= s.cluster(basicblock);
        }

        return rv;
    }
};

char MemOpClusterer::id = 0;

struct LoadMerger : public MachineFunctionPass {
    static char id;

    LoadMerger() : MachineFunctionPass(id) {}

    const char *getPassName() const override { return "PRU LBBO Merger"; }

    bool runOnMachineFunction(MachineFunction &f) override {
        for (const MachineBasicBlock &b : f) {
            b.print(dbgs());
        }
        return false;
    }
};

char LoadMerger::id = 0;

FunctionPass *new_mem_op_clusterer() { return new MemOpClusterer(); }

FunctionPass *new_load_merger() { return new LoadMerger(); }
}
