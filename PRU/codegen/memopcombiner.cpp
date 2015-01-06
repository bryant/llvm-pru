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
    enum { BaseOffset, FrameSlot, AlwaysAliases } kind;
    MachineInstr *i;

    struct _B {
        BaseReg base;
        Offset start;
        Offset end;
    };

    union {
        _B b;          // baseoffset
        int slotindex; // frameslot
    };

    MemLoc(MachineInstr &ii, BaseReg breg, Offset s, Offset e)
        : kind(BaseOffset), i(&ii) {
        b.base = breg;
        b.start = s;
        b.end = e;
    }

    MemLoc(MachineInstr &ii, int fi) : kind(FrameSlot), i(&ii) {
        slotindex = fi;
    }

    MemLoc(MachineInstr &ii) : kind(AlwaysAliases), i(&ii) {}

    static MemLoc from_mi(MachineInstr &ii) {
        if (ii.getOperand(1).isReg() && ii.getOperand(2).isImm()) {
            return MemLoc(
                ii, ii.getOperand(1).getReg(), ii.getOperand(2).getImm(),
                static_cast<Offset>((*ii.memoperands_begin())->getSize()) +
                    ii.getOperand(2).getImm() - 1);
        } else if (ii.getOperand(1).isFI()) {
            return MemLoc(ii, ii.getOperand(1).getIndex());
        } else {
            return MemLoc(ii);
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
        } else if (slotindex != other.slotindex) {
            return false;
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
        o << "FrameSlot(" << l.slotindex << ")";
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
        MemLoc loc(i);
        if (is_clusterable(i, loc)) {
            unsigned mrc;
            if (has_previous_collision(loc, mrc)) {
                add_to_clusters(loc,
                                [mrc](Cluster &c) { return c.index > mrc; });
            } else {
                add_to_clusters(loc, [](Cluster &c) { return true; });
            }
        }
        if (could_alias_clusters(i, loc)) {
            aliasables.push_back(std::make_pair(loc, curidx));
        }
        ++curidx;
    }

    void operator()(MachineInstr &i) { return update(i); }

    bool cluster(MachineBasicBlock &blk) {
        bool rv = false;
        DEBUG(dbgs() << clusters.size() << " clusters:\n");
        for (Cluster &c : clusters) {
            DEBUG(dbgs() << c << "\n");
            if (c.memops.size() > 1) {
                rv = true;
                for (MemLoc &loc : c.memops) {
                    blk.splice(c.memops[0].i, &blk, loc.i);
                }
            }
        }
        return rv;
    }
};

template <typename F, typename G> Collector<F, G> gen_collector(F f, G g) {
    return Collector<F, G>(f, g);
}

bool clusterable_load(MachineInstr &i, MemLoc &loc) {
    if (PRUInstrInfo::is_load(i.getOpcode()) && !i.hasOrderedMemoryRef()) {
        loc = MemLoc::from_mi(i);
        return true;
    }
    return false;
}

bool clusterable_store(MachineInstr &i, MemLoc &loc) {
    if (PRUInstrInfo::is_store(i.getOpcode()) && !i.hasOrderedMemoryRef()) {
        loc = MemLoc::from_mi(i);
        return true;
    }
    return false;
};

bool is_load_or_store(MachineInstr &i, MemLoc &loc) {
    if (PRUInstrInfo::is_load(i.getOpcode()) ||
        PRUInstrInfo::is_store(i.getOpcode())) {
        loc = MemLoc::from_mi(i);
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
