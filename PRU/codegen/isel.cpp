#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetLowering.h"

#include "targetdesc.h"
#include "targetlowering.h"

#define DEBUG_TYPE "pru-isel"

namespace pru {
using namespace llvm;

struct PruISel : public SelectionDAGISel {
    PruISel(TargetMachine &TM, CodeGenOpt::Level OptLevel)
        : SelectionDAGISel(TM, OptLevel) {}

    const char *getPassName() const override {
        return "PRU Instruction Selection";
    }

// contains SDNode SelectCode(SDNode *)
#include "dagisel.inc"

    SDNode *Select(SDNode *op) override {
        // Select the default instruction
        DEBUG(dbgs() << "Selecting: ");
        DEBUG(op->dump(CurDAG));
        DEBUG(dbgs() << "\n");

        SDNode *rv;

        switch (op->getOpcode()) {
        default:
            rv = SelectCode(op);
            break;

        case ISD::FrameIndex:
            rv = CurDAG->SelectNodeTo(
                op, PRU::pru_add_reg32_reg32_i32imm, MVT::i32,
                CurDAG->getTargetFrameIndex(
                    cast<FrameIndexSDNode>(op)->getIndex(), MVT::i32),
                CurDAG->getTargetConstant(0, SDLoc(op), MVT::i32));
            break;
        }

        DEBUG(errs() << "=> ");
        if (rv == nullptr || rv == op)
            DEBUG(op->dump(CurDAG));
        else
            DEBUG(rv->dump(CurDAG));
        DEBUG(errs() << "\n");

        return rv;
    }

    bool SelectAddress(SDValue addr, SDValue &base, SDValue &offset) {
        auto ptr_ty = TLI->getPointerTy(CurDAG->getDataLayout());

        DEBUG(dbgs() << "[pru] addrnode is: ");
        DEBUG(addr.dump());
        DEBUG(dbgs() << "[pru] " << addr.getNumOperands() << " operands!\n");

        // frame index
        if (FrameIndexSDNode *fi = dyn_cast<FrameIndexSDNode>(addr)) {
            base = CurDAG->getTargetFrameIndex(fi->getIndex(), ptr_ty);
            offset = CurDAG->getTargetConstant(0, SDLoc(addr), ptr_ty);

            return true;
        }

        // reg `binop` imm form
        else if (isa<ConstantSDNode>(addr.getOperand(1)) &&
                 (addr.getOpcode() == ISD::ADD ||
                  addr.getOpcode() == ISD::SUB ||
                  CurDAG->isBaseWithConstantOffset(addr))) {
            ConstantSDNode *op1 = dyn_cast<ConstantSDNode>(addr.getOperand(1));
            int noffset = addr.getOpcode() == ISD::SUB ? -op1->getSExtValue()
                                                       : op1->getSExtValue();
            DEBUG(dbgs() << "SelectAddress: noffset = " << noffset << "\n");

            if (0 <= noffset && noffset < 256) {
                offset =
                    CurDAG->getTargetConstant(noffset, SDLoc(addr), ptr_ty);
                if (FrameIndexSDNode *fi =
                        dyn_cast<FrameIndexSDNode>(addr.getOperand(0))) {
                    const auto &mf = *MF->getFrameInfo();
                    int real = mf.getStackSize() +
                               mf.getObjectOffset(fi->getIndex()) + noffset;
                    if (0 <= real && real < 256) {
                        base =
                            CurDAG->getTargetFrameIndex(fi->getIndex(), ptr_ty);
                    } else {
                        base = addr.getOperand(0);
                        // non-target node further iseled
                    }
                } else {
                    base = addr.getOperand(0);
                }
                return true;
            }
        }

        return false;
    }

    bool SelectAddressReg(SDValue addr, SDValue &base, SDValue &offset) {
        auto ptrvt = TLI->getPointerTy(CurDAG->getDataLayout());
        DEBUG(dbgs() << "SelectAddressReg chosen");
        if (addr.getOpcode() == ISD::ADD || addr.getOpcode() == ISD::SUB) {
            if (FrameIndexSDNode *fi =
                    dyn_cast<FrameIndexSDNode>(addr.getOperand(0))) {
                base = CurDAG->getTargetFrameIndex(fi->getIndex(), ptrvt);
            } else {
                base = addr.getOperand(0);
            }
            offset = addr.getOperand(1);
            return true;
        } else {
            base = addr;
            offset = CurDAG->getTargetConstant(0, SDLoc(addr), ptrvt);
            return true;
        }

        return false;
    }
};

FunctionPass *new_pru_isel(TargetMachine &tm, CodeGenOpt::Level opt) {
    return new PruISel(tm, opt);
}
}
