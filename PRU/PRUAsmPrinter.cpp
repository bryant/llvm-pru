// note: LLVMBuild specifically searches for ${TARGET}AsmPrinter.cpp in
// lib/Target/PRU

#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Mangler.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"

#include "instprinter.h"
#include "instrinfo.h"
#include "targetmachine.h"

using namespace llvm;

#define DEBUG_TYPE "asm-printer"

class MachineModuleInfoMachO;

class PRUMCInstLower {
    MCContext &Ctx;

    AsmPrinter &Printer;

  public:
    PRUMCInstLower(MCContext &ctx, AsmPrinter &printer)
        : Ctx(ctx), Printer(printer) {}

    void Lower(const MachineInstr *MI, MCInst &OutMI) const {
        OutMI.setOpcode(MI->getOpcode());

        for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
            const MachineOperand &MO = MI->getOperand(i);

            MCOperand MCOp;
            switch (MO.getType()) {
            default:
                MI->dump();
                llvm_unreachable("unknown operand type");
            case MachineOperand::MO_Register:
                // Ignore all implicit register operands.
                if (MO.isImplicit())
                    continue;
                MCOp = MCOperand::createReg(MO.getReg());
                break;
            case MachineOperand::MO_Immediate:
                MCOp = MCOperand::createImm(MO.getImm());
                break;
            case MachineOperand::MO_MachineBasicBlock:
                MCOp = MCOperand::createExpr(
                    MCSymbolRefExpr::create(MO.getMBB()->getSymbol(), Ctx));
                break;
            case MachineOperand::MO_GlobalAddress:
                MCOp = LowerSymbolOperand(MO, GetGlobalAddressSymbol(MO));
                break;
            case MachineOperand::MO_ExternalSymbol:
                MCOp = LowerSymbolOperand(MO, GetExternalSymbolSymbol(MO));
                break;
            case MachineOperand::MO_JumpTableIndex:
                MCOp = LowerSymbolOperand(MO, GetJumpTableSymbol(MO));
                break;
            case MachineOperand::MO_ConstantPoolIndex:
                MCOp = LowerSymbolOperand(MO, GetConstantPoolIndexSymbol(MO));
                break;
            case MachineOperand::MO_BlockAddress:
                MCOp = LowerSymbolOperand(MO, GetBlockAddressSymbol(MO));
                break;
            case MachineOperand::MO_RegisterMask:
                continue;
            }

            OutMI.addOperand(MCOp);
        }
    }

    MCOperand LowerSymbolOperand(const MachineOperand &MO,
                                 MCSymbol *Sym) const {
        // FIXME: We would like an efficient form for this, so we don't have to
        // do a
        // lot of extra uniquing.
        const MCExpr *Expr = MCSymbolRefExpr::create(Sym, Ctx);

        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        if (!MO.isJTI() && MO.getOffset())
            Expr = MCBinaryExpr::createAdd(
                Expr, MCConstantExpr::create(MO.getOffset(), Ctx), Ctx);
        return MCOperand::createExpr(Expr);
    }

    MCSymbol *GetGlobalAddressSymbol(const MachineOperand &MO) const {
        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        return Printer.getSymbol(MO.getGlobal());
    }

    MCSymbol *GetExternalSymbolSymbol(const MachineOperand &MO) const {
        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        return Printer.GetExternalSymbolSymbol(MO.getSymbolName());
    }

    MCSymbol *GetJumpTableSymbol(const MachineOperand &MO) const {
        const DataLayout DL = Printer.getDataLayout();
        SmallString<256> Name;
        raw_svector_ostream(Name) << DL.getPrivateGlobalPrefix() << "JTI"
                                  << Printer.getFunctionNumber() << '_'
                                  << MO.getIndex();

        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        // Create a symbol for the name.
        return Ctx.getOrCreateSymbol(Name);
    }

    MCSymbol *GetConstantPoolIndexSymbol(const MachineOperand &MO) const {
        const DataLayout DL = Printer.getDataLayout();
        SmallString<256> Name;
        raw_svector_ostream(Name) << DL.getPrivateGlobalPrefix() << "CPI"
                                  << Printer.getFunctionNumber() << '_'
                                  << MO.getIndex();

        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        // Create a symbol for the name.
        return Ctx.getOrCreateSymbol(Name);
    }

    MCSymbol *GetBlockAddressSymbol(const MachineOperand &MO) const {
        switch (MO.getTargetFlags()) {
        default:
            llvm_unreachable("Unknown target flag on GV operand");
        case 0:
            break;
        }

        return Printer.GetBlockAddressSymbol(MO.getBlockAddress());
    }
};

namespace {
class PRUAsmPrinter : public AsmPrinter {
  public:
    PRUAsmPrinter(TargetMachine &TM, std::unique_ptr<MCStreamer> Streamer)
        : AsmPrinter(TM, std::move(Streamer)) {}

    const char *getPassName() const override { return "PRU Assembly Printer"; }

    void printOperand(const MachineInstr *MI, int OpNum, raw_ostream &O,
                      const char *Modifier = nullptr);
    void printSrcMemOperand(const MachineInstr *MI, int OpNum, raw_ostream &O);
    bool PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                         unsigned AsmVariant, const char *ExtraCode,
                         raw_ostream &O) override;
    bool PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                               unsigned AsmVariant, const char *ExtraCode,
                               raw_ostream &O) override;
    void EmitInstruction(const MachineInstr *MI) override;

    void EmitFunctionHeader() override {
        // TODO: this is a mega hack. check if we missed something
        EmitConstantPool();
        EmitFunctionEntryLabel();
    }
};
} // end of anonymous namespace

void PRUAsmPrinter::printOperand(const MachineInstr *MI, int OpNum,
                                 raw_ostream &O, const char *Modifier) {
    const MachineOperand &MO = MI->getOperand(OpNum);
    switch (MO.getType()) {
    default:
        llvm_unreachable("Not implemented yet!");
    case MachineOperand::MO_Register:
        O << PRUInstPrinter::getRegisterName(MO.getReg());
        return;
    case MachineOperand::MO_Immediate:
        if (!Modifier || strcmp(Modifier, "nohash"))
            O << '#';
        O << MO.getImm();
        return;
    case MachineOperand::MO_MachineBasicBlock:
        MO.getMBB()->getSymbol()->print(O, MAI);
        return;
    case MachineOperand::MO_GlobalAddress: {
        bool isMemOp = Modifier && !strcmp(Modifier, "mem");
        uint64_t Offset = MO.getOffset();

        // If the global address expression is a part of displacement field
        // with
        // a
        // register base, we should not emit any prefix symbol here, e.g.
        //   mov.w &foo, r1
        // vs
        //   mov.w glb(r1), r2
        // Otherwise (!) pru-as will silently miscompile the output :(
        if (!Modifier || strcmp(Modifier, "nohash"))
            O << (isMemOp ? '&' : '#');
        if (Offset)
            O << '(' << Offset << '+';

        getSymbol(MO.getGlobal())->print(O, MAI);

        if (Offset)
            O << ')';

        return;
    }
    }
}

void PRUAsmPrinter::printSrcMemOperand(const MachineInstr *MI, int OpNum,
                                       raw_ostream &O) {
    const MachineOperand &Base = MI->getOperand(OpNum);
    const MachineOperand &Disp = MI->getOperand(OpNum + 1);

    // Print displacement first

    // Imm here is in fact global address - print extra modifier.
    if (Disp.isImm() && !Base.getReg())
        O << '&';
    printOperand(MI, OpNum + 1, O, "nohash");

    // Print register base field
    if (Base.getReg()) {
        O << '(';
        printOperand(MI, OpNum, O);
        O << ')';
    }
}

/// PrintAsmOperand - Print out an operand for an inline asm expression.
///
bool PRUAsmPrinter::PrintAsmOperand(const MachineInstr *MI, unsigned OpNo,
                                    unsigned AsmVariant, const char *ExtraCode,
                                    raw_ostream &O) {
    // Does this asm operand have a single letter operand modifier?
    if (ExtraCode && ExtraCode[0])
        return true; // Unknown modifier.

    printOperand(MI, OpNo, O);
    return false;
}

bool PRUAsmPrinter::PrintAsmMemoryOperand(const MachineInstr *MI, unsigned OpNo,
                                          unsigned AsmVariant,
                                          const char *ExtraCode,
                                          raw_ostream &O) {
    if (ExtraCode && ExtraCode[0]) {
        return true; // Unknown modifier.
    }
    printSrcMemOperand(MI, OpNo, O);
    return false;
}

//===----------------------------------------------------------------------===//
void PRUAsmPrinter::EmitInstruction(const MachineInstr *MI) {
    if ((PRUInstrInfo::is_load_multiple(MI->getOpcode()) ||
         PRUInstrInfo::is_store_multiple(MI->getOpcode())) &&
        MI->getNumOperands() > 4) {

        raw_ostream &c = OutStreamer->GetCommentOS();

        c << "batched:";
        for (unsigned opnum = 3; opnum < MI->getNumOperands(); opnum += 1) {
            c << " " << PRUInstPrinter::getRegisterName(
                            MI->getOperand(opnum).getReg());
        }
        OutStreamer->AddComment("");
    }

    PRUMCInstLower MCInstLowering(OutContext, *this);

    MCInst TmpInst;
    MCInstLowering.Lower(MI, TmpInst);
    EmitToStreamer(*OutStreamer, TmpInst);
}

namespace pru {
extern Target target;
}

// Force static initialization.
extern "C" void LLVMInitializePRUAsmPrinter() {
    RegisterAsmPrinter<PRUAsmPrinter> _(pru::target);
}
