#include "instprinter.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;

#define DEBUG_TYPE "asm-printer"

// Include the auto-generated portion of the assembly writer.
#include "asmwriter.inc"

void PRUInstPrinter::printInst(const MCInst *MI, raw_ostream &O,
                               StringRef Annot, const MCSubtargetInfo &STI) {
    printInstruction(MI, O);
    printAnnotation(O, Annot);
}

void PRUInstPrinter::print_addr(const MCInst *inst, unsigned opnum,
                                raw_ostream &out) {
    auto base_reg = inst->getOperand(opnum);
    auto offset = inst->getOperand(opnum + 1);

    out << getRegisterName(base_reg.getReg()) << ", ";
    if (offset.isImm()) {
        out << formatImm(offset.getImm());
    } else if (offset.isReg()) {
        out << getRegisterName(offset.getReg());
    } else {
        llvm_unreachable("offset operand was neither imm nor reg!");
    }
}

void PRUInstPrinter::printPCRelImmOperand(const MCInst *MI, unsigned OpNo,
                                          raw_ostream &O) {
    const MCOperand &Op = MI->getOperand(OpNo);
    if (Op.isImm())
        O << Op.getImm();
    else {
        assert(Op.isExpr() && "unknown pcrel immediate operand");
        Op.getExpr()->print(O, &MAI);
    }
}

void PRUInstPrinter::printOperand(const MCInst *MI, unsigned OpNo,
                                  raw_ostream &O, const char *Modifier) {
    assert((Modifier == nullptr || Modifier[0] == 0) &&
           "No modifiers supported");
    const MCOperand &Op = MI->getOperand(OpNo);
    if (Op.isReg()) {
        O << getRegisterName(Op.getReg());
    } else if (Op.isImm()) {
        // TODO: fix handling of negative immediates
        uint64_t val = static_cast<uint64_t>(Op.getImm()) & 0xffffffff;
        O << formatHex(val);
    } else {
        assert(Op.isExpr() && "unknown operand kind in printOperand");
        // O << '#';
        Op.getExpr()->print(O, &MAI);
    }
}

void PRUInstPrinter::printSrcMemOperand(const MCInst *MI, unsigned OpNo,
                                        raw_ostream &O, const char *Modifier) {
    const MCOperand &Base = MI->getOperand(OpNo);
    const MCOperand &Disp = MI->getOperand(OpNo + 1);

    // Print displacement first

    // If the global address expression is a part of displacement field with a
    // register base, we should not emit any prefix symbol here, e.g.
    //   mov.w &foo, r1
    // vs
    //   mov.w glb(r1), r2
    // Otherwise (!) pru-as will silently miscompile the output :(
    if (!Base.getReg())
        O << '&';

    if (Disp.isExpr())
        Disp.getExpr()->print(O, &MAI);
    else {
        assert(Disp.isImm() && "Expected immediate in displacement field");
        O << Disp.getImm();
    }

    // Print register base field
    if (Base.getReg())
        O << '(' << getRegisterName(Base.getReg()) << ')';
}
