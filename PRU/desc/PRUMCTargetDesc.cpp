//===-- PRUMCTargetDesc.cpp - PRU Target Descriptions ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides PRU specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "instprinter.h"
#include "targetdesc.h"
#include "llvm/MC/MCAsmInfoELF.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

#define GET_INSTRINFO_MC_DESC
#include "instrinfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "subtargetinfo.inc"

#define GET_REGINFO_MC_DESC
#include "registerinfo.inc"

namespace llvm {
class Triple;
class PRUMCAsmInfo : public MCAsmInfoELF {
  public:
    explicit PRUMCAsmInfo(const Triple &TT) {
        PointerSize = CalleeSaveStackSlotSize = 4;

        // UsesELFSectionDirectiveForBSS = true;

        GlobalDirective = "";
        HasFunctionAlignment = false;
        HasSingleParameterDotFile = false;
        HasDotTypeDotSizeDirective = false;
        HasIdentDirective = false;
        CommentString = ";";
    }

    bool shouldOmitSectionDirective(StringRef) const override { return true; }
};
}

static MCInstrInfo *createPRUMCInstrInfo() {
    MCInstrInfo *X = new MCInstrInfo();
    InitPRUMCInstrInfo(X);
    return X;
}

static MCRegisterInfo *createPRUMCRegisterInfo(const Triple &TT) {
    MCRegisterInfo *X = new MCRegisterInfo();
    InitPRUMCRegisterInfo(X, PRU::r3_w2);
    return X;
}

static MCSubtargetInfo *createPRUMCSubtargetInfo(const Triple &TT,
                                                 StringRef CPU, StringRef FS) {
    return createPRUMCSubtargetInfoImpl(TT, CPU, FS);
}

static MCCodeGenInfo *createPRUMCCodeGenInfo(const Triple &TT, Reloc::Model RM,
                                             CodeModel::Model CM,
                                             CodeGenOpt::Level OL) {
    MCCodeGenInfo *X = new MCCodeGenInfo();
    X->initMCCodeGenInfo(RM, CM, OL);
    return X;
}

static MCInstPrinter *createPRUMCInstPrinter(const Triple &T,
                                             unsigned SyntaxVariant,
                                             const MCAsmInfo &MAI,
                                             const MCInstrInfo &MII,
                                             const MCRegisterInfo &MRI) {
    if (SyntaxVariant == 0)
        return new PRUInstPrinter(MAI, MII, MRI);
    return nullptr;
}

extern "C" void LLVMInitializePRUTargetMC() {
    RegisterMCAsmInfo<PRUMCAsmInfo> _(pru::target);
    TargetRegistry::RegisterMCCodeGenInfo(pru::target, createPRUMCCodeGenInfo);
    TargetRegistry::RegisterMCInstrInfo(pru::target, createPRUMCInstrInfo);
    TargetRegistry::RegisterMCRegInfo(pru::target, createPRUMCRegisterInfo);
    TargetRegistry::RegisterMCSubtargetInfo(pru::target,
                                            createPRUMCSubtargetInfo);
    TargetRegistry::RegisterMCInstPrinter(pru::target, createPRUMCInstPrinter);
}
