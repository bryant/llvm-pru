#pragma once

#include "llvm/Support/TargetRegistry.h"

namespace pru {
extern llvm::Target target;
}

// Defines symbolic names for PRU registers.
// This defines a mapping from register name to register number.
#define GET_REGINFO_ENUM
#include "registerinfo.inc"

// Defines symbolic names for the PRU instructions.
#define GET_INSTRINFO_ENUM
#include "instrinfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "subtargetinfo.inc"
