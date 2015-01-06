include "llvm/Target/Target.td"

class Proc<string Name, list<SubtargetFeature> Features> :
    Processor<Name, NoItineraries, Features>;

def : Proc<"generic", []>;

include "tablegen/registers.td"
include "tablegen/optypes.td"

let Namespace = "PRU" in {
include "generated/instructions.td"
include "tablegen/miscinstrs.td"
}

include "generated/patterns.td"
include "tablegen/miscpatterns.td"

include "tablegen/callingconv.td"

def PRUInstrInfo : InstrInfo;

def PRU : Target {
  let InstructionSet = PRUInstrInfo;
}

