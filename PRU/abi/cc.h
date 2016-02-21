#pragma once

#include "llvm/CodeGen/CallingConvLower.h"

using namespace llvm;
using std::vector;
using SDVals = vector<SDValue>;

struct CC {

    // chain used to build CopyFromReg and getLoad nodes which are returned.
    virtual SDVals lower_formal_args(SDValue &ch, SDLoc dl,
                                     ArrayRef<ISD::InputArg> args) = 0;

    // returns an array of operands for the ret node
    virtual SDVals lower_return(SDValue &ch, SDValue &glue, SDLoc dl,
                                ArrayRef<ISD::OutputArg> args,
                                const ArrayRef<SDValue> &vals) = 0;

    // returns an array of operands for call node
    virtual SDVals lower_call(SDValue &ch, SDValue &glue, SDLoc dl,
                              ArrayRef<ISD::OutputArg> args,
                              const ArrayRef<SDValue> &vals) = 0;

    // like formal args, except one arg = retval.
    virtual SDVals lower_call_result(SDValue &ch, SDValue &glue, SDLoc dl,
                                     ArrayRef<ISD::InputArg> args) = 0;

    virtual unsigned compute_call_stack(ArrayRef<ISD::OutputArg> arg) = 0;
};
