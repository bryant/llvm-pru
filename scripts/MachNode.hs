{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module MachNode where

-- machine node kind
data MachKind
    = MachInstr
    -- ^ backend instruction
    | TargetOpCode
    -- ^ one of the opcodes from include/llvm/Target/TargetOpcodes.h

data MachNode (mk :: MachKind) where
    Instruction :: String -> String -> [IAttr] -> String
                -> MachNode MachInstr
    LLMachNode :: String -> MachNode TargetOpCode

-- instruction attributes
data IAttr
    = IsReturn Bool | IsBranch Bool | IsTerminator Bool | HasSideEffects Bool
    | IsPseudo Bool | IsMoveImm Bool | Uses [String] | Defs [String]
    | UsesCustomInserter Bool | IsBarrier Bool | IsRematerializable Bool

llvm_name, asm_format :: MachNode MachInstr -> String
llvm_name (Instruction rv _ _ _) = rv
asm_format (Instruction _ rv _ _) = rv

attributes :: MachNode MachInstr -> [IAttr]
attributes (Instruction _ _ rv _) = rv
