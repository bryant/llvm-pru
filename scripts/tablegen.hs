import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (State)
import SDNode

data PatternArg a = PatternArg Int

data MachineNode
    = Instruction { llvm_name :: String
                  , asm_format :: String
                  , attributes :: [IAttr]
                  }
    | EXTRACT_SUBREG | SUBREG_TO_REG | COPY_TO_REGCLASS

data IAttr
    = IsReturn Bool | IsBranch Bool | IsTerminator Bool | HasSideEffects Bool
    | IsPseudo Bool | IsMoveImm Bool | Uses [String] | Defs [String]
    | UsesCustomInserter Bool | IsBarrier Bool | IsRematerializable Bool

type TableGen = State Int

new_sd_op :: TypeShow a => TableGen (SDNode L a)
new_sd_op = SDPatLeaf "" <$> next_int

i32 :: TableGen (SDNode L I32)
i32 = new_sd_op

i16 :: TableGen (SDNode L I16)
i16 = new_sd_op

i8 :: TableGen (SDNode L I8)
i8 = new_sd_op

next_int = State.get >>= \n -> State.modify (+ 1) >> return n
