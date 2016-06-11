module Patterns where

import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.Class (lift)
import SDNode
import MachNode
import Decls
import Prelude hiding (add, sub, and, or, unlines)

data Pattern = Pattern SDNode MachNode | NullPattern Instruction

instance Show Pattern where
    show (Pattern sd mach) = unlines [
          "def : Pat<"
        , indent 4 (parens $ show sd) ++ ","
        , indent 4 . parens $ show mach
        , ">;"
        ]
    show (NullPattern _) = ""

used_defs :: Pattern -> [Instruction]
used_defs (NullPattern i) = [i]
used_defs (Pattern _ machnode) = search machnode
    where
    search (MachInstr i rest) = i : concatMap search rest
    search (MNode _ rest) = concatMap search rest
    search _ = []

type TableGen = WriterT [Pattern] (State Int)

run_tablegen :: TableGen a -> [Pattern]
run_tablegen = fst . flip State.runState 0 . fmap snd . Writer.runWriterT

next_int :: TableGen Int
next_int = lift $ State.get <* State.modify (+ 1)

next_op opty wrap = SDLeaf opty wrap <$> next_int

next_op_ opty = next_op opty ""

-- operands
i = next_op_ . Reg
[i8, i16, i32] = map i widths

imm width lim = next_op (Imm width) $ "i" ++ show width ++ "imm_" ++ show lim

imm_31 = flip imm 31
[i8imm_31, i16imm_31, i32imm_31] = map imm_31 widths

imm_255 = flip imm 255
[i8imm_255, i16imm_255, i32imm_255] = map imm_255 widths

imm_65535 = flip imm 65535
[i8imm_65535, i16imm_65535, i32imm_65535] = map imm_65535 widths

condcode        = next_op Unknown "condcode"
bb = next_op_ BasicBlock
addr = next_op_ Addr
regaddr = next_op Addr "regaddr"
tglobaladdr = next_op Addr "tglobaladdr"
texternalsym = next_op Addr "texternalsym"

widths :: [Int]
widths = [8, 16, 32]

enumerator :: (Int -> Bool) -> (Int -> TableGen a) -> [Pattern]
enumerator cond f = run_tablegen $ sequence_ [f a | a <- widths, cond a]

enumerator2 :: (Int -> Int -> Bool) -> (Int -> Int -> TableGen a) -> [Pattern]
enumerator2 cond f = run_tablegen $ sequence_ pats
    where pats = [f a b | a <- widths, b <- widths, cond a b]

enumerator2_ = enumerator2 $ \_ _ -> True

enumerator3 :: (Int -> Int -> Int -> Bool) -> (Int -> Int -> Int -> TableGen a)
            -> [Pattern]
enumerator3 cond f = run_tablegen $ sequence_ pats
    where pats = [f a b c | a <- widths, b <- widths, c <- widths, cond a b c]

enumerator3_ = enumerator3 $ \_ _ _ -> True

(->>) :: SDNode -> MachNode -> TableGen ()
sd ->> mach = Writer.tell [Pattern sd mach]

maybe_zext_to targetwidth op@(SDLeaf (Reg n) _ _)
    | n < targetwidth = zext targetwidth op
maybe_zext_to targetwidth op@(SDLeaf (Imm n) _ _)
    | n < targetwidth = zext targetwidth op
maybe_zext_to _ op = op

-- patterns

basic :: [Pattern]
basic = do
    (sd, mach) <- zip [add, sub, and, or, xor, shl, srl]
                      [pru_add, pru_sub, pru_and, pru_or, pru_xor, pru_lsl,
                       pru_lsr]
    enumerator (const True) $ \width -> do
        l <- i width
        r <- i width
        sd l r ->> mach l r

basic_imm :: [Pattern]
basic_imm = do
    (sd, mach) <- zip [add, sub, and, or, xor]
                      [pru_add_imm, pru_sub_imm, pru_and_imm, pru_or_imm,
                       pru_xor_imm]
    enumerator (const True) $ \width -> do
        lhs <- i width
        rhs <- imm_255 width
        sd lhs rhs ->> mach lhs rhs

basic_imm_31 :: [Pattern]
basic_imm_31 = do
    (sd, mach) <- zip [shl, srl] [pru_lsl_imm, pru_lsr_imm]
    enumerator2_ $ \srcwidth shiftwidth -> do
        shift <- imm_31 shiftwidth
        src <- i srcwidth
        sd src shift ->> mach src shift

binop_zext_is_free = do
    (sd, mach, mach_imm) <-
        zip3 [add, sub, and, or, xor, shl, srl]
             [pru_add, pru_sub, pru_and, pru_or, pru_xor, pru_lsl, pru_lsr]
             [pru_add_imm, pru_sub_imm, pru_and_imm, pru_or_imm, pru_xor_imm,
              pru_lsl_imm, pru_lsr_imm]
    enumerator3 (\dest lw rw -> dest >= max lw rw) $ \dest lw rw -> do
        l <- i lw
        r <- i rw
        rimm <- imm_255 rw
        let mb_zext = maybe_zext_to dest
        sd (mb_zext l) (mb_zext r) ->> mach l r
        sd (mb_zext l) (mb_zext rimm) ->> mach_imm l rimm

-- src and shift operands of shift nodes are permitted to be different widths
-- (cf. TargetSelectionDAG.td)
shiftop_zext_is_free = do
    (sd, mach, mach_imm) <- zip3 [shl, srl]
                                 [pru_lsl, pru_lsr]
                                 [pru_lsl_imm, pru_lsr_imm]
    enumerator2 (/=) $ \srcwidth shiftwidth -> do
        src <- i srcwidth
        shift <- i shiftwidth
        shiftimm <- imm_255 shiftwidth

        sd src shift ->> mach src shift
        sd src shiftimm ->> mach_imm src shiftimm

fixed_loads = enumerator (const True) $ \width -> do
    addr <- addr
    coerce width (load addr) ->> pru_lbbo width addr

    raddr <- regaddr
    coerce width (load raddr) ->> pru_lbbo width raddr

fixed_stores = enumerator (const True) $ \width -> do
    val <- i width
    addr <- addr
    store val addr ->> pru_sbbo width val addr

    raddr <- regaddr
    store val raddr ->> pru_sbbo width val raddr

direct_zext = enumerator2 (>) $ \destwidth srcwidth -> do
    src <- i srcwidth
    zext destwidth src ->> pru_mov src

direct_anyext = enumerator2 (>) $ \destwidth srcwidth -> do
    src <- i srcwidth
    anyext destwidth src ->> subreg_to_reg destwidth src (subidx srcwidth 0)

-- truncation = sub-register access. TODO: what if type(srl.0) != type(srl.1)?
trunc_is_free = run_tablegen $ do
    src16 <- i 16
    src32 <- i 32

    trunc 8 src16 ->> extract_subreg src16 (subidx 8 0)
    trunc 8 (srl src16 $ const16 8) ->> extract_subreg src16 (subidx 8 8)

    trunc 8 src32 ->> extract_subreg src32 (subidx 8 0)
    trunc 8 (srl src32 $ const32 8) ->> extract_subreg src32 (subidx 8 8)
    trunc 8 (srl src32 $ const32 16) ->> extract_subreg src32 (subidx 8 16)
    trunc 8 (srl src32 $ const32 24) ->> extract_subreg src32 (subidx 8 24)

    trunc 16 src32 ->> extract_subreg src32 (subidx 16 0)
    trunc 16 (srl src32 $ const32 16) ->> extract_subreg src32 (subidx 16 16)

pairs = run_tablegen $ do
    low16 <- i16
    high16 <- i16
    let low' = insert_subreg (implicit_def 32) low16 (subidx 16 0)
    build_pair low16 high16 ->> insert_subreg low' high16 (subidx 16 16)

    low8 <- i8
    high8 <- i8
    let low' = insert_subreg (implicit_def 16) low8 (subidx 8 0)
    build_pair low8 high8 ->> insert_subreg low' high8 (subidx 8 8)

quick_branch = do
    ((cc, unorderedcc), pru_qb) <- zip condcodes
                                       [pru_qbne, pru_qbeq, pru_qbgt, pru_qbge,
                                        pru_qblt, pru_qble]
    enumerator2_ $ \lwidth rwidth -> do
        l <- i lwidth
        r <- i rwidth
        rimm <- imm_255 rwidth
        destblock <- bb
        let mb_zext = maybe_zext_to (max lwidth rwidth)

        brcc cc (mb_zext l) (mb_zext r) destblock ->> pru_qb destblock l r
        brcc cc (mb_zext l) (mb_zext rimm) destblock ->> pru_qb destblock l rimm
        brcc unorderedcc (mb_zext l) (mb_zext r) destblock ->> pru_qb destblock l r
        brcc unorderedcc (mb_zext l) (mb_zext rimm) destblock ->> pru_qb destblock l rimm

jump = run_tablegen $ do
    dest <- bb
    br dest ->> pru_jmp dest

calls = run_tablegen $ do
    global <- tglobaladdr
    call global ->> pru_call global

    external <- texternalsym
    call external ->> pru_call external

    regsym <- i 32
    call regsym ->> pru_call regsym

selectccs = do
    ((cc, ucc), pru_sel) <- zip condcodes
                                [pru_selectne, pru_selecteq, pru_selectgt,
                                 pru_selectge, pru_selectlt, pru_selectle]
    enumerator3_ $ \lcmpwidth rcmpwidth valwidth -> do
        l <- i lcmpwidth
        r <- i rcmpwidth
        rimm <- imm_255 rcmpwidth
        t <- i valwidth
        f <- i valwidth
        let mb_zext = maybe_zext_to (max lcmpwidth rcmpwidth)

        selectcc (mb_zext l) (mb_zext r) t f cc ->> pru_sel l r t f
        selectcc (mb_zext l) (mb_zext rimm) t f cc ->> pru_sel l rimm t f
        selectcc (mb_zext l) (mb_zext r) t f ucc ->> pru_sel l r t f
        selectcc (mb_zext l) (mb_zext rimm) t f ucc ->> pru_sel l rimm t f

allpats = concat
    [ basic
    , basic_imm
    , basic_imm_31
    , fixed_loads
    , fixed_stores
    , direct_zext
    , direct_anyext
    , trunc_is_free
    , binop_zext_is_free
    , shiftop_zext_is_free
    , quick_branch
    , selectccs
    , jump
    , pairs
    , calls
    ]
