{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Decls where

import Data.Char (toUpper)
import GHC.TypeLits (KnownNat)
import TH
import Data.Proxy (Proxy(Proxy))
import SDNode (SDKind(SDPtr, SDI, SDUnit), comma_join, mach_type_show)
import MachNode (MachKind(I), IAttr(..), MachNode(MachInstr), MachWrap(M_))

{-
--add r.r.op255
--sub
--rsb
--and
--or
--xor
--min
--max
--nop
--lsl r.r.op31
--lsr
--not r.r
mov
ldi r.i65535
ldi32 r.i4294967295
--lbbo r.r.op124
--sbbo
lbco r.c.op124
sbco
xin i253.r.i124
xout
sxin
sxout
sxchg
jmp jt
--qbgt jt.r.op255
--qbge
--qblt
--qble
--qbeq
--qbne
--qba
--qbbs jt.r.op31
--qbbc
wbs
wbc
hlt -
slp
loop jt.r
iloop
selectcc op.op.op.op.cc
-}
-- generate r-r-r forms
fmap concat . mapM (\key ->
    let llprefix = hsname ++ "_"
        hsname = "pru_" ++ key
        opcode = map toUpper key
     in pderive hsname llprefix reg [reg, reg] [] $ mnemonic3 opcode) $
    words "add sub rsb lsl lsr and or xor min max nop"

-- r-r-i forms generated separately as they're rematerializable
fmap concat . mapM (\key ->
    let attrs = [IsRematerializable True]
        llprefix = "pru_" ++ key ++ "_"
        hsname = llprefix ++ "_imm"
        opcode = map toUpper key
     in pderive hsname llprefix reg [reg, imm] attrs $ mnemonic3 opcode) $
    words "add sub rsb lsl lsr and or xor min max nop"

let mnem = [e| \src dest -> "NOT " ++ comma_join [opshow dest, opshow src] |]
 in pderive "pru_not" "pru_not" reg [reg] [] mnem

-- branches
fmap concat . mapM (\key ->
    let hsname = "pru_qb" ++ key
        llprefix = hsname ++ "_"
        opcode = map toUpper key
     in pderive_ hsname llprefix [bb, reg, regimm] [] $ mnemonic4_ opcode) $
    words "gt ge lt le eq ne a bs bc"

-- loads and stores. polymorphic across value widths.
pru_lbbo :: forall k n. KnownNat n => MachNode k SDPtr
         -> MachNode (I '[SDPtr]) (SDI n)
pru_lbbo addr = MachInstr [] ("pru_lbbo_" ++ destty) asm [M_ addr]
    where
    asm s d = "LBBO " ++ comma_join [opshow d, opshow s]
    destty = mach_type_show (Proxy :: Proxy (SDI n))

pru_sbbo :: forall j k n. KnownNat n => MachNode j (SDI n) -> MachNode k SDPtr
         -> MachNode (I '[SDI n, SDPtr]) SDUnit
pru_sbbo val addr = MachInstr [] ("pru_sbbo_" ++ valty) asm [M_ val, M_ addr]
    where
    asm val addr _ = "SBBO " ++ comma_join [opshow val, opshow addr]
    valty = mach_type_show (Proxy :: Proxy (SDI n))

