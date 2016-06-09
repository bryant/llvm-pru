{-# LANGUAGE DeriveLift #-}  -- 8.0.1
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TH where

import Data.Char (toUpper)
import Data.List (intercalate, nub)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat)
import GHC.Exts (Constraint)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import SDNode
import MachNode hiding (L)
import qualified MachNode

type family TypeShows tys :: Constraint where
    TypeShows '[] = ()
    TypeShows (t ': ts) = (TypeShow t, TypeShows ts)

deriving instance Lift IAttr

-- TypeShow constraint to ward off accidental printing of SDUnit dest kinds
opshow :: TypeShow a => MachNode MachNode.L a -> String
opshow (MachLeaf (SDPatLeaf _ opid)) = "$op" ++ show opid

-- ScopedTypeVariables requires explicit forall.
instr2 :: forall l r lty rty dty.
       (TypeShows '[lty, rty, dty], C '[lty, rty] dty)
       => String -> String -> [IAttr] -> MachNode l lty -> MachNode r rty
       -> MachNode (I '[lty, rty]) dty
instr2 opcode prefix attrs lhs rhs = MachInstr attrs llname asm [M_ lhs, M_ rhs]
    where
    llname = intercalate "_" [prefix, destty, lhsty, rhsty]
    asm l r d = opcode ++ " " ++ comma_join [opshow d, opshow l, opshow r]
    destty = mach_type_show (Proxy :: Proxy dty)
    lhsty = mach_type_show (Proxy :: Proxy lty)
    rhsty = mach_type_show (Proxy :: Proxy rty)

instr3u :: forall l m r lty mty rty.
        (TypeShows '[lty, mty, rty], C '[lty, mty, rty] SDUnit)
        => String -> String -> [IAttr] -> MachNode l lty -> MachNode m mty
        -> MachNode r rty -> MachNode (I '[lty, mty, rty]) SDUnit
instr3u opcode prefix attrs lhs mid rhs =
        MachInstr attrs llname asm [M_ lhs, M_ mid, M_ rhs]
    where
    llname = intercalate "_" [prefix, lhsty, midty, rhsty]
    asm l m r d = opcode ++ " " ++ comma_join [opshow l, opshow m, opshow r]
    lhsty = mach_type_show (Proxy :: Proxy lty)
    rhsty = mach_type_show (Proxy :: Proxy rty)
    midty = mach_type_show (Proxy :: Proxy mty)

data THArg = THArg { tyvar :: Type, constraints :: [Type] }
    deriving Show

new_type_var = VarT <$> newName "t"

reg :: Q THArg
reg = do
    t <- new_type_var
    THArg <$> [t| SDI $(return t) |] <*> fmap (: []) [t| KnownNat $(return t) |]

imm :: Q THArg
imm = do
    t <- new_type_var
    THArg <$> [t| Imm $(return t) |] <*> fmap (: []) [t| KnownNat $(return t) |]

rn :: Q THArg
rn = THArg <$> [t| I32 |] <*> pure []

ptr :: Q THArg
ptr = THArg <$> [t| SDPtr |] <*> pure []

bb :: Q THArg
bb = THArg <$> [t| BasicBlock |] <*> pure []

regimm :: Q THArg
regimm = do
    t <- new_type_var
    cx <- [t| SDIsInt $(return t) |]
    THArg <$> [t| $(return t) |] <*> pure [cx]

find_ty_vars :: Type -> [Name]
find_ty_vars (AppT t u) = find_ty_vars t `mappend` find_ty_vars u
find_ty_vars (SigT t _) = find_ty_vars t
find_ty_vars (VarT n) = [n]
find_ty_vars (InfixT l _ r) = find_ty_vars l `mappend` find_ty_vars r
find_ty_vars _ = mempty

to_mach th = [t| MachNode $(new_type_var) $(return $ tyvar th) |]

mksig :: THArg -> [THArg] -> Q Type
mksig out args = do
    let list = foldr (\x y -> AppT (AppT PromotedConsT x) y) PromotedNilT $ map tyvar args
    let out' = [t| MachNode (I $(return list)) $(return $ tyvar out) |]
    fnty <- foldr arrow out' $ map to_mach args
    let frees = map PlainTV . nub $ find_ty_vars fnty
    return $ ForallT frees (concatMap constraints $ out : args) fnty

arrow u v = appT (appT arrowT u) v

new_name = newName "p"

pderive :: String -> String -> Q THArg -> [Q THArg] -> [IAttr] -> Q Exp
        -> Q [Dec]
pderive name llpref out args attrs asmp = do
    oty <- out
    argty <- sequence args
    varz <- mapM (const new_name) args
    let llname = [e| llpref ++ intercalate "_" $(listE . map suffix $ oty : argty) |]
    let nodes = listE $ map (appE [e| M_ |] . varE) varz
    sequence [ sigD hsname $ mksig oty argty
             , funD hsname [clause (map varP varz)
                                   (body attrs llname asmp nodes) []]]
    where hsname = mkName name

suffix = return . AppE (VarE 'mach_type_show) . SigE (ConE 'Proxy)
                . AppT (ConT ''Proxy) . tyvar

body attrs llname asmp nodes =
    normalB $ [e| MachInstr attrs |] `appE` llname `appE` asmp `appE` nodes

pderive_ :: String -> String -> [Q THArg] -> [IAttr] -> Q Exp -> Q [Dec]
pderive_ name llpref args attrs asmp = do
    argty <- sequence args
    varz <- mapM (const new_name) args
    let llname = [e| llpref ++ intercalate "_" $(listE $ map suffix argty) |]
        nodes = listE $ map (appE [e| M_ |] . varE) varz
    sequence [ sigD hsname $ mksig unit argty
             , funD hsname [clause (map varP varz)
                                   (body attrs llname asmp nodes) []]]
    where
    hsname = mkName name
    unit = THArg (ConT 'SDUnit) []

mnemonic3 opcode =
    [e| \l r d -> unwords [opcode, comma_join [opshow d, opshow l, opshow r]] |]

mnemonic4_ opcode =
    [e| \a b c _ ->
        unwords [opcode, comma_join [opshow a, opshow b, opshow c]]
    |]
