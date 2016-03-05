type Reg = Int
type Offset = Int
type Frame = Int

data MemLoc = BaseReg Reg Offset Offset | MemLoc Frame Offset Offset | Unknown
    deriving Show

type Cluster = [MemLoc]

can_add :: Cluster -> MemLoc -> Bool
can_add c m = and $ map (m `compat`) c

compat a b = case (a, b) of
    (MemLoc fi0 s0 e0, MemLoc fi1 s1 e1)
        | fi1 /= fi0 -> False
    _ -> not $ a `aliases` b

aliases a b = case (a, b) of
    (BaseReg r0 s0 e0, BaseReg r1 s1 e1)
        | r0 == r1 -> (s0, e0) `overlaps` (s1, e1)
    (MemLoc fi0 s0 e0, MemLoc fi1 s1 e1)
        | fi1 /= fi0 -> False
        | otherwise -> (s0, e0) `overlaps` (s1, e1)
    _ -> True

a@(x, y) `overlaps` b@(u, v)
    | ordered a && ordered b = not $ y < u || x > v
    | otherwise = error $ "spotted an unordered pair " ++ show a ++ " " ++ show b

ordered (a, b) = a <= b
