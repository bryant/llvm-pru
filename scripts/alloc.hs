import qualified Data.Array as Arr
import Data.Array (Array)
import Data.List (intersect, foldl')

type RegFile = Array Int Bool

data Width = Byte | Word | DWord deriving Show

type MemLoc = (Int, Width)

size Byte = 1
size Word = 2
size DWord = 4

place_group :: RegFile -> [Width] -> [Int]
place_group _ [] = []
place_group regs [w] = place_width regs w
place_group regs blk@(u:ws@(_:_)) = map (subtract blksize) blkends
    where
    u_ends = map (+ size u) $ place_width regs u
    blkends = foldl' (place_next regs) u_ends ws
    blksize = sum $ map size blk

place_next :: RegFile -> [Int] -> Width -> [Int]
place_next regs prev_ends thisblk =
        map (+ size thisblk) . intersect prev_ends $ place_width regs thisblk

place_width :: RegFile -> Width -> [Int]
place_width regs Byte = filter (free_piece regs 1) [0..len regs - 1]
place_width regs Word = filter (free_piece regs 2) [b + off
                                                   | b <- [0, 4..len regs - 1],
                                                   off <- [0, 1, 2]]
place_width regs DWord = filter (free_piece regs 4) [0, 4..len regs - 1]

free_piece :: RegFile -> Int -> Int -> Bool
free_piece regs plen n =
        n + plen <= len regs && and [regs Arr.! (n + k) | k <- [0..plen - 1]]

len arr = 1 + snd (Arr.bounds arr)

chunks n regs = zip [0, n..] . c' n $ Arr.elems regs
    where
    c' n xs = case drop n xs of
        [] -> [take n xs]
        rest -> take n xs : c' n rest

mk_reg n = Arr.listArray (0, n - 1) $ replicate n True
