-- exactly which forms of mvi{b,w,d} are valid? `am335xPruReferenceGuide.pdf`
-- says:
--
-- ```
-- MVIB, MVIW, MVID
-- [*][&][--]REG1[++], [*][&][--]REG2[++]
-- [*][&][--]REG1[++], [*][&][--]REG2[++]
-- [*][&][--]REG1[++], [*][&][--]REG2[++]
-- Note that register pointer registers are restricted to r1.b0, r1.b1, r1.b2,
-- and r1.b3.
-- ```
--
-- but this definition doesn't always produce clpru-acceptable output. for
-- instance, `mvid r2, &r0` is considered invalid by the compiler.

import Data.Bits ((.&.))
import System.IO.Error (tryIOError)
import System.IO (hGetLine)
import System.Process (runInteractiveCommand)

-- generates a modifier that does: reg => --&*reg++
modifier :: Int -> String -> String
modifier n = mb' "++" (n .&. 0x08) . mb "&" (n .&. 0x04) . mb "*" (n .&. 0x02)
                                   . mb "--" (n .&. 0x01)
    where
    mb xs k | k == 0 = id | otherwise = (xs ++)
    mb' xs k | k == 0 = id | otherwise = (++ xs)

mod_id = [0..2 ^ 4 - 1]
regs = words "r0 r1 r2 r0.w0 r1.w0 r2.w0 r0.b0 r1.b0 r2.b0"
suffixes = "bwd"

cases = do
    suf <- suffixes
    lmod <- mod_id
    rmod <- mod_id
    lreg <- regs
    rreg <- regs
    let left = modifier lmod lreg
        right = modifier rmod rreg
    return $ "mvi" ++ [suf] ++ " " ++ left ++ ", " ++ right

indent n = (replicate n ' ' ++)

run_against_ti cases = do
    writeFile asm . unlines $ map (indent 4) cases
    (_, stdout, _, _) <- runInteractiveCommand cmd
    rv <- loop [] $ tryIOError (read . drop 8 <$> hGetLine stdout :: IO Int)
    return $ reverse rv
    where
    asm = "cases.asm"
    cmd = "./ti-pru-2.1.2/bin/clpru " ++ asm ++
          " 2>&1 | grep -o 'at line [0-9]\\+'"

loop acc act = do
    rv <- act
    case rv of
        Left _ -> return acc
        Right n -> loop (n : acc) act

filter_stream _ xs [] = xs
filter_stream _ [] _ = []
filter_stream n (x:xs) (l:ls)
    | n == l = filter_stream (n + 1) xs ls
    | otherwise = x : filter_stream (n + 1) xs (l : ls)

main = do
    fails <- run_against_ti cases
    mapM_ putStrLn $ filter_stream 0 cases fails
