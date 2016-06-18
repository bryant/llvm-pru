module Main where

import Data.List (nub)
import Patterns (allpats, used_defs)
import MachNode (gen_def)
import System.Environment (getArgs)

main = do
    args <- getArgs
    case args of
        [which, outfile] ->
            let output | outfile == "-" = putStrLn
                       | otherwise = writeFile outfile
             in case which of
            "decls" -> output . unlines . nub . map gen_def $
                                    concatMap used_defs allpats
            "pats" -> output . unlines $ map show allpats
            _ -> usage
        _ -> usage
    where usage = putStrLn "gen2 (decls|pats) output_file"

