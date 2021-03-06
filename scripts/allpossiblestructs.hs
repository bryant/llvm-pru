struct_of_size bytes = filter (all (`elem` [1, 2, 4])) . map g . sequence $
                            replicate (bytes - 1) "01"
    where g = barriers_to_struct_members

barriers_to_struct_members = map to_int . split_barriers

to_int xs = length xs + 1

split_barriers bars = case split_while (/= '1') bars of
    (member, []) -> [member]
    (member, rest) -> member : split_barriers (drop 1 rest)

split_while pred xs = (takeWhile pred xs, dropWhile pred xs)

gen_members :: [Int] -> String
gen_members = braced . unlines . indent 4 . map (++ ";")
                     . zipWith to_member [0..]
    where
    to_member idx k = int_ty k ++ " p" ++ show idx
    int_ty 1 = "unsigned char"
    int_ty 2 = "unsigned short"
    int_ty 4 = "unsigned int"

braced xs = "{\n" ++ xs ++ "}"

indent n xs = map (spaces ++) xs where spaces = replicate n ' '

gen_fn structname mems = braced . unlines . indent 4 $ adds ++ ["return s;"]
    where
    adds = ["s.p" ++ show n ++ " += " ++ show (n + 1) ++ ";"
           | n <- [0..length mems - 1]]

structs = unlines . (prelude ++ )
                  . zipWith mktestcase [0..] $ concatMap struct_of_size [1..9]
    where
    mktestcase idx mems = mkstruct idx mems ++ "\n\n" ++ mkfn idx mems ++ "\n"

    mkstruct idx mems =
        concat ["typedef struct ", gen_members mems, " S", show idx, ";"]

    mkfn idx mems = decl ++ " " ++ gen_fn structname mems
        where
        decl = concat [structname, " ", fnname, "(unsigned char p, ",
                       structname, " s)"]
        fnname = "mk" ++ structname
        structname = "S" ++ show idx

    prelude = ["#pragma pack(1)"]

main = putStrLn structs
