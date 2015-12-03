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
gen_members = braced . unlines . map (++ ";") . zipWith to_member [0..]
    where
    to_member idx k = int_ty k ++ " p" ++ show idx
    int_ty 1 = "unsigned char"
    int_ty 2 = "unsigned short"
    int_ty 4 = "unsigned int"

braced xs = "{\n" ++ xs ++ "}"

gen_fn structname mems = decl ++ stmts ++ " return s; }\n"
    where
    decl = concat [structname, " mk", structname, "(unsigned char p, ",
                   structname, " s) {"]
    stmts = concat ["s.p" ++ show n ++ " += " ++ show (n + 1) ++ "; "
                   | n <- [0..length mems - 1]]

structs = unlines . (prelude ++ )
                  . zipWith mkstruct [0..] $ concatMap struct_of_size [1..9]
    where
    mkstruct idx mems =
        concat ["typedef struct ", gen_members mems, " S", show idx, ";\n"] ++
        "\n" ++ gen_fn ("S" ++ show idx) mems
    prelude = ["#pragma pack(1)"]
