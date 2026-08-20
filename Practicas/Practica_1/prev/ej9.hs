foldNat :: Int -> (Int -> b) -> Int -> Int
foldNat cBase cRec t = case t of
                        0 -> cBase
                        x -> rec x
    where
        rec = foldNat cBase cRec 

potencia :: Int -> Int -> Int
potencia base exp = foldNat 1 (\rec -> base * rec) exp