import GHC.Float.RealFracMethods (ceilingDoubleInt)
data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin t = case t of
                            Hoja a -> cHoja a
                            Bin i d -> cBin (rec i) (rec d)
    where   
        rec = foldAIH cHoja cBin

altura :: AIH a -> Integer
altura ai = foldAIH (const 1) (\recI recD -> 1 + max recI recD)