data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin t = case t of
                        Nil       -> cNil
                        Bin i r d -> cBin (rec i) r (rec d)
    where
        rec = foldAB cNil cBin

recAB :: b -> (b -> AB a -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin t = case t of
                    Nil -> cNil
                    Bin i r d -> cBin (rec i) i r d (rec d)
    where
        rec = recAB cNil cBin

-- esNil, altura y cantNodos
esNil :: AB a -> bool
esNil = foldAB True (\_ _ _ -> False)

altura :: AB a -> Int
altura = foldAB 0 (\i r d -> 1 + max i d)

-- cantNodos :: AB a -> Int
-- cantNodos = recAB 0 (\recI i r d recD -> 1 + (if esNil i then 0 else recI) + (if esNil d then 0 else recD))

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\recI r recD -> 1 + recI + recD)

mejorSegun :: (a -> a -> Bool) -> AB a -> Maybe a
mejorSegun f t = case foldAB Nothing (\recI r recD -> Just (elegirMejor r recI recD)) t of
                    Nothing -> error "MejorSegun de un arbol vacio"
                    Just x  -> x
  where
    elegirMejor :: a -> Maybe a -> Maybe a -> a
    elegirMejor r recI recD = 
        let mejorI = case recI of
                       Nothing -> r
                       Just x  -> if f r x then r else x
        in case recD of
             Nothing -> mejorI
             Just y  -> if f mejorI y then mejorI else y

esABB :: Ord a => AB a -> Bool
esABB t = fst (foldAB (True, Nothing) (\recI r recD ->
    let
        -- Desarmamos los acumuladores que vienen de la izquierda y derecha
        (okI, boundsI) = recI
        (okD, boundsD) = recD
        
        -- 1. ¿Es válido el lado izquierdo respecto a la raíz 'r'?
        validoIzq = okI && case boundsI of
                             Nothing         -> True
                             Just (_, maxI)  -> maxI <= r
                             
        -- 2. ¿Es válido el lado derecho respecto a la raíz 'r'?
        validoDer = okD && case boundsD of
                             Nothing         -> True
                             Just (minD, _)  -> r < minD
                             
        -- 3. Si todo es válido, calculamos los nuevos extremos del árbol combinado
        nuevosLimites = if validoIzq && validoDer
                        then Just ( case boundsI of
                                      Nothing        -> r
                                      Just (minI, _) -> minI
                                  , case boundsD of
                                      Nothing        -> r
                                      Just (_, maxD) -> maxD
                                  )
                        else Nothing
    in (validoIzq && validoDer, nuevosLimites)
    ) t)

ramas :: AB a -> [[a]]
ramas = foldAB [] (\recI r recD -> 
                    if null recI && null recD
                    then [[r]] 
                    else map (\cam -> cam : (recI ++ recD))

cantHojas :: AB a -> Int
cantHojas = foldAB 0 (\recI r recD -> if recI == 0 && recD == 0 
                                        then 1 
                                        else recI + recD)

espejo :: AB a -> AB a
espejo = foldAB Nil (\recI r recD -> Bin recD r recI)


mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura a1 a2 = foldAB (\s2 -> case s2 of
                                         Nil -> True
                                         _   -> False
                               ) 
                               (\recI r recD -> \s2 -> 
                                  case s2 of
                                    Nil -> False
                                    Bin i2 _ d2 -> recI i2 && recD d2
                               ) 
                               a1 a2