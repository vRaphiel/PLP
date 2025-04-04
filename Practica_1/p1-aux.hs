sum' :: [Int] -> Int
sum' = foldr (\x acc -> x + acc) 0

elem' x = foldr (\y acc-> x == y || acc) False 

concatenar xs ys = foldr (:) ys xs

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y)

mejorSegun' :: (a -> a -> Bool) -> [a] -> a
mejorSegun' _ [x] = x
mejorSegun' f (x:xs) = if f x rec then x else rec
    where rec = mejorSegun' f xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales [] = []
sumasParciales (x: xs) = x : sumar x (sumasParciales xs)
    where
        sumar _ [] = []
        sumar acc (y: ys) = acc + y : sumar (acc + y) ys

sumaAlt :: [Int] -> Int
sumaAlt = foldr (\x acc -> x - acc) 0

--- Ejercicio 4
-- Dada una lista devueve las permutaciones. Usar concatMap :: (a -> [b]) -> [a] -> [b], take, drop.
-- permutaciones :: [a] -> [[a]]

-- Ejercicio 5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)



entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)
-- Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
-- En caso contrario, explicar el motivo.