sumaLista :: [Int] -> Int
sumaLista [] = 0 
sumaLista (x:xs) = x + sumaLista xs



foldr' :: (a -> b -> b) 
        -> b
        -> [a]
        -> b

foldr' fRec cB [] = cB 
foldr' fRec cB (x:xs) = fRec x (foldr' fRec cB xs)


elem' :: Eq a => a -> [a] -> Bool
elem' v [] = False
elem' v (x:xs)
    | x == v = True
    | otherwise = elem' v xs



elemConFold x = foldr (\y acc-> x == y || acc) False 