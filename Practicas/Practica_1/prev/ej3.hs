foldSum :: [Int] -> Int
foldSum = foldr (+) 0

foldElem :: Eq a => a -> [a] -> Bool
foldElem e = foldr (\x rec -> x == e || rec) False

foldPlusPlus :: [a] -> [a] -> [a]
foldPlusPlus l1 l2 = foldr (:) l2 l1

foldFilter :: Eq a => a -> [a] -> [a]
foldFilter e = foldr (\x rec -> if x == e then rec else x:rec) []

foldMap :: (a -> b) -> [a] -> [b]
foldMap f = foldr (\x rec -> (f x) : rec) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = foldr (\x rec ac -> let suma = ac + x in suma : rec suma) (const []) xs 0

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

-- Si no usamos el flip quedaria x3 - x4, cuando queremos x4 - x3
sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (flip (-)) 0