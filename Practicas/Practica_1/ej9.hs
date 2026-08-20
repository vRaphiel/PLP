select :: [a] -> [(a, [a])]
select = foldr (\x rec -> (x, map fst rec) : map (\(y, ys) -> (y, x:ys)) rec) []

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\(y, ys) -> map (y:) (permutaciones ys)) (select xs)

permutaciones2 :: [a] -> [[a]]
permutaciones2 [] = [[]]
permutaciones2 xs = concatMap (\i -> 
    let 
        elemI = head (drop i xs)
        resto = take i xs ++ drop (i + 1) xs
    in map (elemI :) (permutaciones2 resto)
    ) [0 .. length xs - 1]

partes :: [a] -> [[a]]
partes = foldr (\x rec -> rec ++ map (x:) rec) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldr (\x rec -> [] : map (x:) rec) [[]]

sufijos :: [a] -> [[a]]
sufijos = foldr (\x rec -> (x : head rec) : rec) [[]]

sublistas :: Eq a => [a] -> [[a]]
sublistas xs = nub (foldr (\suf rec -> prefijos suf ++ rec) [] (sufijos xs))