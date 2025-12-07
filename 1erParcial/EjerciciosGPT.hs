recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

{- 1. sumatoria :: [Int] -> Int Suma todos los elementos de una lista de enteros.
Preguntas:
    ¿La operación que aplico se puede expresar con foldr? Si, por la recursión sobre lista
    ¿Necesito ver más de un elemento a la vez o solo el actual? El elemento actual y el acumulador
Recursión estructural (foldr o foldl). -}

sumatoria :: [Int] -> Int
sumatoria = foldr (\x acc -> x + acc) 0

sumatoria' :: [Int] -> Int
sumatoria' = foldl (\x acc -> x + acc) 0

{- 2. producto :: [Int] -> Int Multiplica todos los elementos.
¿Qué valor neutro tiene la multiplicación? ¿Hay riesgo con listas vacías? Si hay riesgo. Ya que el neutro es el 1 en la multiplicación y una list vacia tendría producto 0.
Recursión estructural con foldr.
Ideal para practicar valores neutros y errores con listas vacías. -}
producto :: [Int] -> Int
producto = foldr (\x acc -> x * acc) 1

{- sublistasConsecutivas :: [a] -> [[a]]
Dada una lista, devolver todas las sublistas formadas por elementos consecutivos.
Por ejemplo, para la lista [1,2,3] el resultado esperado podría ser: [[1],[2],[3],[1,2],[2,3],[1,2,3]]
- ¿Cómo elegís el inicio y el final de cada sublista?
- ¿Podés usar dos recursiones anidadas o un esquema basado en listas por comprensión?
- ¿Qué esquema de recursión te resulta natural para recorrer índices de la lista? -}
sublistasConsecutivas :: [a] -> [[a]]
sublistasConsecutivas [] = [[]]
sublistasConsecutivas x = concatMap (\n -> [take n x]) [1..length x]

{- intercalarListas :: [[a]] -> [a] 
Dada una lista de listas, devuelva una lista formada por la intercalación de los elementos de cada lista.
Por ejemplo, intercalarListas [[1,2,3],[4,5,6]]  -- podría producir [1,4,2,5,3,6]
- ¿Cómo combinás dos listas de forma intercalada?
- ¿Qué función auxiliar podrías definir para intercalar dos listas?
- ¿Podés luego usar un fold (por ejemplo, foldr) para combinar todas las listas de la lista principal? -}
intercalarListas :: [[a]] -> [a] 
intercalarListas [] = []
intercalarListas x
    | all null x = []
    | otherwise  = map head (filter (\x' -> if null x' then False else True) x) ++ intercalarListas (map tail (filter (not . null) x))

{- lista de diferencias sucesivas diferencias :: [Int] -> [Int]
Dada una lista [a, b, c, d], devolver [b - a, c - b, d - c].
- Usar recursión primitiva.
- Necesitás acceder a más de un elemento al mismo tiempo. -}
diferencias :: [Int] -> [Int]
diferencias [] = []
diferencias [a] = [a]
diferencias (x:y:xs) = (x-y):diferencias xs

-- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
{- Invertir lista invertir :: [a] -> [a] -}
invertir :: [a] -> [a]
invertir = foldl (\x acc -> acc:x) []

{- Dada una lista, eliminar elementos repetidos consecutivos. eliminarRepetidos :: Eq a => [a] -> [a]
Ejemplo: [1,1,2,2,2,3,1,1] → [1,2,3,1]
Sugerencias:
- Necesitás comparar el elemento actual con el anterior.
- Recursión primitiva o iterativa (con estado previo). -}
eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos = reverse . foldl (\acc x -> if null acc then [x] else if x == head(acc) then acc else x:acc) []

{- Pangram: Test: the quick brown fox jumps over the lazy dog -}
pangram :: String -> Bool
pangram str = checkIfPanagram "abcdefghijklmnopqrstuvwxyz" str
    where 
        checkIfPanagram :: String -> String -> Bool
        checkIfPanagram alph [] = null alph
        checkIfPanagram alph (x:xs) = checkIfPanagram (if x `elem` alph then remover alph x else alph) xs
        remover :: Eq a => [a] -> a -> [a]
        remover (x:xs) y = if y == x then xs else x:(remover xs y)
