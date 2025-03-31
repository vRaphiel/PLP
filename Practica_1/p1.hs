{- Ejercicio 3 -}
{- Redefinir usando foldr las funciones sum, elem, (++), filter y map. -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
sum' :: [Int] -> Int
sum' = foldr (+) 0

{- Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento de la lista según una 
función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).-}
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x r -> if f x r then x else r)

{- Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ; [1,5,4,4,9]. -}
sumasParciales :: Num a => [a] -> [a]
sumasParciales [] = []
sumasParciales (x:xs) = x: sumasParcialesAux x xs
    where
            sumasParcialesAux :: Num a => a -> [a] -> [a]
            sumasParcialesAux _ [] = []
            sumasParcialesAux a (x:xs) = (a + x) : sumasParcialesAux (a + x) xs

{- 
sumasParciales' :: Num a => [a] -> [a]
sumasParciales' = snd . foldr (\x (rs1, rs2) -> (rs1 + x, (rs1 + x) : rs2)) (0, [])

sumasParciales'' :: Num a => [a] -> [a]
sumasParciales'' = tail . reverse . foldl (\acc x -> (x + head acc) : acc) [0] 
-}

{- Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
-}
sumaAlt :: [Int] -> Int
sumaAlt = foldr (\x r -> x + (-1)*r) 0

{- Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
etc.). Pensar qué esquema de recursión conviene usar en este caso-}
sumaAltInverse :: [Int] -> Int
sumaAltInverse = foldl (\ac x -> (-1)*ac + x) 0

{- Ejercicio 4 -}
{- Definir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutaciones. 
Se recomienda utilizar concatMap :: (a -> [b]) -> [a] -> [b], y también take y drop. -}
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\ x -> concatMap (rotaciones . (x :))) [[]]

rotaciones :: [a] -> [[a]]
rotaciones xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

{- Definir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
mismos elementos de L, en su mismo orden de aparición. 
Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]] (en algún orden). -}
partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ concatMap (\ys -> [ys, x : ys]) acc) [[]]

{- Definir la función prefijos, que dada una lista, devuelve todos sus prefijos.
Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]] -}
prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos x = concatMap (\n -> [take n x]) [0..length x]
{-     Es recursivo. Entonces lo que hacemos es, desde 0 hasta n -1 agregar a la lista mayor la lista actual
    Eso lo podemos hacer con un take
    Es decir,   si es 0 => take 0
                si es 1 => take 1
                si es 2 => take 2
                ...
                si es n => take n
    Todo eso lo podemos concatenar
    concatMap puede servir? Cual sería la funcion recursiva que se le pasa? Esta acepta un numero como parametro. 
    Si le pasamos en la funcion lambda el x que queremos usar y luego al concatenar buscamos que sea una lista de 
    elementos 0 a longitud de lista es suficiente.
    concatMap en su funcion lambda toma como n el elemento cabeza de la lista
    -}

{-Definir la función sublistas que, dada una lista, devuelve todas sus sublistas (listas de elementos que
aparecen consecutivos en la lista original).
Ejemplo: sublistas [5, 1, 2] → [[], [5], [1], [2], [5, 1], [1, 2], [5, 1, 2]] (en algún orden) -}
{-
    - Tomo 0 elementos. => Take 0
    - Tomo 1 elemento => Tomo cada valor de la lsita como un solo item => Drop 0 Take 1 | Drop 1 Take 1 | Drop 2 Take 1
    - Tomo 2 elementos de la lista (En orden)
    - Tomo 3 elementos de la lista (En orden)
-}

{- Ejercicio 5 -}
{- Considerar las siguientes funciones. Indicar si la recursión utilizada en cada una de ellas es o no estructural. 
Si lo es, reescribirla utilizando foldr. En caso contrario, explicar el motivo. -}

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)
-- No es estructural. Se usa xs en el IF

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)
-- Es estructural? 
entrelazar' :: [a] -> [a] -> [a]
entrelazar' xs ys = foldr (\x acc -> case ys of
                                       [] -> x : acc
                                       (y:ys') -> y : x : acc) [] xs

{- Ejercicio 6 -}
{- El siguiente esquema captura la recursión primitiva sobre listas. -}
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
{- Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
resultado de eliminar de la lista la primera aparición del elemento (si está presente). -}

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna a = recr (\x xs r -> if x == a then xs else x:r) []

{- Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
sacarUna del punto anterior. No es adecuado porque no se puede definir donde parar sin tener que acceder a usar al xs
por fuera del caso recursivo. -}

{- Definir la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
ordenada (de manera creciente), de manera que se preserva el ordenamiento. -}
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado a = recr (\x xs r -> if a <= x then a : x : xs else x : r) []

{- Ejercicio 7 -}
{- Definir las siguientes funciones para trabajar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
listas finitas e infinitas. -}
{- i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry. -}


{- ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial. -}