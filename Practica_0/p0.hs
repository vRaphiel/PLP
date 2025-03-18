valorAbsoluto :: Float -> Float
valorAbsoluto x = abs x

bisiesto :: Int -> Bool
bisiesto b = (mod b 4) == 0

factorial :: Int -> Int
factorial n
    | n == 0 = 1
    | n > 0 = n * factorial (n - 1)
    | otherwise = (-1)


data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

{-}
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso a = Just (1 / a)

aEntero :: Either Int Bool -> Int 
aEntero a
    | a == True = 1
    | b == False = 0
    | otherwise = a
-}

quitarLetra :: Char -> String -> String
quitarLetra a [] = []
quitarLetra a (s:xs)
    | a == s = quitarLetra a xs
    | otherwise = s : quitarLetra a xs

limpiar :: String -> String -> String
limpiar f [] = []
limpiar [] s = s
limpiar (f: xs) s = limpiar xs (quitarLetra f s)

todosIgualesA :: Int -> [Int] -> Bool
todosIgualesA a [] = True
todosIgualesA a (f: xf)
    | a == f = todosIgualesA a xf
    | otherwise = False

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [a] = True
todosIguales (a: xa) = todosIgualesA a xa

difPromedio :: [Float] -> [Float]
difPromedio xs = difPromedioRec xs 0 0

difPromedioRec :: [Float] -> Float -> Float -> [Float]
difPromedioRec [] _ _ = []
difPromedioRec (x:xs) total count = 
    let promedio = total / count
    in (x - promedio) : difPromedioRec xs (total + x) (count + 1)


{-
data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB a  
    == Nil = True
    | otherwise = False


definir las siguientes funciones:
b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación
de cada uno de los nodos.
c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol
-}