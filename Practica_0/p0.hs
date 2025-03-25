valorAbsoluto :: Float -> Float
valorAbsoluto = abs

bisiesto :: Int -> Bool
bisiesto b = mod b 4 == 0

factorial :: Int -> Int
factorial n
    | n == 0 = 1
    | n > 0 = n * factorial (n - 1)
    | otherwise = -1

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso a = Just (1 / a)

aEntero :: Either Int Bool -> Int
aEntero (Left n)  = n
aEntero (Right b)
    | b = 1
    | otherwise = 0

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

longitud :: [Float] -> Float
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

sumaTotal :: [Float] -> Float
sumaTotal [] = 0
sumaTotal (x: xs) = sumaTotal xs + x

promedio :: [Float] -> Float
promedio [] = 0
promedio x = sumaTotal x / longitud x

difPromedioCalc :: [Float] -> Float -> [Float]
difPromedioCalc [] _ = []
difPromedioCalc (x:xs) p = (x - p):difPromedioCalc xs p

difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio a = difPromedioCalc a (promedio a)

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l root r) = Bin (negacionAB l) (not root) (negacionAB r)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l root r) = root * productoAB l * productoAB r

