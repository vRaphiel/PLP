menosDe5Letras :: [String] -> [String]
menosDe5Letras = filter (\x -> length x < 5)

estanAprobados :: [Integer] -> [Bool]
estanAprobados = map (\x -> x > 6)

todosLosParesAlCuadrado :: [Integer] -> [Integer]
todosLosParesAlCuadrado lista = map (\x -> x * x) (filter (\x -> mod x 2 == 0) lista)

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
sumaAlt = foldr (\x rec -> x - rec) 0

sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (\acum x -> x - acum) 0

componerTodas :: [a -> a] -> a -> a
componerTodas = foldr (\f rec -> \x -> f (rec x)) (id)

{--
También es súper válido escribirlo de forma un poco más compacta usando el operador de composición 
nativo (.) de Haskell, si te sentís cómodo:

componerTodas :: [a -> a] -> a -> a
componerTodas = foldr (\f rec -> f . rec) (\x -> x)

Trazamos el ejemplo del enunciado:
componerTodas [flip mod 2, (+1), (*3)] aplicado a 2
Haskell expande la composición de derecha a izquierda:
1.  Aplicamos primero la función de más a la derecha: `(*3) 2` (\implies 6).
2.  Aplicamos la siguiente: `(+1) 6` (\implies 7).
3.  Aplicamos la última: `flip mod 2 7` (\implies \text{mod } 7\ 2 = \mathbf{1}).

¡Da exactamente `1` tal como lo pide el ejemplo de la guía!
--}