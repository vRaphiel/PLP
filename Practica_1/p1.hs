{- Ejercicio 3 -}
{- Redefinir usando foldr las funciones sum, elem, (++), filter y map. -}
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
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
--mapPares f = map (\(x1,x2) -> f x1 x2)
mapPares f = map (uncurry f )

{- ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial. -}
armarPares :: [a] -> [b] -> [(a, b)]
armarPares x y  = f x y
    where
        f :: [a] -> [b] -> [(a,b)]
        f [x] (y:ys) = [(x,y)]
        f (x:xs) [y] = [(x,y)]
        f (x:xs) (y:ys) = (x,y) : f xs ys

armarPares' :: [a] -> [b] -> [(a, b)]
armarPares' xs ys = map (uncurry (,)) (aux xs ys)
  where
    aux (x:xs) (y:ys) = (x, y) : aux xs ys
    aux _ _ = []

{- Ejercicio 8 -}
{- i. Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una matriz como 
la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas, todas de la misma 
longitud, con elementos enteros. Recordamos que la suma de matrices se define como la suma celda a celda. 
Asumir que las dos matrices a sumar están bien formadas y tienen las mismas dimensiones.
sumaMat :: [[Int]] -> [[Int]] -> [[Int]] -}
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (\x y -> zipWith (+) x y)

{- ii. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
en la posición i, j del resultado está el contenido de la posición j, i de la matriz original. Notar que si la
entrada es una lista de N listas, todas de longitud M , la salida debe tener M listas, todas de longitud N .
trasponer :: [[Int]] -> [[Int]] -}

{- Ejercicio 10 -}
{- Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de 
elementos, a partir de un elemento inicial y de una función de incremento entre los elementos de la lista.
Dicha función de incremento, dado un elemento de la lista, devuelve el elemento siguiente. -}
genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista x f n = x : genLista (f x) f (n - 1)

{- Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el segundo), 
devuelve una lista de números consecutivos desde el primero hasta el segundo. -}
desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (+1) (y - x + 1)

{- Ejercicio 12 -}
data AB a = Nil | BinAB (AB a) a (AB a)

arbolUno = BinAB (BinAB Nil 1 Nil) 2 (BinAB Nil 3 (BinAB Nil 4 Nil))
arbolDos = BinAB (BinAB Nil 4 Nil) 2 (BinAB Nil 3 (BinAB Nil 4 Nil))
arbolTres = BinAB (BinAB (BinAB Nil 1 (BinAB Nil 2 Nil)) 4 Nil) 2 (BinAB (BinAB Nil 4 Nil) 3 (BinAB Nil 4 Nil))

{- Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), 
y dar sus tipos. -}
foldAB ::  b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin t = case t of
                        Nil -> cNil
                        BinAB i r d -> cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

foldAB1 :: (b -> a -> b -> b) -> AB a -> b
foldAB1 cBin (BinAB i r d) = cBin (foldAB1 cBin i) r (foldAB1 cBin d)

-- Caso base. Funcion que tiene como recursivo los subarboles enteros, los resultados de los nodos, la raiz.
recAB :: b -> (AB a -> b -> a -> b -> AB a -> b) -> AB a -> b
recAB cNil cBin Nil = cNil
recAB cNil cBin (BinAB i r d) = cBin i (recAB cNil cBin i) r (recAB cNil cBin d) d

{- Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB o recAB). -}
esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Integer
altura = foldAB 0 (\i _ d -> 1 + max i d)

cantNodos :: AB a -> Integer
cantNodos = foldAB 0 (\i r d -> 1 + i + d)

{- Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión para un 
árbol que puede o no ser Nil. -}
mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
mejorSegunAB f Nil = error "No se puede aplicar a un árbol vacío"
mejorSegunAB f (BinAB Nil r Nil) = r
mejorSegunAB f (BinAB Nil r d) = if f r (mejorSegunAB f d) then r else mejorSegunAB f d
mejorSegunAB f (BinAB i r Nil) = if f r (mejorSegunAB f i) then r else mejorSegunAB f i
mejorSegunAB f (BinAB i r d) = if compararResultadoAB f r (mejorResultadoIterativo f i d) then r else mejorResultadoIterativo f i d
    where
        mejorResultadoIterativo :: (a -> a -> Bool) -> AB a -> AB a -> a
        mejorResultadoIterativo f i d = if f (mejorSegunAB f i) (mejorSegunAB f d) then mejorSegunAB f i else mejorSegunAB f d

        compararResultadoAB :: (a -> a -> Bool) -> a -> a -> Bool
        compararResultadoAB f = f


{- Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que aparecen en el 
subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol derecho. -}

{- esABB debe ser primitiva. Ya que se necesita ver el resto del arbol en cada paso mas el paso actual -}
esABB :: Ord a => AB a -> Bool
esABB = recAB True (\abi i r d abd -> esSubArbIzqABB abi r && esSubArbDerABB abd r)
    where
        esSubArbIzqABB :: Ord a => AB a -> a -> Bool
        esSubArbIzqABB Nil _ = True
        esSubArbIzqABB (BinAB i r' d) a = (r' < a) && (esSubArbIzqABB i r' && esSubArbDerABB d r')

        esSubArbDerABB :: Ord a => AB a -> a -> Bool
        esSubArbDerABB Nil _ = True
        esSubArbDerABB (BinAB i r' d) a = (r' > a) && (esSubArbIzqABB i r' && esSubArbDerABB d r')

{- Ejercicio 13 - Sigue del Ejercicio 12 -}
{- Definir las funciones ramas (caminos desde la raíz hasta las hojas), cantHojas y espejo. -}
ramas :: AB a -> [[a]]
ramas = foldAB [] (\i r d -> if null i && null d then [[r]] else map (r:) (i ++ d))

cantHojas :: AB a -> Integer
cantHojas = foldAB 0 (\i r d -> if i == 0 && d == 0 then 1 else i + d)

-- espejo
espejo :: AB a -> AB a
espejo = foldAB Nil (\i r d -> BinAB (espejo d) r (espejo i))

{- Definir la función mismaEstructura :: AB a -> AB b -> Bool que, dados dos árboles, indica si éstos tienen la misma 
forma, independientemente del contenido de sus nodos. Pista: usar evaluación parcial y recordar el ejercicio 7 -}
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil Nil = True
mismaEstructura (BinAB i1 _ d1) (BinAB i2 _ d2) = mismaEstructura i1 i2 && mismaEstructura d1 d2
mismaEstructura _ _ = False

{- Ejercicio 14 -}
{- Se desea modelar los árboles con información en las hojas (y sólo en ellas). Se introduce el siguiente tipo: -}
data AIH a = Hoja a | BinAIH (AIH a) (AIH a)
arbolAIHUno = BinAIH (Hoja 1) (BinAIH (Hoja 2) (Hoja 3))
arbolAIHDos :: AIH Integer
arbolAIHDos = BinAIH (BinAIH (BinAIH (BinAIH (Hoja 1) (Hoja 2)) (Hoja 3)) (Hoja 4)) (Hoja 5)
{- Definir el esquema de recursión estructural foldAIH y dar su tipo. Por tratarse del primer esquema de
recursión que tenemos para este tipo, se permite usar recursión explícita. -}
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin t = case t of
                    Hoja i -> cHoja i
                    BinAIH i d -> cBin (rec i) (rec d)
    where
        rec = foldAIH cHoja cBin

{- Escribir las funciones altura :: AIH a -> Integer y tamaño :: AIH a -> Integer.
Considerar que la altura de una hoja es 1 y el tamaño de un AIH es su cantidad de hojas. -}
alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (\i d -> 1 + max i d)

tamañoAIH :: AIH a -> Integer
tamañoAIH = foldAIH (const 1) (\i d -> i + d)

{- Definir la función sumaAIH :: Num a => AIH a -> a, que suma los valores de las hojas. -}
sumaAIH :: Num a => AIH a -> a
sumaAIH = foldAIH id (+)

{- Ejercicio 16 -}
{- Se desea representar conjuntos mediante Hashing abierto. El Hashing abierto consta de dos funciones: 
una función de Hash, que dado un elemento devuelve un valor entero (el cual se espera que no se repita con frecuencia),
y una tabla de Hash, que dado un número entero devuelve los elementos del conjunto a los que la función de Hash asignó 
dicho número (es decir, la preimagen de la función de Hash para ese número). Se representarán como:
    data HashSet a = Hash (a -> Integer) (Integer -> [a])
Por contexto de uso, vamos a suponer que la tabla de Hash es una función total, que devuelve listas vacías para los 
números que no corresponden a elementos del conjunto. Este es un invariante que deberá preservarse en todas las 
funciones que devuelvan conjuntos -}
data HashSet a = Hash (a -> Integer) (Integer -> [a])

hashSetUno = agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5)
hashSetDos = agregar 3 $ agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5)
hashSetTres = Hash (\x -> x `mod` 3) (\i -> case i of
                                          0 -> [3, 6]
                                          1 -> [1, 4]
                                          2 -> [2, 5])

{-  vacío :: (a -> Integer) -> HashSet a, que devuelve un conjunto vacío con la función de Hash indicada. -}
vacío :: (a -> Integer) -> HashSet a
vacío f = Hash f (const [])

{- pertenece :: Eq a => a -> HashSet a -> Bool, que indica si un elemento pertenece a un conjunto. Es decir, si se 
encuentra en la lista obtenida en la tabla de Hash para el número correspondiente a la función de Hash del elemento.
Por ejemplo:
pertenece 5 $ agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5) devuelve False.
pertenece 2 $ agregar 1 $ agregar 2 $ agregar 1 $ vacío (flip mod 5) devuelve True. -}
pertenece :: Eq a => a -> HashSet a -> Bool
pertenece a (Hash f t) = elem a (t (f a))

{- agregar :: Eq a => a -> HashSet a -> HashSet a, que agrega un elemento a un conjunto. Si el elemento ya estaba en el
conjunto, se debe devolver el conjunto sin modificaciones. -}
agregar :: Eq a => a -> HashSet a -> HashSet a
agregar a (Hash f t) = if pertenece a (Hash f t) then Hash f t else Hash f (\x -> if x == f a then a : t x else t x)

{- intersección :: Eq a => HashSet a -> HashSet a -> HashSet a que, dados dos conjuntos, devuelve un conjunto con la 
misma función de Hash del primero y con los elementos que pertenecen a ambos conjuntos a la vez. -}
intersección :: Eq a => HashSet a -> HashSet a -> HashSet a
intersección (Hash f1 t1) (Hash f2 t2) = Hash f1 (\i -> [x | x <- t1 i, elem x (t2 i)])

{- foldr1(no relacionada con los conjuntos). Dar el tipo y definir la función foldr1 para listas sin usar recursión 
explícita, recurriendo a alguno de los esquemas de recursión conocidos. Se recomienda usar la función 
error :: String -> a para el caso de la lista vacía -}
foldr1HashSet :: (a -> a -> a) -> [Integer] -> HashSet a -> a
foldr1HashSet cRec hs (Hash _ valFn) = foldr1 cRec (concatMap valFn hs)

{- Ejercicio 18 -}
paresDeNat :: [(Int, Int)]
paresDeNat = [(x,n - x) | n <- [0..10], x <- [0..n]]

{- Ejercicio 19 -}
{- Una tripla pitagórica es una tripla (a, b, c) de enteros positivos tal que a2 + b2 = c2.
La siguiente expresión intenta ser una definición de una lista (infinita) de triplas pitagóricas:
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]
Explicar por qué esta definición no es útil. Dar una definición mejor. 

No es util ya que se esta dando un conjunto infinito de valores y, como a es infinito, este nunca cambia. Lo mismo con b,
con b se queda fijo en un valor y como c es infinito este sigue creciendo y ni a ni b van a cambiar.
Otro enfoque podría ser un plano tridimencional de pares de Nat, que sean triplas
-}
pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [ (a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

{- Ejercicio 20 -}
{- Escribir la función listasQueSuman :: Int -> [[Int]] que, dado un número natural n, devuelve todas las
listas de enteros positivos (es decir, mayores o iguales que 1) cuya suma sea n. Para este ejercicio se permite
usar recursión explícita. Pensar por qué la recursón utilizada no es estructural. (Este ejercicio no es de
generación infinita, pero puede ser útil para otras funciones que generen listas infinitas de listas). -}
listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [ x:xs | x <- [1..n], xs <- listasQueSuman (n - x) ]

{- Ejercicio 21 -}
{- Definir en Haskell una lista que contenga todas las listas finitas de enteros positivos (esto es, con elementos
mayores o iguales que 1). -}


{- Ejercicio 22 -}
{- Dado el tipo de datos AIH a definido en el ejercicio 14:
a) Definir la lista (infinita) de todos los AIH cuyas hojas tienen tipo ()1. Se recomienda definir una función
auxiliar. Para este ejercicio se permite utilizar recursión explícita.
b) Explicar por qué la recursión utilizada en el punto a) no es estructural -}