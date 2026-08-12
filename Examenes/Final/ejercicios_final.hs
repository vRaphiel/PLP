{-
Ejercicio 3 (Programación funcional). El tipo algebraico AdjAB a describe a las adjunciones de árboles binarios con elementos de tipo a. Las AdjAB representan listas de árboles binarios, donde cada uno de ellos está al mismo nivel que el otro, que pueden o no tener una raíz en común de acuerdo al constructor usado.

data AdjAB a = Raiz a [AB a] | Adj [AB a]
data AB a = Nil | Hoja a | Arb a (AB a) (AB a)

Por ejemplo, las siguientes expresiones denotan distintos árboles de tipo AdjAB Int:

    Adj [Nil, Nil, Arb 3 Nil Nil, Hoja 6]
    Raiz 5 [Arb 3 (Arb 1 Nil Nil) (Hoja 4), Arb 3 Nil Nil, Nil]
    Raiz 10 []

a) Dar el tipo y definir una función foldAdjAB, que abstraiga el esquema de recursión estructural sobre las adjunciones de árboles binarios, recorriendo todos los árboles por completo.

b) Sin usar recursión explícita, dar el tipo y definir una función mapAdjAB que aplique una función dada a todos los valores de una adjunción.

c) Dar el tipo y definir recAdjAB que abstraiga el esquema de recursión primitiva sobre las adjunciones, recorriendo todos los árboles por completo.

d) Sin usar recursión explícita, dar el tipo y definir la función ordenado, que permita saber si toda la adjunción está ordenada. Así, cada árbol debe estar ordenado como un ABB y, adicionalmente, si hubiera raíz común, esta debe ser mayor que la raíz de todos los árboles. En el caso de Nil, este árbol está trivialmente ordenado y, además, se lo puede ignorar para comparar con la raíz común.
-}

data AdjAB a = Raiz a [AB a] | Adj [AB a]
data AB a = Nil | Hoja a | Arb a (AB a) (AB a)

foldAB :: c -> (a -> c) -> (a -> c -> c -> c) -> AB a -> c
foldAB cNil cHoja cArb t = case t of
    Nil         -> cNil
    Hoja x      -> cHoja x
    Arb x i d   -> cArb x (rec i) (rec d)
  where
    rec = foldAB cNil cHoja cArb

foldAdjAB :: (a -> [c] -> b) -> ([c] -> b) -> c -> (a -> c) -> (a -> c -> c -> c) -> AdjAB a -> b
foldAdjAB cRaiz cAdj cNil cHoja cArb t = case t of 
    Raiz r l    -> cRaiz r (map rec l)
    Adj l       -> cAdj (map rec l)
  where
    rec = foldAB cNil cHoja cArb

mapAdjAB :: (a -> b) -> AdjAB a -> AdjAB b
mapAdjAB f = foldAdjAB
            (\r l -> Raiz (f r) l)
            Adj
            Nil
            (\r -> Hoja (f r))
            (\r i d -> Arb (f r) i d)

recAB :: c -> (a -> c) -> (a -> c -> c -> AB a -> AB a -> c) -> AB a -> c
recAB cNil cHoja cArb = recursor
  where
    recursor Nil         = cNil
    recursor (Hoja x)    = cHoja x
    recursor (Arb x i d) = cArb x (recursor i) (recursor d) i d

recAdjAB :: (a -> [c] -> [AB a] -> b) -> ([c] -> [AB a] -> b) -> c -> (a -> c) -> (a -> c -> c -> AB a -> AB a -> c) -> AdjAB a -> b
recAdjAB cRaiz cAdj cNil cHoja cArb = recursorAdj
  where
    recursorAdj (Raiz r l) = cRaiz r (map (recAB cNil cHoja cArb) l) l
    recursorAdj (Adj l)    = cAdj (map (recAB cNil cHoja cArb) l) l

ordenado :: Ord a => AdjAB a -> Bool
ordenado = recAdjAB 
    (\r rec arbs -> and rec && all (r >) (raices arbs)) 
    (\rec arbs -> and rec)
    True
    (\_ -> True)
    (\x reci resd abi abd -> reci && resd && todosMenores x abi && todosMayores x abd)
  where
    todosMenores x arbol = all (< x) (aplanar arbol)
    todosMayores x arbol = all (> x) (aplanar arbol)
    aplanar = foldAB [] (\x -> [x]) (\x i d -> i ++ [x] ++ d)
    raices = concatMap raizDe
    raizDe Nil         = []
    raizDe (Hoja x)    = [x]
    raizDe (Arb x _ _) = [x]

{-
Ejercicio 1

Programación Funcional

Sean los siguientes tipos:

data Indice = I1 | I2 | I3
data AT a = Hoja a | Nodo (Indice → AT a)

En donde AT a representa el tipo de dato "Árbol Trébol", dar:

  a. Dar el tipo e implementar foldAT
  b. Definir la función altura :: AT a → Int
  c. Dar el tipo e implementar mapAT que, dada una función, se la aplica a todas las hojas de un árbol trébol.

Todo nodo SIEMPRE tiene 3 árboles trébol asociados. 
-}

data Indice = I1 | I2 | I3
data AT a = Hoja a | Nodo (Indice -> AT a)

foldIndice :: b -> b -> b -> Indice -> b
foldIndice cI1 cI2 cI3 t = 
    case t of 
        I1 -> cI1
        I2 -> cI2
        I3 -> cI3

foldAT :: (a -> b) -> ((Indice -> b) -> b) -> AT a -> b
foldAT cHoja cNodo t =
    case t of
        Hoja a -> cHoja a
        Nodo f -> cNodo (rec . f)
    where
        rec = foldAT cHoja cNodo

altura :: AT a -> Int
altura = foldAT (const 0) (\f -> 1 + max (f I1) (max (f I2) (f I3)))

mapAT :: (a -> b) -> AT a -> AT b
mapAT f = foldAT (\a -> Hoja (f a)) (\f -> Nodo f)

{- Final 29 de Mayo de 2026 -}

data Trie a = TrieNodo (Maybe a) [(Char, Trie a)]

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cTrieNodo t = case t of
    TrieNodo v l -> cTrieNodo v (map (\(c, h) -> (c, rec h)) l)
    where
        rec = foldTrie cTrieNodo

palabras :: Trie a -> [[Char]]
palabras = foldTrie (\v l -> 
    let palabrasDeLosHijos = concatMap (\(c, palabrasDelHijo) -> map (c:) palabrasDelHijo) l
    in case v of
        Just _  -> "" : palabrasDeLosHijos
        Nothing -> palabrasDeLosHijos
    )

mapTrie :: (a -> b) -> Trie a -> Trie b
mapTrie f = foldTrie (\v l -> TrieNodo (transformar f v) l)
    where
        transformar :: (a -> b) -> Maybe a -> Maybe b
        transformar f (Just v) = Just (f v)
        transformar f Nothing  = Nothing

conLongitud :: Trie a -> Trie (a, Int)
conLongitud t = foldTrie (\v l -> \longitudActual ->
        let v' = fmap (\x -> (x, longitudActual)) v
            l' = map (\(c, funcionHijo) -> (c, funcionHijo (longitudActual + 1))) l
        in TrieNodo v' l'
    ) t 0

{-
Final de Julio del 25-07-25
Considerar la funcion:
-}
foldu z f []     = z
foldu z f (x:xs) = f (x (foldu z f xs)) (foldu z f xs)

-- Dar el tipo de foldu
foldu :: b -> (c -> b -> b) -> [b -> c] -> b

-- Definir foldr usando foldu
-- foldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr fr cb xs = foldu cb fr (map const xs)

-- Definir foldu usando foldr
myFoldu :: b -> (c -> b -> b) -> [b -> c] -> b
myFoldu cb f xs = foldr (\x rec -> f (x rec) rec) cb xs