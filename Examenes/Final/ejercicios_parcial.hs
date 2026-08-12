
{- Primer Recuperatorio - 2do Cuatrimestre de 2024 -}
data Operador = Sumar Int | DividirPor Int | Secuencia [Operador]

foldOperador :: (Int -> b) -> (Int -> b) -> ([b] -> b) -> Operador -> b
foldOperador cSumar cDividirPor cSecuencia t =
    case t of
        Sumar i -> cSumar i
        DividirPor i -> cDividirPor i
        Secuencia l -> cSecuencia (map rec l)
    where
        rec = foldOperador cSumar cDividirPor cSecuencia

falla :: Operador -> Bool
falla = foldOperador (const False) (\i -> i == 0) (any id)

aplanar :: Operador -> Operador
aplanar = foldOperador id id (\ls -> Secuencia (concatMap descomponer ops))
    where
        descomponer :: Operador -> [Operador]
        descomponer (Secuencia xs) = concatMap descomponer xs
        descomponer Op = [Op]

componerTodas :: [a -> a] -> (a -> a)
componerTodas [] = []
componerTodas [x1 : x2 : xs] = aplanar (x1 . x2)

{- Primer Parcial - 1er Cuatrimestre de 2025 -}
data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a) a (ABNV a)
abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

foldABNV :: (a -> b) -> (a -> b -> b) -> (b -> a -> b -> b) -> ABNV a -> b
foldABNV cHoja cUni cBi t = case t of
    Hoja v   -> cHoja v
    Uni v ab -> cUni v (rec ab)
    Bi i r d -> cBi (rec i) r (rec d)
    where
        rec = foldABNV cHoja cUni cBi

recABNV :: (a -> b) -> (a -> b -> ABNV a -> b) -> (b -> ABNV a -> a -> b -> ABNV a -> b) -> ABNV a -> b
recABNV cHoja cUni cBi t = case t of
    Hoja v   -> cHoja v
    Uni v ab -> cUni v (rec ab) ab
    Bi i r d -> cBi (rec i) i r (rec d) d
    where
        rec = recABNV cHoja cUni cBi

elemABNV :: Eq a => a -> ABNV a -> Bool
elemABNV value = foldABNV (== value) (\v rec -> v == value || rec) (\i r d -> r == value || d || i)

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno value repl = recABNV  (\x -> if x == value then Hoja repl else Hoja x) 
                                    (\v rec arb -> if v == value then Uni repl arb else Uni value rec) 
                                    (\i ri r d rd -> if r == value then Bi ri repl rd else if elemABNV value i then Bi i v rd else Bi ri v d)

nivel :: ABNV a -> Int -> [a]
nivel = foldABNV (\v -> (\i -> if i == 0 then [v] else []))
                 (\v rec -> (\i -> if i == 0 then [v] else rec (i - 1)))
                 (\i v d -> (\x -> if x == 0 then [v] else (i (x -1)) ++ (d (x - 1)))) 


-- Ejercicio 1: Recuperatorio 2025
data Tren = Locomotora | Carga Tren | Pasajeros Int Tren 
  deriving Show

foldTren :: b -> (b -> b) -> (Int -> b -> b) -> Tren -> b
foldTren cLocomotora cCarga cPasajeros t = case t of 
    Locomotora      -> cLocomotora
    Carga tn        -> cCarga (rec tn)
    Pasajeros i tn  -> cPasajeros i (rec tn)
  where
    rec = foldTren cLocomotora cCarga cPasajeros

capacidadTotal :: Tren -> Int
capacidadTotal = foldTren 0 id (+)

invertir :: Tren -> Tren
invertir tren = foldTren base agregarCarga agregarPasajeros tren Locomotora
  where
    base :: Tren -> Tren
    base = id

    agregarCarga :: (Tren -> Tren) -> Tren -> Tren
    agregarCarga rec acc = rec (Carga acc)

    agregarPasajeros :: Int -> (Tren -> Tren) -> Tren -> Tren
    agregarPasajeros n rec acc = rec (Pasajeros n acc)

todosLosTrenesDePasajeros :: [Tren]
todosLosTrenesDePasajeros = generarPorPeso 0
  where
    -- Función recursiva explícita que junta los trenes de peso N 
    -- con los de peso N+1, N+2... hacia el infinito.
    generarPorPeso :: Int -> [Tren]
    generarPorPeso peso = trenesDePeso peso ++ generarPorPeso (peso + 1)

    -- Función auxiliar que genera todos los trenes EXACTOS de un peso dado
    trenesDePeso :: Int -> [Tren]
    trenesDePeso 0 = [Locomotora]
    trenesDePeso p = [ Pasajeros n t | n <- [1..p], t <- trenesDePeso (p - n) ]
