import Data.List

-- Recuperatorio 2do Cuat 2024
data Operador = Sumar Int | DividirPor Int | Secuencia [Operador]
{-  Sumar n representa la operacion que suma n a un numero entero.
    Dividir por representa la operación que divide un entero por n (Descartando el resto).
    Secuencia ops representa la composición a izquierda de todas las operaciones en ops. 
    Es decir, representa la operación de aplicar las operaciones en ops de izquierda a derecha,
    siendo resultado de cada operación la entrada de la siguiente.
    Ej: Secuencia [Sumar 5, DividirPor2] representa la operación que, dado un entero, le suma 5 y al resultado
    lo divide por 2.
-}

-- a) Dar el tipo y definir la función foldOperator, el esquema de recursión estructural para el tipo Operador
foldOperator :: (Int -> b) -> (Int -> b) -> ([b] -> b) -> Operador -> b
foldOperator cSumar cDividir cSecuencia t = case t of
    Sumar n       -> cSumar n
    DividirPor n  -> cDividir n
    Secuencia ops -> cSecuencia (map rec' ops)
    where
        rec' = foldOperator cSumar cDividir cSecuencia

falla:: Operador -> Bool
falla = foldOperator (\_ -> False) (\n -> n == 0) (any id)

aplanar :: Operador -> Operador
aplanar op = caso (aplanado op)
    where
        aplanado :: Operador -> [Operador]
        aplanado = foldOperator (\n -> [Sumar n]) (\n -> [DividirPor n]) (concatMap id)

        caso [x] = x
        caso xs = Secuencia xs

componerTodas :: [a -> a] -> (a -> a)
componerTodas [] = id
componerTodas (x:xs) = x . (componerTodas xs)

{- Primer recuperatorio 1er cuatrimestre de 2025 -}
data LineaProd = Materiales [String] | Agregar String LineaProd | Unir LineaProd LineaProd

l1 = Unir
    (Materiales ["acero", "cobre"])
    (Unir (Agregar "madera" (Materiales ["madera"])) (Materiales ["mercurio"]))
l2 = Unir
    (Materiales ["acero", "cobre"])
    (Unir
    (Agregar "madera" (Materiales ["aluminio"]))
    (Agregar "aluminio" (Materiales ["plastico"])))
l3 = Unir
    (Materiales ["m1"])
    (Unir (Agregar "m3" (Materiales ["m3", "m4"])) (Materiales ["m1", "m2"]))

foldLineaProd :: ([String] -> b) -> (String -> b -> b) -> (b -> b -> b) -> LineaProd -> b
foldLineaProd cMateriales cAgregar cUnir t = case t of
    Materiales s -> cMateriales s
    Agregar s l  -> cAgregar s (rec l)
    Unir l1 l2   -> cUnir (rec l1) (rec l2)
    where
        rec = foldLineaProd cMateriales cAgregar cUnir  

recLineaProd :: ([String] -> b) -> (String -> b -> LineaProd -> b) -> (b -> b -> LineaProd -> LineaProd -> b) -> LineaProd -> b
recLineaProd cMateriales cAgregar cUnir t = case t of
    Materiales s -> cMateriales s
    Agregar s l  -> cAgregar s (rec l) l
    Unir l1 l2   -> cUnir (rec l1) (rec l2) l1 l2
    where
        rec = recLineaProd cMateriales cAgregar cUnir

materialesUsados :: LineaProd -> [String]
materialesUsados = foldLineaProd id (\s rec -> nub (s:rec)) (\rec1 rec2 -> nub (rec1 ++ rec2))

sublineasDisjuntas :: LineaProd -> Bool
sublineasDisjuntas = recLineaProd (const True)
                        (\s rec l -> rec)
                        (\rec1 rec2 l1 l2 -> (rec1 && rec2) && 
                            null (intersect (materialesUsados l1) (materialesUsados l2)))

mismaEstructura :: LineaProd -> LineaProd -> Bool
mismaEstructura = foldLineaProd cMat cAg cUn
  where
    cMat _ = foldLineaProd (\_ -> True) (\_ _ -> False) (\_ _ -> False)
    cAg _ rec = recLineaProd (\_ -> False) (\_ _ sub2 -> rec sub2) (\_ _ _ _ -> False)
    cUn rec1 rec2 = recLineaProd (\_ -> False) (\_ _ _ -> False) (\_ _ sub2a sub2b -> rec1 sub2a && rec2 sub2b)
