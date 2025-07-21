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
falla = foldOperator (\_     -> False) (\n     -> n == 0) (any id)

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