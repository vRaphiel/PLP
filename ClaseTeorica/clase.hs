esPar n = n `mod` 2 == 0
esImpar n = n `mod` 2 == 1

-- Funcion Lambda
-- \x -> e
-- Las funciones tiene un solo parametro 

data AB a = Nil
            | Bin (AB a) a (AB a)

inorder :: AB a -> [a]
inorder Nil         = []
inorder (Bin i r d) = inorder i ++ [r] ++ inorder d
-- inorder (Bin (Bin Nil 2 Nil) 3 (Bin Nil 4 Nil))

-- Pre: ABB (Sin repetidos)
-- Post: ABB sin repetidos
insertar :: Ord a => a -> AB a -> AB a
insertar x Nil          = Bin Nil x Nil
insertar x (Bin i r d)  = if x < r then insertar x i else if x == r then Bin i r d else Bin i r (insertar x d)

{-
    Recursion estructural: Al tener tipos de datos algebraicos T, las funciones en general estan definidas por n + m ecuaciones.
    Lo que debe cumplir el código para ser una definición por inducción estructural es que se puedan hacer llamada a los parametros sin usar a G. (No va en el caso recursivo)
    Ej.: suma(Hoja n) = n
-}

-- ¿Como podemos definir un foldAB que abstraiga el esquema de recursión estructural sobre árboles binarios?
-- ¿Cuales son los constructores y que tipo tienen?
-- Hay que ver cuantas funciones combinadoras hay: Una por cada constructor
foldAB :: 
    b                       -- NIL
    -> (b -> a -> b -> b)   -- BIN. Tenes que combinar el caso izquierdo BIN, la raiz, el caso derecho BIN, y devolves un Arbol.
    -> AB a                 
    -> b

Nil :: AB a
Bin :: AB a -> a -> AB a -> AB a

-- Por nemotecnica llamar al tipo del caso base como la funcion combinadora

foldAB cNil fBin cNil           = fNil -- Este sería el caso base
foldAB cNil cBin (Bin i r d)    = cBin (foldAB cNil cBin c) r (foldAB cNil cBin d)

-- Funciones estructurales: En caso base te devuelven algo fijo. Sobre el caso recursivo llaman algo para el lado izquierdo, para el derecho y combinan eso con la raiz de alguna manera.
