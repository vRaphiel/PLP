-- Esta currificada la funcion, por lo tanto, recibe los parametros uno a uno.
-- Luego cada item de la lista es una tupla, lo cual lo podemos representar.
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = foldr (\(x, y) rec -> f x y : rec) []

armarPares :: [a] -> [b] -> [(a, b)]
armarPares l1 l2 = foldr (\x rec -> (
                            \s2 -> case s2 of
                                    [] -> []
                                    (y:ys) -> (x: y): rec ys
)) (\_ -> []) l1 l2

{-- 
- Queremos procesar dos listas, pero `foldr` solo puede recorrer una. 
- La técnica: Hacemos que el `foldr` recorra la primera lista, pero en lugar de devolver la 
    lista final de pares, devuelve una función que está esperando la segunda lista.
- Por ende, el tipo del acumulador recursivo `rec` en este plegado no es una lista, 
    sino una función de tipo [b] -> [(a, b)]
--}

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f l1 l2 = foldr (\x rec -> (
                    \s2 -> case s2 of
                            [] -> []
                            (y:ys) -> f x y : rec ys
)) (\_ -> []) l1 l2