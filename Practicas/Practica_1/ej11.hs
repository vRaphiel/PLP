elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
then [x]
else x : elementosEnPosicionesPares (tail xs)

elementosEnPosicionesPares2 :: [a] -> [a]
elementosEnPosicionesPares2 xs = fst (foldr (\x (par, impar) -> (x:impar, par)) ([], []) xs)
-- No es estructural, ya que se usa la cola de la list, el cuerpo de la estructura, para otra operación
-- que no sea el llamado recursivo

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)

-- Es estructural. La recursión es sobre xs, se usa su cabeza y luego solo se pasa la cola
-- de la lista a las demas subfunciones. Si es sobre ys enotnces no es estructural.

entrelazar2 :: [a] -> [a] -> [a]
entrelazar2 = foldr (\x rec ys -> if null ys
                                 then x : rec []
                                 else x : head ys : rec (tail ys)) id