import Functions

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


slowSort :: Ord a => [a] -> [a]
slowSort [] = []
slowSort (p:xs) = slowSort menores ++ [p] ++ slowSort mayores
    where
        menores = [x | x <- xs, x <= p]
        mayores = [x | x <- xs, x > p]

-- No es estructural porque usamos el cuerpo de la lista en las funciones auxiliares, que son
-- generadores infinitas o subestructuras secundarias. Es recursión global.

-- Los llamados recursivos se hacen sobre las listas `menores` y `mayores`. 
-- Estas listas son subestructuras arbitrarias que se construyen filtrando dinámicamente los elementos de `xs` según su relación 
-- con el pivote `p`. 
-- Al no procesar la cola directa, la recursión es de tipo global o por curso de valores (course-of-values).

--  Dado que no es recursión estructural ni primitiva, no se puede reescribir usando `foldr` ni `recr`** directamente.

sufijos :: [a] -> [[a]]
sufijos [] = [[]]
sufijos (x:xs) = (x:xs) : sufijos xs

-- No es estructural, es recursiva primitiva. Esto debido a que el cuerpo de la lista se utiliza en otro sitio aparte del llamado
-- recursivo, justamente, en (x:xs). Con ello, puede reconstruir la lista original en cada paso y evaluar los sufijos de la demás lista. 

sufijos2 :: [a] -> [[a]]
sufijos2 = recr (\x xs rec -> (x:xs) : rec) [[]]

-- Si quisieramos simular la recursion primitiva usando foldr, podemos usar tuplas.
-- foldr es una tupla del tipo (cola_actual, resultado_acumulado)

sufijos3 :: [a] -> [[a]]
sufijos3 = snd (foldr (\x (ys, rec) -> (x:ys, (x:ys) : rec)) [[]])

miScanr :: (a -> b -> b) -> b -> [a] -> [b]
miScanr f n [] = [n]
miScanr f n (x:xs) = let (y:ys) = miScanr f n xs in (f x y) : (y:ys)

-- Recursión estructural. El xs no se usa en otro sitio que no sea sobre la función a la llamada recursiva.
-- El (y:ys) solo es el resultado de aplicar la función recursiva, por lo que, es recursión estructural sobre xs.
-- Reconstruyendola con foldr

miScanr2 :: (a -> b -> b) -> b -> [a] -> [b]
miScanr2 f n = recr (\x xs rec -> (f x (head rec)) : rec) [n]