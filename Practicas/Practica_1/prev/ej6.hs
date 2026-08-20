recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z []     = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if x == n then xs else x:xs) []

-- No es adecuado el sistema de foldr ya que requerimos el resto de la lista. No estamos
-- recorriendo solo para ver y aplicar cosas si no que necesitamos quedarnos con lo que
-- queda con la lista si no que concatenamos lo que hacemos

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec -> if n <= x then n:x:xs else x:rec) []
