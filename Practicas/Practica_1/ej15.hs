genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista e f n = f e : genLista (f e) f (n - 1)

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta a b = genLista a (\x -> x + 1) b