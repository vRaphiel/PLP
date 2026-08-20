listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listaQueSumaN n = [ x : xs | x <- [1..], xs <- listasQueSuman (n - 1)]

{--
n = 1. 1 sola lista
n = 2: 2 listas, [1,1], [2]
n = 3: 4 listas, [1,1,1], [1,2], [2,1], [3]

La recursión no es estructural porque el llamado recursivo no se hace sobre (n-1), 
sino sobre (n-x), lo cual es recursión global)
--}
