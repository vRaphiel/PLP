listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listaQueSumaN n = [ x : xs | x <- [1..], xs <- listasQueSuman (n - 1)]

todasLasListasFinitas :: [[Int]]
todasLasListasFinitas = concat (map (\s -> listasQueSuman s) [1..])