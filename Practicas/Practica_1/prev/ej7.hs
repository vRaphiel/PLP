-- Esta versión sintacticamente esta bien, pero no cumple con lo pedido
mapPares :: ((a, b) -> (c, d)) -> [(a, b)] -> [(c, d)]
mapPares f = foldr (\x rec -> f x : rec) []

-- Esta es valida porque recibe una funcion de dos parametros currificada
mapPares2 :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares2 f = map (uncurry f)