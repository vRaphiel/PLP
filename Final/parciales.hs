data Tren = Locomotora | Carga Tren | Pasajeros Int Tren 
  deriving Show

foldTren :: b -> (b -> b) -> (Int -> b -> b) -> Tren -> b
foldTren cLocomotora cCarga cPasajeros t = case t of 
    Locomotora      -> cLocomotora
    Carga tn        -> cCarga (rec tn)
    Pasajeros i tn  -> cPasajeros i (rec tn)
  where
    rec = foldTren cLocomotora cCarga cPasajeros

capacidadTotal :: Tren -> Int
capacidadTotal = foldTren 0 id (+)

invertir :: Tren -> Tren
invertir tren = foldTren base agregarCarga agregarPasajeros tren Locomotora
  where
    base :: Tren -> Tren
    base = id

    agregarCarga :: (Tren -> Tren) -> Tren -> Tren
    agregarCarga rec acc = rec (Carga acc)

    agregarPasajeros :: Int -> (Tren -> Tren) -> Tren -> Tren
    agregarPasajeros n rec acc = rec (Pasajeros n acc)

todosLosTrenesDePasajeros :: [Tren]
todosLosTrenesDePasajeros = generarPorPeso 0
  where
    -- Función recursiva explícita que junta los trenes de peso N 
    -- con los de peso N+1, N+2... hacia el infinito.
    generarPorPeso :: Int -> [Tren]
    generarPorPeso peso = trenesDePeso peso ++ generarPorPeso (peso + 1)

    -- Función auxiliar que genera todos los trenes EXACTOS de un peso dado
    trenesDePeso :: Int -> [Tren]
    trenesDePeso 0 = [Locomotora]
    trenesDePeso p = [ Pasajeros n t | n <- [1..p], t <- trenesDePeso (p - n) ]