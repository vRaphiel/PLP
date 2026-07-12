import Test.HUnit

-- ------------------------------------------------------------
-- Tipos base (provistos por la cátedra — NO modificar)
-- ------------------------------------------------------------

type Carga = Int

type Altura = Int

type CantidadDeCristales = Int

data Cueva
  = Salida Altura
  | Depósito CantidadDeCristales Cueva
  | Compuerta (Carga -> Bool) Cueva
  | Bifurcación (Carga -> Dirección) Cueva Cueva

data Dirección = Izq | Der

-- ============================================================
-- EJERCICIO 1
-- ============================================================

recCueva :: (Altura -> b) -> (CantidadDeCristales -> Cueva -> b -> b) -> ((Carga -> Bool) -> Cueva -> b -> b) -> ((Carga -> Dirección) -> Cueva -> b -> Cueva -> b -> b) -> Cueva -> b
recCueva fSalida fDeposito fCompuerta fBifurcacion c = case c of
  Salida x -> fSalida x
  Depósito x y -> fDeposito x y (rec y)
  Compuerta x y -> fCompuerta x y (rec y)
  Bifurcación f izq der -> fBifurcacion f izq (rec izq) der (rec der)
  where
    rec = recCueva fSalida fDeposito fCompuerta fBifurcacion

-- ============================================================
-- EJERCICIO 2
-- ============================================================

foldCueva :: (Altura -> b) -> (CantidadDeCristales -> b -> b) -> ((Carga -> Bool) -> b -> b) -> ((Carga -> Dirección) -> b -> b -> b) -> Cueva -> b
foldCueva fSalida fDeposito fCompuerta fBifurcacion =
  recCueva
    fSalida
    (\n _ rec -> fDeposito n rec)
    (\f _ rec -> fCompuerta f rec)
    (\f _ recIzq _ recDer -> fBifurcacion f recIzq recDer)

-- ============================================================
-- EJERCICIO 3
-- ============================================================

cantidadNodos :: Cueva -> Int
cantidadNodos = foldCueva (const 1) (\_ rec -> 1 + rec) (\_ rec -> 1 + rec) (\_ rec1 rec2 -> 1 + rec1 + rec2)

-- ============================================================
-- EJERCICIO 4
-- ============================================================

profundidad :: Cueva -> Int
profundidad = foldCueva (const 1) (\_ rec -> 1 + rec) (\_ rec -> 1 + rec) (\_ rec1 rec2 -> 1 + max rec1 rec2)

-- Nota: con cantidadNodos compartimos casi la misma algebra del fold; solo cambia la bifurcacion (+ vs max). 
-- Pensamos unificar con algun helper los casos (\_ rec -> 1 + rec) pero sentimos que la lambda explicita daba a entender mejor lo que buscabamos hacer.

-- ============================================================
-- EJERCICIO 5
-- ============================================================

cristalesDeDepositos :: Cueva -> [CantidadDeCristales]
cristalesDeDepositos =
  foldCueva
    (const [])
    (\n recAbajo -> n : recAbajo)
    (\f recAbajo -> recAbajo)
    (\f recIzq recDer -> (++) recIzq recDer)

-- ============================================================
-- EJERCICIO 6
-- ============================================================

-- Decidimos usar recCueva y no foldCueva ya que se debe inspeccionar el hijo directo de cada Compuerta antes de combinar el resultado recursivo.
esBienFormada :: Cueva -> Bool
esBienFormada =
  recCueva
    (const True)
    (\_ _ rec -> rec)
    (\_ subCueva rec -> not (esCompuerta subCueva) && rec)
    (\_ _ recIzq _ recDer -> recIzq && recDer)

esCompuerta :: Cueva -> Bool
esCompuerta (Compuerta _ _) = True
esCompuerta _ = False

-- ============================================================
-- EJERCICIO 7
-- ============================================================

trasComprimirCompuertas :: Cueva -> Cueva
trasComprimirCompuertas =
  recCueva
    (\x -> Salida x)
    (\x y rec -> Depósito x rec)
    ( \n _ rec -> case rec of
        Compuerta m z -> Compuerta (\c -> n c && m c) z
        _ -> Compuerta n rec
    )
    (\f _ reci _ recd -> Bifurcación f reci recd)

-- ============================================================
-- EJERCICIO 8
-- ============================================================

{-
El fold devuelve Carga -> Maybe Carga, no el resultado final: 
- primero se recorre la cueva
- recien al final se aplica esa funcion a la carga inicial.
En bifurcaciones, la carga que queda al terminar la primera rama es la que usa la segunda.
(Para ello usamos la definicion de Bifurcacion del enunciado: la carga con la que termina la primera rama es la carga con la que se entra a la segunda).
-}
cargaRestante :: Cueva -> Carga -> Maybe Carga
cargaRestante =
  foldCueva
    ( \alt ->
        (\carga -> if carga >= alt then Just (carga - alt) else Nothing)
    )
    ( \cristales rec ->
        (\carga -> rec (carga + cristales))
    )
    ( \pred rec ->
        (\carga -> if pred carga then rec carga else Nothing)
    )
    ( \dir recIzq recDer ->
        ( \carga -> case dir carga of
            Izq -> case recIzq carga of
              Nothing -> Nothing
              Just c' -> recDer c'
            Der -> case recDer carga of
              Nothing -> Nothing
              Just c' -> recIzq c'
        )
    )

-- ============================================================
-- EJERCICIO 9
-- ============================================================
{-
Mientras recorremos nos llevamos dos cosas: pasos visitados (s) y carga (c).
k es la continuacion de exito: si el resto de la exploracion termina bien, responde Nothing; si algo falla antes, devolvemos Just con los pasos hasta ahi.
Es por eso que Salida al tener exito llama k (s+1) (...), pero al quedarse sin carga responde Just (s+1) contando ese nodo como ultimo alcanzado.
-}

pasosHastaAtrapado :: Cueva -> Carga -> Maybe Int
pasosHastaAtrapado cueva carga =
  foldCueva
    (\h s c k -> if c >= h then k (s + 1) (c - h) else Just (s + 1))
    (\cristales rec s c k -> rec (s + 1) (c + cristales) k)
    (\p rec s c k -> if p c then rec (s + 1) c k else Just (s + 1))
    ( \d recI recD s c k -> case d c of
        Izq -> recI (s + 1) c (\sI cI -> recD sI cI k)
        Der -> recD (s + 1) c (\sD cD -> recI sD cD k)
    )
    cueva
    0
    carga
    (\_ _ -> Nothing)

-- ============================================================
-- TESTS
-- ============================================================

-- Para correr los tests: runTestTT tests
-- Se requiere el paquete HUnit (cabal install HUnit)

-- Cuevas auxiliares para los tests

cuevaSimple :: Cueva
cuevaSimple = Depósito 10 (Salida 3)

cuevaProfunda :: Cueva
cuevaProfunda = Depósito 1 (Depósito 2 (Depósito 3 (Salida 0)))

cuevaCompleja1 :: Cueva
cuevaCompleja1 =
  Bifurcación
    (\c -> if c > 50 then Izq else Der)
    (Depósito 20 (Compuerta (> 20) (Salida 10)))
    (Compuerta (< 100) (Bifurcación (const Der) (Salida 5) (Depósito 5 (Salida 5))))

cuevaMalFormada1 :: Cueva
cuevaMalFormada1 = Depósito 5 (Compuerta (> 10) (Compuerta (< 20) (Salida 0)))

cuevaCompuertasAnidadas :: Cueva
cuevaCompuertasAnidadas =
  Compuerta (const True) (Compuerta (const False) (Salida 0))

cuevaAComprimir :: Cueva
cuevaAComprimir =
  Bifurcación
    (const Izq)
    (Compuerta (> 0) (Compuerta (< 10) (Compuerta (== 5) (Salida 0))))
    (Depósito 5 (Compuerta (> 2) (Compuerta (< 8) (Salida 2))))

cuevaEstadoViajero :: Cueva
cuevaEstadoViajero =
  Bifurcación
    (const Izq)
    (Depósito 10 (Salida 5))
    (Compuerta (> 15) (Salida 10))

cuevaDobleBifurcacion :: Cueva
cuevaDobleBifurcacion =
  Bifurcación
    (const Izq)
    (Salida 0)
    ( Bifurcación
        (const Izq)
        (Depósito 0 (Salida 0))
        (Compuerta (const False) (Salida 0))
    )

cuevaBifurcacionesSimetricas :: Cueva
cuevaBifurcacionesSimetricas =
  Bifurcación
    (const Izq)
    ( Bifurcación
        (const Izq)
        (Salida 0)
        (Salida 0)
    )
    ( Bifurcación
        (const Izq)
        (Salida 0)
        (Compuerta (const False) (Salida 0))
    )

cuevaPrimeraRamaFalla :: Cueva
cuevaPrimeraRamaFalla =
  Bifurcación
    (const Izq)
    (Compuerta (const False) (Salida 0))
    (Depósito 100 (Salida 0))

cuevaCompuertasSeparadasPorDeposito :: Cueva
cuevaCompuertasSeparadasPorDeposito =
  Compuerta (const True) (Depósito 5 (Compuerta (const True) (Salida 0)))

cuevaCompuertasEnRamaDeBifurcacion :: Cueva
cuevaCompuertasEnRamaDeBifurcacion =
  Compuerta
    (const True)
    ( Bifurcación
        (const Izq)
        (Compuerta (const True) (Compuerta (const True) (Salida 0)))
        (Salida 0)
    )

cuevaCompuertasNoConsecutivas :: Cueva
cuevaCompuertasNoConsecutivas =
  Compuerta (> 0) (Depósito 5 (Compuerta (< 10) (Salida 0)))

cuevaOrdenDerImporta :: Cueva
cuevaOrdenDerImporta =
  Bifurcación
    (const Der)
    (Salida 50)
    (Depósito 30 (Salida 0))

cuevaCompuertasConsecutivasTrasDeposito :: Cueva
cuevaCompuertasConsecutivasTrasDeposito =
  Depósito 5 (Compuerta (> 10) (Compuerta (< 20) (Salida 0)))

cuevaBifurcacionCompuertasOk :: Cueva
cuevaBifurcacionCompuertasOk =
  Bifurcación
    (const Izq)
    (Compuerta (const True) (Salida 0))
    (Compuerta (const True) (Salida 0))

cuevaBifurcacionHojaVsProfunda :: Cueva
cuevaBifurcacionHojaVsProfunda =
  Bifurcación
    (const Izq)
    (Salida 0)
    (Depósito 2 (Depósito 3 (Salida 0)))

---------- Tests Ejercicio 1 y 2: foldCueva / recCueva ------
testsEsquemas :: Test
testsEsquemas =
  TestList
    [ "suma los cristales de un deposito simple"
        ~: foldCueva (const 0) (+) (const id) (\_ i d -> i + d) cuevaSimple
        ~?= 10,
      "recCueva cuenta nodos en cadena de depositos"
        ~: recCueva (const 1) (\_ _ rec -> 1 + rec) (\_ _ rec -> 1 + rec) (\_ _ recI _ recD -> 1 + recI + recD) cuevaProfunda
        ~?= 4,
      "fold y rec coinciden en una salida"
        ~: foldCueva (const 1) (\_ r -> 1 + r) (\_ r -> 1 + r) (\_ i d -> 1 + i + d) (Salida 0)
        ~?= recCueva (const 1) (\_ _ r -> 1 + r) (\_ _ r -> 1 + r) (\_ _ i _ d -> 1 + i + d) (Salida 0),
      "fold y rec coinciden en un deposito"
        ~: foldCueva (const 1) (\_ r -> 1 + r) (\_ r -> 1 + r) (\_ i d -> 1 + i + d) (Depósito 3 (Salida 0))
        ~?= recCueva (const 1) (\_ _ r -> 1 + r) (\_ _ r -> 1 + r) (\_ _ i _ d -> 1 + i + d) (Depósito 3 (Salida 0)),
      "fold cuenta compuerta y salida"
        ~: foldCueva (const 1) (\_ r -> 1 + r) (\_ r -> 1 + r) (\_ i d -> 1 + i + d) (Compuerta (const True) (Salida 0))
        ~?= 2,
      "fold cuenta una bifurcacion chica"
        ~: foldCueva (const 1) (\_ r -> 1 + r) (\_ r -> 1 + r) (\_ i d -> 1 + i + d) (Bifurcación (const Izq) (Salida 0) (Salida 0))
        ~?= 3,
      "fold suma depositos de las dos ramas"
        ~: foldCueva (const 0) (+) (const id) (\_ i d -> i + d)
            (Bifurcación (const Izq) (Depósito 3 (Salida 0)) (Depósito 7 (Salida 0)))
        ~?= 10
    ]

---------- Tests Ejercicio 3: cantidadNodos --------------
testsCantidadNodos :: Test
testsCantidadNodos =
  TestList
    [ "solo una salida" ~: cantidadNodos (Salida 5) ~?= 1,
      "deposito y salida debajo" ~: cantidadNodos (Depósito 10 (Salida 3)) ~?= 2,
      "ejemplo del enunciado con bifurcacion" ~: cantidadNodos (Bifurcación (const Izq) (Salida 1) (Depósito 2 (Salida 3))) ~?= 4,
      "cueva grande de prueba" ~: cantidadNodos cuevaCompleja1 ~?= 9,
      "compuerta con salida" ~: cantidadNodos (Compuerta (const True) (Salida 0)) ~?= 2,
      "bifurcacion con dos salidas nomas" ~: cantidadNodos (Bifurcación (const Izq) (Salida 0) (Salida 0)) ~?= 3,
      "dos compuertas seguidas" ~: cantidadNodos (Compuerta (const True) (Compuerta (const True) (Salida 0))) ~?= 3,
      "una rama corta y otra mas larga" ~: cantidadNodos cuevaBifurcacionHojaVsProfunda ~?= 5
    ]

---------- Tests Ejercicio 4: profundidad -------------------
testsProfundidad :: Test
testsProfundidad =
  TestList
    [ "salida sola tiene profundidad 1"
        ~: profundidad (Salida 5)
        ~?= 1,
      "deposito suma un nivel"
        ~: profundidad (Depósito 10 (Salida 3))
        ~?= 2,
      "bifurcacion elige la rama mas profunda"
        ~: profundidad (Bifurcación (const Izq) (Salida 1) (Depósito 2 (Salida 3)))
        ~?= 3,
      "cadena larga de depositos"
        ~: profundidad cuevaProfunda
        ~?= 4,
      "cueva grande de prueba"
        ~: profundidad cuevaCompleja1
        ~?= 5,
      "ramas con distinta profundidad"
        ~: profundidad (Bifurcación (const Izq) (Salida 0) (Depósito 2 (Salida 3)))
        ~?= 3,
      "dos ramas iguales y chicas"
        ~: profundidad (Bifurcación (const Izq) (Salida 0) (Salida 0))
        ~?= 2,
      "misma profundidad pero distinta forma"
        ~: profundidad (Bifurcación (const Izq) (Depósito 2 (Salida 3)) (Depósito 4 (Salida 5)))
        ~?= 3,
      "compuertas seguidas tambien suman niveles"
        ~: profundidad (Compuerta (const True) (Compuerta (const True) (Salida 0)))
        ~?= 3,
      "bifurcacion arriba de una cadena larga"
        ~: profundidad (Bifurcación (const Izq) (Salida 0) (Depósito 1 (Depósito 2 (Depósito 3 (Salida 0)))))
        ~?= 5
    ]

---------- Tests Ejercicio 5: cristalesDeDepositos --------------
testsCristalesDeDepositos :: Test
testsCristalesDeDepositos =
  TestList
    [ "salida sola no tiene depositos" ~: cristalesDeDepositos (Salida 0) ~?= [],
      "un deposito devuelve una lista" ~: cristalesDeDepositos (Depósito 5 (Salida 0)) ~?= [5],
      "ejemplo del enunciado respeta orden izq-der"
        ~: cristalesDeDepositos (Bifurcación (const Der) (Depósito 3 (Salida 0)) (Depósito 7 (Salida 0)))
        ~?= [3, 7],
      "cueva grande de prueba" ~: cristalesDeDepositos cuevaCompleja1 ~?= [20, 5],
      "depositos solo en la rama derecha"
        ~: cristalesDeDepositos (Bifurcación (const Der) (Salida 0) (Depósito 7 (Salida 0)))
        ~?= [7],
      "depositos solo en la rama izquierda"
        ~: cristalesDeDepositos (Bifurcación (const Der) (Depósito 3 (Salida 0)) (Salida 0))
        ~?= [3],
      "deposito vacio igual aparece en la lista"
        ~: cristalesDeDepositos (Depósito 0 (Salida 0))
        ~?= [0],
      "compuerta en el medio no cambia la lista"
        ~: cristalesDeDepositos (Depósito 5 (Compuerta (const True) (Depósito 3 (Salida 0))))
        ~?= [5, 3],
      "varios depositos en fila"
        ~: cristalesDeDepositos cuevaProfunda
        ~?= [1, 2, 3]
    ]

---------- Tests Ejercicio 6: esBienFormada -----------------
testsEsBienFormada :: Test
testsEsBienFormada =
  TestList
    [ "salida sola esta bien" ~: esBienFormada (Salida 0) ~?= True,
      "una compuerta sobre salida esta bien" ~: esBienFormada (Compuerta (const True) (Salida 0)) ~?= True,
      "dos compuertas seguidas no estan bien" ~: esBienFormada (Compuerta (const True) (Compuerta (const True) (Salida 0))) ~?= False,
      "tres compuertas seguidas tampoco"
        ~: esBienFormada (Compuerta (const True) (Compuerta (const True) (Compuerta (const True) (Salida 0))))
        ~?= False,
      "mal formada: compuertas seguidas en una rama de bifurcacion"
        ~: esBienFormada (Bifurcación (const Izq) (Compuerta (const True) (Salida 0)) (Compuerta (const True) (Compuerta (const True) (Salida 0))))
        ~?= False,
      "mal formada: compuertas seguidas adentro de una rama"
        ~: esBienFormada cuevaCompuertasEnRamaDeBifurcacion
        ~?= False,
      "bien formada: compuertas separadas por un deposito"
        ~: esBienFormada cuevaCompuertasSeparadasPorDeposito
        ~?= True,
      "bien formada: una compuerta por rama"
        ~: esBienFormada cuevaBifurcacionCompuertasOk
        ~?= True,
      "mal formada: compuertas seguidas despues de un deposito"
        ~: esBienFormada cuevaMalFormada1
        ~?= False,
      "cueva grande de prueba esta bien formada"
        ~: esBienFormada cuevaCompleja1
        ~?= True
    ]

---------- Tests Ejercicio 7: trasComprimirCompuertas -----------
testsTrasComprimirCompuertas :: Test
testsTrasComprimirCompuertas =
  TestList
    [ "despues de comprimir queda bien formada (compuertas anidadas)"
        ~: esBienFormada (trasComprimirCompuertas cuevaCompuertasAnidadas)
        ~?= True,
      "despues de comprimir queda bien formada (despues de deposito)"
        ~: esBienFormada (trasComprimirCompuertas cuevaMalFormada1)
        ~?= True,
      "despues de comprimir queda bien formada (varias ramas)"
        ~: esBienFormada (trasComprimirCompuertas cuevaAComprimir)
        ~?= True,
      "ejemplo del enunciado: una sola compuerta no cambia"
        ~: case trasComprimirCompuertas (Compuerta (const False) (Salida 7)) of
             Compuerta p (Salida 7) -> p 0 ~?= False
             _ -> TestCase (assertFailure "Debe quedar una compuerta sobre Salida 7"),
      "junta dos compuertas en un solo predicado"
        ~: let comp = trasComprimirCompuertas (Compuerta (> 10) (Compuerta (< 15) (Salida 4)))
            in case comp of
                 Compuerta p (Salida 4) -> (p 12, p 10, p 15) ~?= (True, False, False)
                 _ -> TestCase (assertFailure "Debe comprimir en una compuerta"),
      "junta tres compuertas seguidas"
        ~: let comp = trasComprimirCompuertas (Compuerta (> 0) (Compuerta (< 10) (Compuerta (== 5) (Salida 0))))
            in case comp of
                 Compuerta p _ -> (p 5, p 2, p 12) ~?= (True, False, False)
                 _ -> TestCase (assertFailure "Debe ser compuerta"),
      "comprime por separado en cada rama"
        ~: let comp = trasComprimirCompuertas cuevaAComprimir
            in case comp of
                 Bifurcación _ (Compuerta p1 _) (Depósito _ (Compuerta p2 _)) ->
                   (p1 5, p1 2, p2 5, p2 10) ~?= (True, False, True, False)
                 _ -> TestCase (assertFailure "Estructura incorrecta"),
      "deposito sin compuertas queda igual"
        ~: case trasComprimirCompuertas (Depósito 10 (Salida 3)) of
             Depósito 10 (Salida 3) -> True ~?= True
             _ -> TestCase (assertFailure "Deposito debe quedar igual"),
      "bifurcacion sin compuertas seguidas queda igual"
        ~: case trasComprimirCompuertas (Bifurcación (const Izq) (Salida 1) (Depósito 2 (Salida 3))) of
             Bifurcación _ (Salida 1) (Depósito 2 (Salida 3)) -> True ~?= True
             _ -> TestCase (assertFailure "Bifurcacion debe quedar igual"),
      "no junta compuertas si hay un deposito en el medio"
        ~: case trasComprimirCompuertas cuevaCompuertasNoConsecutivas of
             Compuerta p (Depósito 5 (Compuerta q (Salida 0))) ->
               (p 5, q 5, p 15, q 15) ~?= (True, True, True, False)
             _ -> TestCase (assertFailure "Deposito debe separar compuertas"),
      "si compuertas seguidas estan abajo de un deposito, comprime solo esas"
        ~: let comp = trasComprimirCompuertas cuevaCompuertasConsecutivasTrasDeposito
            in case comp of
                 Depósito 5 (Compuerta p (Salida 0)) -> (p 15, p 25, p 5) ~?= (True, False, False)
                 _ -> TestCase (assertFailure "Debe fusionar solo las compuertas internas")
    ]

---------- Tests Ejercicio 8: cargaRestante -----------------
testsCargaRestante :: Test
testsCargaRestante =
  TestList
    [ "ejemplo del enunciado: le alcanza la carga" ~: cargaRestante (Salida 11) 13 ~?= Just 2,
      "le alcanza justo" ~: cargaRestante (Salida 11) 11 ~?= Just 0,
      "no le alcanza para salir" ~: cargaRestante (Salida 11) 5 ~?= Nothing,
      "deposito recarga antes de salir" ~: cargaRestante (Depósito 10 (Salida 3)) 100 ~?= Just 107,
      "compuerta bloqueante lo frena" ~: cargaRestante (Compuerta (const False) (Salida 0)) 500 ~?= Nothing,
      "pasa la compuerta y sigue" ~: cargaRestante (Compuerta (> 10) (Salida 0)) 15 ~?= Just 15,
      "la carga de la primera rama llega a la segunda" ~: cargaRestante cuevaEstadoViajero 12 ~?= Just 7,
      "sin carga suficiente queda atrapado en la segunda rama" ~: cargaRestante cuevaEstadoViajero 2 ~?= Nothing,
      "cueva grande: se queda sin carga" ~: cargaRestante cuevaCompleja1 120 ~?= Nothing,
      "cueva grande: termina bien" ~: cargaRestante cuevaCompleja1 40 ~?= Just 45,
      "arranca con cero y la salida es cero" ~: cargaRestante (Salida 0) 0 ~?= Just 0,
      "arranca con cero y no le alcanza" ~: cargaRestante (Depósito 5 (Salida 10)) 0 ~?= Nothing,
      "salida con altura negativa suma carga" ~: cargaRestante (Salida (-5)) 0 ~?= Just 5,
      "carga inicial negativa no sirve" ~: cargaRestante (Salida 3) (-1) ~?= Nothing,
      "pasa compuerta pero no llega a la salida" ~: cargaRestante (Compuerta (const True) (Salida 100)) 50 ~?= Nothing,
      "deposito de cero no cambia nada" ~: cargaRestante (Depósito 0 (Salida 5)) 5 ~?= Just 0,
      "bifurcacion: recarga en izq y consume en der"
        ~: cargaRestante (Bifurcación (const Izq) (Depósito 10 (Salida 0)) (Salida 5)) 0
        ~?= Just 5,
      "bifurcacion: el orden Der importa"
        ~: cargaRestante cuevaOrdenDerImporta 40
        ~?= Just 20,
      "si la primera rama falla, no sigue"
        ~: cargaRestante cuevaPrimeraRamaFalla 0
        ~?= Nothing
    ]

---------- Tests Ejercicio 9: pasosHastaAtrapado ------------
testsPasosHastaAtrapado :: Test
testsPasosHastaAtrapado =
  TestList
    [ "ejemplo del enunciado: termina sin quedar atrapado" ~: pasosHastaAtrapado (Salida 2) 50 ~?= Nothing,
      "se queda sin carga en la salida" ~: pasosHastaAtrapado (Depósito 10 (Salida 120)) 100 ~?= Just 2,
      "compuerta lo frena antes de seguir" ~: pasosHastaAtrapado (Depósito 3 (Compuerta (> 60) (Depósito 9 (Salida 6)))) 50 ~?= Just 2,
      "pasa compuerta pero no llega a salir" ~: pasosHastaAtrapado (Compuerta (< 100) (Salida 75)) 50 ~?= Just 2,
      "ejemplo del enunciado: atrapado en la segunda rama" ~: pasosHastaAtrapado (Bifurcación (const Izq) (Depósito 5 (Salida 0)) (Compuerta (const False) (Salida 0))) 50 ~?= Just 4,
      "cuenta bien los pasos cuando la carga viaja" ~: pasosHastaAtrapado cuevaEstadoViajero 2 ~?= Just 4,
      "con carga suficiente recorre todo" ~: pasosHastaAtrapado cuevaEstadoViajero 12 ~?= Nothing,
      "cueva grande: atrapado en rama derecha" ~: pasosHastaAtrapado cuevaCompleja1 120 ~?= Just 5,
      "cueva grande: recorre todo" ~: pasosHastaAtrapado cuevaCompleja1 40 ~?= Nothing,
      "recarga en izq y compuerta en der deja pasar"
        ~: pasosHastaAtrapado (Bifurcación (const Izq) (Depósito 10 (Salida 0)) (Compuerta (>= 10) (Salida 0))) 0
        ~?= Nothing,
      "dos bifurcaciones seguidas: cuenta hasta la trampa" ~: pasosHastaAtrapado cuevaDobleBifurcacion 0 ~?= Just 6,
      "bifurcaciones simetricas: cuenta hasta la trampa" ~: pasosHastaAtrapado cuevaBifurcacionesSimetricas 0 ~?= Just 7,
      "compuerta en la raiz frena al toque" ~: pasosHastaAtrapado (Compuerta (const False) (Salida 0)) 100 ~?= Just 1,
      "salida en la raiz sin carga suficiente" ~: pasosHastaAtrapado (Salida 100) 50 ~?= Just 1,
      "bifurcacion chica: recorre las dos ramas" ~: pasosHastaAtrapado (Bifurcación (const Izq) (Salida 0) (Salida 0)) 10 ~?= Nothing,
      "bifurcacion Der: cuenta distinto el orden de pasos"
        ~: pasosHastaAtrapado (Bifurcación (const Der) (Compuerta (const False) (Salida 0)) (Salida 0)) 50
        ~?= Just 3,
      "bifurcacion Der: puede terminar todo"
        ~: pasosHastaAtrapado cuevaOrdenDerImporta 40
        ~?= Nothing,
      "si la primera rama falla, corta ahi"
        ~: pasosHastaAtrapado cuevaPrimeraRamaFalla 0
        ~?= Just 2,
      "bifurcaciones anidadas: llega al final"
        ~: pasosHastaAtrapado
            ( Bifurcación
                (const Izq)
                (Salida 0)
                ( Bifurcación
                    (const Izq)
                    (Salida 0)
                    (Salida 0)
                )
            )
            5
        ~?= Nothing
    ]

---------- Suite completa -----------------------------------
tests :: Test
tests =
  TestList
    [ TestLabel "Esquemas" testsEsquemas,
      TestLabel "CantidadNodos" testsCantidadNodos,
      TestLabel "Profundidad" testsProfundidad,
      TestLabel "CristalesDeDepositos" testsCristalesDeDepositos,
      TestLabel "EsBienFormada" testsEsBienFormada,
      TestLabel "TrasComprimirCompuertas" testsTrasComprimirCompuertas,
      TestLabel "CargaRestante" testsCargaRestante,
      TestLabel "PasosHastaAtrapado" testsPasosHastaAtrapado
    ]
