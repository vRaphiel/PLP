module Main (main) where

import App
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Expr
import Expr.Parser
import GHC.Float (isInfinite)
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

-- AUXILIAR: Tolerancia global para comparaciones aproximadas
epsilon :: Float
epsilon = 1e-6

infix 4 ~?~
(~?~) :: (HasCallStack) => Float -> Float -> Test
actual ~?~ esperado =
  TestCase (assertBool ("Esperaba ≈ " ++ show esperado ++ ", obtuve " ++ show actual)
                        (abs (actual - esperado) < epsilon))
---

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [       -- Tests básicos
      "alinearDerecha_con_cadena_vacia" ~: alinearDerecha 5 "" ~?= "     ",
      "alinearDerecha_sin_cambios" ~: alinearDerecha 5 "hola mundo" ~?= "hola mundo",
      "alinearDerecha_con_espacios" ~: alinearDerecha 6 " hola" ~?= "  hola",
      "alinearDerecha_longitud_exacta" ~: alinearDerecha 6 "exacto" ~?= "exacto",
      "alinearDerecha_con_caracteres_especiales" ~: alinearDerecha 8 "¡Hola!" ~?= "  ¡Hola!",
      "alinearDerecha_con_numeros" ~: alinearDerecha 7 "12345" ~?= "  12345",

      -- Tests de casos límite
      "alinearDerecha_n_cero" ~: alinearDerecha 0 "hola" ~?= "hola",
      "alinearDerecha_n_negativo" ~: alinearDerecha (-3) "hola" ~?= "hola",
      "alinearDerecha_con_unicode" ~: alinearDerecha 6 "αβγ" ~?= "   αβγ",

      -- Tests de propiedades
      "propiedad_longitud_resultado" ~: let s = "test"; n = 10; resultado = alinearDerecha n s
                                          in length resultado ~?= max n (length s),
      "propiedad_prefijo_espacios" ~: let s = "abc"; n = 6; resultado = alinearDerecha n s
                                        in TestList [ take (n - length s) resultado ~?= replicate (n - length s) ' ',
                                                      drop (n - length s) resultado ~?= s ]
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ -- Tests básicos
      "actualizarElem_en_posicion_valida" ~: actualizarElem 1 (+10) [0, 1, 2, 3, 4] ~?= [0, 11, 2, 3, 4],
      "actualizarElem_primer_elemento" ~: actualizarElem 0 (*100) [1, 1, 2, 3] ~?= [100, 1, 2, 3],
      "actualizarElem_ultimo_elemento" ~: actualizarElem 3 (+1) [1, 2, 3, 41] ~?= [1, 2, 3, 42],
      "actualizarElem_con_funcion_string" ~: actualizarElem 1 (map toUpper) ["a", "hello", "c"] ~?= ["a", "HELLO", "c"],

      -- Tests con índices fuera de límites
      "actualizarElem_indice_negativo" ~: actualizarElem (-1) (+10) [1, 2, 3] ~?= [1, 2, 3],
      "actualizarElem_indice_muy_grande" ~: actualizarElem 10 (+10) [1, 2, 3] ~?= [1, 2, 3],
      "actualizarElem_lista_vacia" ~: actualizarElem 0 (+1) [] ~?= [],

      -- Tests con diferentes tipos
      "actualizarElem_con_booleanos" ~: actualizarElem 1 not [True, True, True] ~?= [True, False, True],
      "actualizarElem_con_floats" ~: actualizarElem 1 (*2) [1.0, 1.75, 3.0] ~?= [1.0, 3.5, 3.0],
      "actualizarElem_con_tuplas" ~: actualizarElem 1 (\(a,b) -> (b,a)) [(1,2), (3,4), (3,4)] ~?= [(1,2), (4,3), (3,4)],

      -- Tests de propiedades
      "propiedad_longitud_invariante" ~: let xs = [1, 2, 3, 4, 5]; resultado = actualizarElem 2 (+1) xs
                                           in length resultado ~?= length xs,
      "propiedad_elementos_no_afectados" ~: let xs = [10, 20, 30, 40, 50]; resultado = actualizarElem 2 (+100) xs
                                              in TestList [ resultado !! 0 ~?= 10,
                                                            resultado !! 1 ~?= 20,
                                                            resultado !! 3 ~?= 40,
                                                            resultado !! 4 ~?= 50 ],
      "propiedad_solo_elemento_objetivo_afectado" ~: let xs = [1, 1, 1, 1, 1]; resultado = actualizarElem 2 (+5) xs
                                                       in TestList [ resultado !! 2 ~?= 6,
                                                                     take 2 resultado ++ drop 3 resultado ~?= [1, 1, 1, 1] ],

      -- Tests con funciones complejas
      "actualizarElem_con_funcion_compleja" ~: actualizarElem 1 (++ [30]) [[1,2], [10,20], [3,4]] ~?= [[1,2], [10,20,30], [3,4]],
      "actualizarElem_con_funcion_condicional" ~: actualizarElem 1 (\x -> if x > 5 then x*10 else x) [1, 10, 3, 4] ~?= [1, 100, 3, 4]
    ]

testsVacio :: Test
testsVacio =
  test
    [ -- Tests básicos existentes
      casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],

      -- Caso borde: n = 1 (mínimo permitido)
      "vacio_n_minimo" ~: casilleros (vacio 1 (5, 15))
        ~?= [ Casillero infinitoNegativo 5 0 0,
              Casillero 5 15 0 0,
              Casillero 15 infinitoPositivo 0 0
            ],

      -- Caso: rango muy pequeño
      -- Usamos los observadores y tolerancia para evitar problemas de precisión en punto flotante
      -- en los límites de los casilleros
      "vacio_rango_minimo" ~: let cs = casilleros (vacio 2 (0.1, 0.2))
                               in TestList [ length cs ~?= 4,
                                             casMinimo (cs !! 0) ~?= infinitoNegativo,
                                             casMaximo (cs !! 0) ~?~ 0.1,
                                             casMinimo (cs !! 1) ~?~ 0.1,
                                             casMaximo (cs !! 1) ~?~ 0.15,
                                             casMinimo (cs !! 2) ~?~ 0.15,
                                             casMaximo (cs !! 2) ~?~ 0.2,
                                             casMinimo (cs !! 3) ~?~ 0.2,
                                             casMaximo (cs !! 3) ~?= infinitoPositivo,
                                             all (\c -> casCantidad c == 0 && casPorcentaje c == 0) cs ~?= True
                                           ],

      "vacio_rango_negativo" ~: casilleros (vacio 2 (-10, -5))
        ~?= [ Casillero infinitoNegativo (-10) 0 0,
              Casillero (-10) (-7.5) 0 0,
              Casillero (-7.5) (-5) 0 0,
              Casillero (-5) infinitoPositivo 0 0
            ],

      "vacio_rango_cero" ~: casilleros (vacio 1 (5, 5))
        ~?= [ Casillero infinitoNegativo 5 0 0,
              Casillero 5 5 0 0,
              Casillero 5 infinitoPositivo 0 0
            ],


      "vacio_muchos_casilleros" ~: let cs = casilleros (vacio 100 (0, 10))
      in TestList [ length cs ~?= 102,  -- 100 + 2 extremos
                    all (\c -> casCantidad c == 0 && casPorcentaje c == 0) cs ~?= True,
                    casMinimo (cs !! 0) ~?= infinitoNegativo,
                    casMaximo (cs !! 0) ~?= 0,
                    casMinimo (cs !! 101) ~?= 10,
                    casMaximo (cs !! 101) ~?= infinitoPositivo ],

      "vacio_decimales" ~: casilleros (vacio 3 (1.5, 4.7))
        ~?= [ Casillero infinitoNegativo 1.5 0 0,
              Casillero 1.5 (1.5 + (4.7-1.5)/3) 0 0,
              Casillero (1.5 + (4.7-1.5)/3) (1.5 + 2*(4.7-1.5)/3) 0 0,
              Casillero (1.5 + 2*(4.7-1.5)/3) 4.7 0 0,
              Casillero 4.7 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ -- Tests básicos existentes
          casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],

          -- Tests exhaustivos en casos
          "agregar_limite_inferior_exacto" ~:
            casilleros (agregar 0 (vacio 2 (0, 4)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 1 100,
                    Casillero 2 4 0 0,
                    Casillero 4 infinitoPositivo 0 0
                  ],

          "agregar_limite_superior_exacto" ~:
            casilleros (agregar 6 (vacio 3 (0, 6)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 0 0,
                    Casillero 2 4 0 0,
                    Casillero 4 6 0 0,
                    Casillero 6 infinitoPositivo 1 100
                  ],

          "agregar_punto_medio" ~:
            casilleros (agregar 3 (vacio 2 (0, 6)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 3 0 0,
                    Casillero 3 6 1 100,
                    Casillero 6 infinitoPositivo 0 0
                  ],

          "agregar_negativo_grande" ~:
            casilleros (agregar (-1000) (vacio 2 (0, 10)))
              ~?= [ Casillero infinitoNegativo 0 1 100,
                    Casillero 0 5 0 0,
                    Casillero 5 10 0 0,
                    Casillero 10 infinitoPositivo 0 0
                  ],

          "agregar_positivo_grande" ~:
            casilleros (agregar 1000 (vacio 2 (0, 10)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 5 0 0,
                    Casillero 5 10 0 0,
                    Casillero 10 infinitoPositivo 1 100
                  ],

          "agregar_precision_alta" ~:
            casilleros (agregar 2.999999 (vacio 3 (0, 6)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 0 0,
                    Casillero 2 4 1 100,
                    Casillero 4 6 0 0,
                    Casillero 6 infinitoPositivo 0 0
                  ],

          "agregar_multiples_mismo_valor" ~:
            casilleros (agregar 2.5 (agregar 2.5 (agregar 2.5 (vacio 2 (0, 5)))))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2.5 0 0,
                    Casillero 2.5 5 3 100,
                    Casillero 5 infinitoPositivo 0 0
                  ],

          "agregar_diferentes_casilleros" ~:
            casilleros (agregar 0.5 (agregar 2.5 (agregar 4.5 (vacio 3 (0, 6)))))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 1 (100/3),
                    Casillero 2 4 1 (100/3),
                    Casillero 4 6 1 (100/3),
                    Casillero 6 infinitoPositivo 0 0
                  ],

          "agregar_cercano_limite_inferior" ~:
            casilleros (agregar 1e-10 (vacio 2 (0, 4)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 1 100,
                    Casillero 2 4 0 0,
                    Casillero 4 infinitoPositivo 0 0
                  ],

          "agregar_cercano_limite_superior" ~:
            casilleros (agregar (6 - epsilon) (vacio 3 (0, 6)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 2 0 0,
                    Casillero 2 4 0 0,
                    Casillero 4 6 1 100,
                    Casillero 6 infinitoPositivo 0 0
                  ],

          "agregar_rango_negativo" ~:
            casilleros (agregar (-5) (vacio 2 (-10, 0)))
              ~?= [ Casillero infinitoNegativo (-10) 0 0,
                    Casillero (-10) (-5) 0 0,
                    Casillero (-5) 0 1 100,
                    Casillero 0 infinitoPositivo 0 0
                  ],

          "agregar_notacion_cientifica" ~:
            casilleros (agregar epsilon (vacio 2 (0, 1e-3)))
              ~?= [ Casillero infinitoNegativo 0 0 0,
                    Casillero 0 5e-4 1 100,
                    Casillero 5e-4 1e-3 0 0,
                    Casillero 1e-3 infinitoPositivo 0 0
                  ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ -- Test básico existente
      histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),

      -- Tests exhaustivos adicionales
      -- Caso borde: lista vacía
      "histograma_lista_vacia" ~:
        let h = histograma 3 (0, 10) []
            cs = casilleros h
         in all (\c -> casCantidad c == 0 && casPorcentaje c == 0) cs ~?= True,

      -- Caso borde: un solo elemento
      "histograma_un_elemento" ~:
        let h = histograma 2 (0, 4) [2.5]
            cs = casilleros h
         in TestList [ casCantidad (cs !! 2) ~?= 1,  -- [2, 4)
                       casPorcentaje (cs !! 2) ~?= 100 ],

      -- Caso borde: muchos elementos idénticos
      "histograma_elementos_identicos" ~:
        let h = histograma 3 (0, 9) (replicate 100 3)
            cs = casilleros h
         in TestList [ casCantidad (cs !! 2) ~?= 100,  -- [3, 6)
                       casPorcentaje (cs !! 2) ~?= 100 ],

      -- Caso borde: elementos distribuidos uniformemente
      "histograma_distribucion_uniforme" ~:
        let valores = [0.5, 1.5, 2.5, 3.5, 4.5]
            h = histograma 5 (0, 5) valores
            cs = casilleros h
         in all (\c -> casCantidad c == 1) (drop 1 (take 6 cs)) ~?= True,  -- Casilleros centrales

      -- Caso borde: valores fuera del rango
      "histograma_valores_fuera_rango" ~:
        let valores = [-5, 15, 25]
            h = histograma 2 (0, 10) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 0) ~?= 1,  -- (-∞, 0)
                       casCantidad (cs !! 3) ~?= 2 ], -- [10, +∞)

      -- Caso borde: números decimales de alta precisión
      "histograma_precision_alta" ~:
        let valores = [1.0000001, 1.9999999, 2.0000001]
            h = histograma 2 (1, 3) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 2,  -- [1, 2)
                       casCantidad (cs !! 2) ~?= 1 ], -- [2, 3)

      -- Caso borde: números muy grandes
      "histograma_numeros_grandes" ~:
        let valores = [1e15, 2e15, 3e15]
            h = histograma 2 (0, 4e15) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 1,  -- [0, 2e15)
                       casCantidad (cs !! 2) ~?= 2 ], -- [2e15, 4e15)

      -- Caso borde: números muy pequeños
      "histograma_numeros_pequenos" ~:
        let valores = [1e-10, 2e-10, 3e-10]
            h = histograma 2 (0, 4e-10) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 1,  -- [0, 2e-10)
                       casCantidad (cs !! 2) ~?= 2 ], -- [2e-10, 4e-10)

      "histograma_positivos_negativos" ~:
        let valores = [-10, -5, 0, 5, 10]
            h = histograma 4 (-10, 10) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 1,  -- (-∞, -10) 
                       casCantidad (cs !! 2) ~?= 1,  -- [-5, 0)
                       casCantidad (cs !! 3) ~?= 1,  -- [0, 5)
                       casCantidad (cs !! 4) ~?= 1,  -- [5, 10)
                       casCantidad (cs !! 5) ~?= 1 ], -- [10, +∞)

      -- Caso borde: valores en límites exactos
      "histograma_limites_exactos" ~:
        let valores = [0, 2.5, 5]
            h = histograma 2 (0, 5) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 1,  -- [0, 2.5) solo 0
                       casCantidad (cs !! 2) ~?= 1,  -- [2.5, 5) solo 2.5
                       casCantidad (cs !! 3) ~?= 1 ], -- [5, +∞) el 5

      "histograma_distribucion_sesgada" ~:
        let valores = replicate 90 1 ++ replicate 10 9
            h = histograma 4 (0, 10) valores
            cs = casilleros h
         in TestList [ casPorcentaje (cs !! 1) ~?= 90,  -- [0, 2.5)
                       casPorcentaje (cs !! 4) ~?= 10 ]  -- [7.5, 10)
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ -- Tests básicos existentes
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],

      -- Tests exhaustivos adicionales
      "casilleros_histograma_vacio_nan_check" ~: let cs = casilleros (vacio 2 (0, 10))
                                                  in TestList [ all (\c -> casPorcentaje c == 0.0) cs ~?= True,
                                                                all (\c -> not (isNaN (casPorcentaje c))) cs ~?= True ],

      "casilleros_un_casillero_lleno" ~:
        let valores = replicate 50 2.5
            h = histograma 2 (0, 5) valores
            cs = casilleros h
         in TestList [ casPorcentaje (cs !! 2) ~?= 100,  -- [2.5, 5)
                       all (\c -> casPorcentaje c == 0) [cs !! 1, cs !! 3] ~?= True ],

      "casilleros_extremos_llenos" ~:
        let valores = replicate 10 (-100) ++ replicate 10 100
            h = histograma 2 (0, 10) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 0) ~?= 10,  -- (-∞, 0)
                       casCantidad (cs !! 3) ~?= 10,  -- [10, +∞)
                       casPorcentaje (cs !! 0) ~?= 50,
                       casPorcentaje (cs !! 3) ~?= 50 ],

      "casilleros_suma_porcentajes_100" ~:
        let valores = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            h = histograma 5 (0, 10) valores
            cs = casilleros h
            sumaPorcentajes = sum (map casPorcentaje cs)
         in TestCase (assertBool ("Esperaba ≈ 100.0, obtuve " ++ show sumaPorcentajes)
                                  (abs (sumaPorcentajes - 100.0) < 0.01)),


      "casilleros_redondeo_porcentajes" ~:
        let valores = replicate 7 1 ++ replicate 3 9
            h = histograma 4 (0, 10) valores
            cs = casilleros h
         in TestList [ casPorcentaje (cs !! 1) ~?~ 70,  -- ~70%
                       casPorcentaje (cs !! 4) ~?~ 30 ], -- ~30%

      "casilleros_un_solo_valor" ~:
        let cs = casilleros (agregar 3 (vacio 3 (0, 9)))
         in TestList [ casPorcentaje (cs !! 2) ~?= 100,  -- [3, 6)
                       all (\c -> casPorcentaje c == 0) [cs !! 1, cs !! 3, cs !! 4] ~?= True ],

      "casilleros_sin_nan_nunca" ~:
        let valores = [0/0, 1/0, -1/0, 0]  -- Incluye NaN e infinitos
            h = histograma 2 (0, 10) valores
            cs = casilleros h
         in all (\c -> not (isNaN (casPorcentaje c))) cs ~?= True,

      "casilleros_overflow_conteo" ~:
        let valores = replicate 100000 2.5
            h = histograma 2 (0, 5) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 2) ~?= 100000,  -- [2.5, 5)
                       casPorcentaje (cs !! 2) ~?= 100 ],

      "casilleros_balance_perfecto" ~:
        let valores = concat [replicate 25 x | x <- [0.5, 1.5, 2.5, 3.5]]
            h = histograma 4 (0, 4) valores
            cs = casilleros h
         in all (\c -> casPorcentaje c == 25) (drop 1 (take 5 cs)) ~?= True,

      "casilleros_valores_identicos_limites" ~:
        let valores = replicate 10 2.0 ++ replicate 10 4.0 ++ replicate 10 6.0
            h = histograma 2 (0, 6) valores
            cs = casilleros h
         in TestList [ casCantidad (cs !! 1) ~?= 10,  -- [0, 3) -> 2.0
                       casCantidad (cs !! 2) ~?= 10,  -- [3, 6) -> 4.0
                       casCantidad (cs !! 3) ~?= 10 ], -- [6, +∞) -> 6.0

      "casilleros_vacio_despues_operaciones" ~:
        let h0 = vacio 3 (0, 6)
            h1 = agregar 2 h0
            h2 = agregar (-1) h1  -- Esto no afecta el histograma vacío original
            cs = casilleros h0
         in all (\c -> casCantidad c == 0 && casPorcentaje c == 0) cs ~?= True,

      "casilleros_integridad_datos" ~:
        let valores = [1..1000]
            h = histograma 10 (0, 1000) valores
            cs = casilleros h
         in TestList [ sum (map casCantidad cs) ~?= 1000,
                       TestCase (assertBool "Suma de porcentajes ≈ 100" (abs (sum (map casPorcentaje cs) - 100.0) < 0.1)),
                       all (\c -> casCantidad c >= 0) cs ~?= True,
                       all (\c -> casPorcentaje c >= 0 && casPorcentaje c <= 100) cs ~?= True ]
    ]

testsRecr :: Test
testsRecr =
  let
    -- Tamaño del AST usando recursión primitiva
    sizeRecr :: Expr -> Int
    sizeRecr =
      recrExpr
        (const 1)
        (\_ _ -> 1)
        (\_ _ l r -> 1 + l + r)
        (\_ _ l r -> 1 + l + r)
        (\_ _ l r -> 1 + l + r)
        (\_ _ l r -> 1 + l + r)

    -- Cuenta cuántos nodos binarios tienen hijos iguales
    equalChildrenCount :: Expr -> Int
    equalChildrenCount =
      recrExpr
        (const 0)
        (\_ _ -> 0)
        (\e1 e2 l r -> l + r + if e1 == e2 then 1 else 0)
        (\e1 e2 l r -> l + r + if e1 == e2 then 1 else 0)
        (\e1 e2 l r -> l + r + if e1 == e2 then 1 else 0)
        (\e1 e2 l r -> l + r + if e1 == e2 then 1 else 0)

    -- Reconstrucción identidad usando recursión primitiva
    rebuildRecr :: Expr -> Expr
    rebuildRecr =
      recrExpr
        Const
        Rango
        (\e1 e2 _ _ -> Suma e1 e2)
        (\e1 e2 _ _ -> Resta e1 e2)
        (\e1 e2 _ _ -> Mult e1 e2)
        (\e1 e2 _ _ -> Div e1 e2)

    e0 = Const 42
    e1 = Suma (Const 1) (Const 2)
    e2 = Mult (Suma (Const 1) (Const 2)) (Resta (Const 3) (Const 4))
    e3 = Suma (Const 1) (Const 1)
    e4 = Suma (Suma (Const 1) (Const 1)) (Const 0)
  in
  test
    [ "size_recr_const" ~: sizeRecr e0 ~?= 1,
      "size_recr_simple" ~: sizeRecr e1 ~?= 3,
      "size_recr_anidado" ~: sizeRecr e2 ~?= 7,
      "equal_children_simple" ~: equalChildrenCount e3 ~?= 1,
      "equal_children_anidado" ~: equalChildrenCount e4 ~?= 1,
      "rebuild_identity" ~: rebuildRecr e2 ~?= e2
    ]

testsFold :: Test
testsFold =
  let
    -- Tamaño del AST usando fold
    sizeFold :: Expr -> Int
    sizeFold =
      foldExpr
        (const 1)
        (\_ _ -> 1)
        (\l r -> 1 + l + r)
        (\l r -> 1 + l + r)
        (\l r -> 1 + l + r)
        (\l r -> 1 + l + r)

    -- Reconstrucción identidad usando fold
    rebuildFold :: Expr -> Expr
    rebuildFold = foldExpr Const Rango Suma Resta Mult Div

    e0 = Const 42
    e1 = Suma (Const 1) (Const 2)
    e2 = Mult (Suma (Const 1) (Const 2)) (Resta (Const 3) (Const 4))
  in
  test
    [ "size_fold_const" ~: sizeFold e0 ~?= 1,
      "size_fold_simple" ~: sizeFold e1 ~?= 3,
      "size_fold_anidado" ~: sizeFold e2 ~?= 7,
      "rebuild_fold_identity" ~: rebuildFold e2 ~?= e2
    ]

testsEval :: Test
testsEval =
  test
    [ -- Eval determinista con genFijo: el rango se evalúa al punto medio
      fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- Reproducibilidad con semilla: el primer rango evalúa a 2.7980492
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      -- Constante pura
      "eval_constante" ~: fst (eval (Const 5) genFijo) ~?= 5.0,
      -- Operaciones determinísticas con genFijo
      "eval_resta_const" ~: fst (eval (Resta (Const 5) (Const 2)) genFijo) ~?= 3.0,
      "eval_mult_const" ~: fst (eval (Mult (Const 3) (Const 4)) genFijo) ~?= 12.0,
      "eval_div_const" ~: fst (eval (Div (Const 12) (Const 3)) genFijo) ~?= 4.0,
      -- Rango con genFijo retorna el medio
      "eval_rango_genFijo" ~: fst (eval (Rango 1 5) genFijo) ~?= 3.0,
      -- Expresión compuesta con genFijo: valida el encadenamiento del generador
      let exprC = Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2)
       in "eval_compuesto_genFijo" ~: fst (eval exprC genFijo) ~?= 155.25,
      -- División por cero: infinito y NaN
      let vInf = fst (eval (Div (Const 1) (Const 0)) genFijo)
          vNaN = fst (eval (Div (Const 0) (Const 0)) genFijo)
       in TestList [ "eval_div_zero_infinite" ~: isInfinite vInf ~?= True,
                     "eval_div_zero_nan" ~: isNaN vNaN ~?= True ],
      -- Orden/consumo del generador afecta el resultado cuando difiere la cantidad de draws por lado
      let a = Suma (Rango 1 5) (Rango 1 5) -- consume 2
          b = Rango 1 5                    -- consume 1
          s = 0
          vAB = fst (eval (Resta a b) (genNormalConSemilla s)) -- (x1+x2) - x3
          vBA = fst (eval (Resta b a) (genNormalConSemilla s)) -- x1 - (x2+x3)
       in "eval_orden_consumo_generador_no_conmutativo" ~:
            TestCase (assertBool "Se esperaba distinto por distinto consumo de PRNG" (abs (vAB - vBA) > epsilon)),
      -- Con dos rangos por evaluación, el generador avanza 2 * n
      let expr2 = Suma (Rango 1 5) (Rango 1 5)
          n2 = 7
          g00 = genNormalConSemilla 0
          (_, gFinal2) = muestra (eval expr2) n2 g00
          xs2n1 = fst $ muestra (dameUno (1, 5)) (2 * n2 + 1) g00
          esperado2 = last xs2n1
       in "eval_avance_generador_2_por_eval" ~: fst (dameUno (1, 5) gFinal2) ~?~ esperado2
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  let
    -- Caso determinístico con genFijo: todos los valores iguales a 3.0
    hFijo = fst $ armarHistograma 3 5 (dameUno (1, 5)) genFijo
    csFijo = casilleros hFijo

    -- Verifica avance del generador: el siguiente valor tras tomar n muestras coincide
    n = 10
    f = dameUno (1, 5)
    g0 = genNormalConSemilla 0
    (hAv, gAv') = armarHistograma 5 n f g0
    xsN1 = fst $ muestra f (n + 1) g0
    nextFromGAv' = fst (f gAv')
  in
  test
    [ "armar_histograma_genFijo_cuentas" ~:
        TestList
          [ length csFijo ~?= 5, -- 3 casilleros centrales + 2 extremos
            casCantidad (csFijo !! 0) ~?= 0,
            casCantidad (csFijo !! 1) ~?= 0,
            casCantidad (csFijo !! 2) ~?= 5,
            casCantidad (csFijo !! 3) ~?= 0,
            casCantidad (csFijo !! 4) ~?= 0
          ],
      -- El rango se calcula con rango95 a partir de las muestras
      "armar_histograma_genFijo_rango95" ~:
        let l = casMinimo (csFijo !! 1)
            u = casMaximo (csFijo !! 3)
         in TestList [ l ~?~ 2.0,
                       u ~?~ 4.0 ],
      -- El generador resultante queda avanzado exactamente n pasos
      "armar_histograma_avanza_generador" ~:
        let esperado = last xsN1
         in nextFromGAv' ~?~ esperado,
      -- Mínimos permitidos m=1, n=1 con generador constante
      let constG x g = (x, g)
          hMin = fst $ armarHistograma 1 1 (constG 10.0) genFijo
          csMin = casilleros hMin
       in "armar_histograma_minimos" ~:
            TestList
              [ length csMin ~?= 3,
                casCantidad (csMin !! 0) ~?= 0,
                casCantidad (csMin !! 1) ~?= 1,
                casCantidad (csMin !! 2) ~?= 0
              ],
      -- Rango95 con varianza baja: 100% en el casillero central
      let hConst = fst $ armarHistograma 5 20 (\g -> (3.0, g)) genFijo
          csConst = casilleros hConst
       in "armar_histograma_varianza_baja" ~:
            TestList [ length csConst ~?= 7,
                       casCantidad (csConst !! 3) ~?= 20,
                       casPorcentaje (csConst !! 3) ~?= 100,
                       all (\i -> i == 3 || (casCantidad (csConst !! i) == 0 && casPorcentaje (csConst !! i) == 0)) [0..6] ~?= True ],
      -- Estabilidad de porcentajes con n grande
      let mGrande = 10
          nGrande = 1000
          hGrande = fst $ armarHistograma mGrande nGrande (dameUno (0, 1)) (genNormalConSemilla 42)
          sumaP = sum (map casPorcentaje (casilleros hGrande))
       in "armar_histograma_suma_porcentajes_100_grande" ~:
            TestCase (assertBool ("Esperaba ≈ 100.0, obtuve " ++ show sumaP)
                                (abs (sumaP - 100.0) < 1e-4))
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  let
    -- expr constante -> histograma determinístico como en armarHistograma con valor 3.0
    hConst = fst $ evalHistograma 3 5 (Const 3) genFijo
    csConst = casilleros hConst

    -- expr con rangos pero genFijo -> valores constantes
    exprC = Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))
    hExpr = fst $ evalHistograma 3 5 exprC genFijo
    csExpr = casilleros hExpr

    -- avance del generador con expresión
    n = 8
    g0 = genNormalConSemilla 0
    (hE, gE') = evalHistograma 5 n (Rango 1 5) g0
    xsN1 = fst $ muestra (eval (Rango 1 5)) (n + 1) g0
    nextFromGE' = fst (eval (Rango 1 5) gE')
  in
  test
    [ -- Con genFijo, cualquier expresión basada en rangos es determinística
      "eval_histograma_expr_genFijo" ~:
        TestList
          [ length csExpr ~?= 5,
            casCantidad (csExpr !! 2) ~?= 5
          ],
      -- El generador queda avanzado exactamente n evaluaciones de la expresión
      "eval_histograma_avanza_generador" ~:
        let esperado = last xsN1
         in nextFromGE' ~?~ esperado
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ -- División con suma y producto: chequea precedencia y paréntesis en el numerador
      mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      -- Sumas anidadas: sin paréntesis innecesarios por asociatividad
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      -- Productos anidados: sin paréntesis innecesarios por asociatividad
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      -- Restas: se parentiza para reflejar no asociatividad
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      -- Suma de producto: producto se parentiza por mayor precedencia en el operando
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      -- Producto de suma: suma se parentiza por menor precedencia
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
      -- División anidada a la derecha: se parentiza el denominador
      mostrar (Div (Const 1) (Div (Const 2) (Const 3)))
        ~?= "1.0 / (2.0 / 3.0)",
      -- División con suma en el denominador: se parentiza el denominador
      mostrar (Div (Const 1) (Suma (Const 2) (Const 3)))
        ~?= "1.0 / (2.0 + 3.0)",
      -- No parentizar Rango innecesariamente
      mostrar (Suma (Rango 1 5) (Const 2))
        ~?= "1.0~5.0 + 2.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]