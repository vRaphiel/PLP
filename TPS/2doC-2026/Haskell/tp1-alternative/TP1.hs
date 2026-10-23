module TP1 where

import Data.List (union)

data Caja = Bombilla Bool | Nada
  deriving (Eq)

instance Show Caja where
  show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito
  = Caja Caja
  | Serie Circuito Circuito
  | Paralelo Caja Circuito Circuito Caja
  deriving (Eq)

instance Show Circuito where
  show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada)
    ++ "{"
    ++ (showDeCircuito circuitoIzquierdo)
    ++ "}"
    ++ "{"
    ++ (showDeCircuito circuitoDerecho)
    ++ "}"
    ++ (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) =
  "("
    ++ (showDeCircuitoConEstructura circuitoInicial)
    ++ "-"
    ++ (showDeCircuitoConEstructura circuitoFinal)
    ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada)
    ++ "{"
    ++ (showDeCircuitoConEstructura circuitoIzquierdo)
    ++ "}"
    ++ "{"
    ++ (showDeCircuitoConEstructura circuitoDerecho)
    ++ "}"
    ++ (showDeCaja cajaSalida)

on = Bombilla True

off = Bombilla False

cajaOn = Caja on

cajaOff = Caja off

cajaNada = Caja Nada

-- 1: recCircuito
recCircuito ::
  (Caja -> a) ->
  (Circuito -> Circuito -> a -> a -> a) ->
  (Caja -> Circuito -> Circuito -> Caja -> a -> a -> a) ->
  Circuito ->
  a
recCircuito fCaja fSerie fParalelo c = case c of
  Caja caja -> fCaja caja
  Serie c1 c2 ->
    fSerie c1 c2 (rec c1) (rec c2)
  Paralelo cEntrada cIzq cDer cSalida ->
    fParalelo cEntrada cIzq cDer cSalida (rec cIzq) (rec cDer)
  where
    rec = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito
foldCircuito ::
  (Caja -> a) ->
  (a -> a -> a) ->
  (Caja -> a -> a -> Caja -> a) ->
  Circuito ->
  a
foldCircuito fCaja fSerie fParalelo =
  recCircuito
    fCaja
    (\_ _ rIzq rDer -> fSerie rIzq rDer)
    (\cEntrada _ _ cSalida rIzq rDer -> fParalelo cEntrada rIzq rDer cSalida)

-- 3 invertido
invertido :: Circuito -> Circuito
invertido =
  foldCircuito
    (\c -> Caja c)
    (\rIzq rDer -> Serie rDer rIzq)
    (\cEnt rIzq rDer cSal -> Paralelo cSal rDer rIzq cEnt)

-- 4: hayCaminoIluminado
hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado =
  foldCircuito
    (\c -> c == on)
    (\rIzq rDer -> rIzq && rDer)
    (\cEnt rIzq rDer cSal -> (cEnt == on) && (cSal == on) && (rIzq || rDer))

-- 5: cantidadPrendidas
cantidadPrendidas :: Circuito -> Int
cantidadPrendidas =
  foldCircuito
    (\c -> esOn)
    (\rIzq rDer -> rIzq + rDer)
    (\cEnt rIzq rDer cSal -> esOn cEnt + esOn cSal + rIzq + rDer)
  where
    esOn c = if c == on then 1 else 0

-- 6: cajasDeCircuito
cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito =
  foldCircuito
    (\c -> [c])
    (++)
    (\cEnt rIzq rDer cSal -> [cEnt] ++ rIzq ++ rDer ++ [cSal])

-- 7: esCircuitoProlijo
esSerie :: Circuito -> Bool
esSerie (Serie _ _) = True
esSerie _ = False

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo =
  recCircuito
    (const True)
    (\_ cDer rIzq rDer -> rIzq && rDer && not (esSerie cDer))
    (\_ _ _ _ rIzq rDer -> rIzq && rDer)

-- 8: circuitoEmprolijado
-- circuitoEmprolijado :: Circuito -> Circuito

-- 9: tienenLaMismaEstructura
tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura =
  foldCircuito
    ( \_ c2 -> case c2 of
        Caja _ -> True
        _ -> False
    )
    ( \rIzq rDer c2 -> case c2 of
        Serie c2Izq c2Der -> rIzq c2Izq && rDer c2Der
        _ -> False
    )
    ( \_ rIzq rDer _ c2 -> case c2 of
        Paralelo _ c2Izq c2Der _ -> rIzq c2Izq && rDer c2Der
        _ -> False
    )

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito = undefined

-- 10: subCircuitoMásResistente
subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = undefined

{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR
Demostración: alternado . alternado = id

Por el principio de extensionalidad funcional, queremos probar que para todo c :: Circuito:

(alternado . alternado) c = id c

---

Por el lema de Generación, c puede ser de tres casos: Caja (caso 1), Serie (caso 2) o Paralelo (caso 3)

Caso 1: c = Caja c0
Por extensionalidad sobre Caja, c0 puede tomar 3 valores posibles: Nada, Bombilla True o Bombilla False.

Caso 1.a: c0 = Nada
(alternado . alternado) (Caja Nada)
  = { C }
    alternado (alternado (Caja Nada))
  = { AC }
    alternado (Caja (cajaAlternada Nada))
  = { CAN }
    alternado (Caja Nada)
  = { AC }
    Caja (cajaAlternada Nada)
  = { CAN }
    Caja Nada
  = { I }
    id (Caja Nada)

Caso 1.b: c0 = Bombilla True
(alternado . alternado) (Caja (Bombilla True))
  = { C }
    alternado (alternado (Caja (Bombilla True)))
  = { AC }
    alternado (Caja (cajaAlternada (Bombilla True)))
  = { CAB }
    alternado (Caja (Bombilla (not True)))
  = { NT }
    alternado (Caja (Bombilla False))
  = { AC }
    Caja (cajaAlternada (Bombilla False))
  = { CAB }
    Caja (Bombilla (not False))
  = { NF }
    Caja (Bombilla True)
  = { I }
    id (Caja (Bombilla True))

Caso 1.c: c0 = Bombilla False
(alternado . alternado) (Caja (Bombilla False))
  = { C }
    alternado (alternado (Caja (Bombilla False)))
  = { AC }
    alternado (Caja (cajaAlternada (Bombilla False)))
  = { CAB }
    alternado (Caja (Bombilla (not False)))
  = { NF }
    alternado (Caja (Bombilla True))
  = { AC }
    Caja (cajaAlternada (Bombilla True))
  = { CAB }
    Caja (Bombilla (not True))
  = { NT }
    Caja (Bombilla False)
  = { I } (Caja (Bombilla False))

Lema {L1} :
Por lo probado, tenemos que
∀ x :: Caja, cajaAlternada (cajaAlternada x) = x.

---

Caso 2: c = Serie ci cf

- Hipótesis Inductivas:
  - HI1: alternado (alternado ci) = ci
  - HI2: alternado (alternado cf) = cf

(alternado . alternado) (Serie ci cf)
  = { C }
    alternado (alternado (Serie ci cf))
  = { AS }
    alternado (Serie (alternado ci) (alternado cf))
  = { AS }
    Serie (alternado (alternado ci)) (alternado (alternado cf))
  = { HI1 }
    Serie ci (alternado (alternado cf))
  = { HI2 }
    Serie ci cf
  = { I }
    id (Serie ci cf)

---

Caso 3: c = Paralelo ce ci cd cs

- Hipótesis Inductivas:
  - HI1: alternado (alternado ci) = ci
  - HI2: alternado (alternado cd) = cd

(alternado . alternado) (Paralelo ce ci cd cs)
  = { C }
    alternado (alternado (Paralelo ce ci cd cs))
  = { AP }
    alternado (Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs))
  = { AP }
    Paralelo (cajaAlternada (cajaAlternada ce)) (alternado (alternado ci)) (alternado (alternado cd)) (cajaAlternada (cajaAlternada cs))
  = { Lema 1 }
    Paralelo ce (alternado (alternado ci)) (alternado (alternado cd)) cs
  = { HI1 }
    Paralelo ce ci (alternado (alternado cd)) cs
  = { HI2 }
    Paralelo ce ci cd cs
  = { I }
    id (Paralelo ce ci cd cs)

Probamos con inducción estructural sobre Circuito que alternado . alternado = id.

--}