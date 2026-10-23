module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show :: Circuito -> String
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito :: (Caja -> b) -> (Circuito -> b -> Circuito -> b -> b) -> (Caja -> Circuito -> b -> Circuito -> b -> Caja -> b) -> Circuito -> b
recCircuito fCaja fSerie fParalelo circuito = case circuito of
    Caja caja -> fCaja caja
    Serie circuito1 circuito2 -> fSerie circuito1 (recSobreCircuito circuito1) circuito2 (recSobreCircuito circuito2)
    Paralelo caja1 circuito1 circuito2 caja2 -> fParalelo caja1 circuito1 (recSobreCircuito circuito1) circuito2 (recSobreCircuito circuito2) caja2
  where recSobreCircuito = recCircuito fCaja fSerie fParalelo

-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b -> b -> b) -> (Caja -> b -> b -> Caja -> b) -> Circuito -> b
foldCircuito fCaja fSerie fParalelo circuito = recCircuito fCaja fEstructSerie fEstructParalelo circuito
 where fEstructSerie _ circuito1 _ circuito2 = fSerie circuito1 circuito2
       fEstructParalelo caja1 _ circuito1 _ circuito2 caja2 = fParalelo caja1 circuito1 circuito2 caja2

-- 3 invertido

invertido :: Circuito -> Circuito
invertido =
  foldCircuito
    (\c -> Caja c)
    (\rIzq rDer -> Serie rDer rIzq)
    (\cEnt rIzq rDer cSal -> Paralelo cSal rDer rIzq cEnt)

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito
    estaEncendida
    (&&)
    paraleloTieneIluminado
  where
    estaEncendida (Bombilla True)  = True
    estaEncendida (Bombilla False) = False
    estaEncendida Nada             = False

    paraleloTieneIluminado caja1 circuito1 circuito2 caja2 =
      estaEncendida caja1 &&
      estaEncendida caja2 &&
      (circuito1 || circuito2)

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = foldCircuito
    estaPrendida
    (+)
    cantidadPrendidasParalelo
  where
    estaPrendida (Bombilla True)  = 1
    estaPrendida (Bombilla False) = 0
    estaPrendida Nada             = 0

    cantidadPrendidasParalelo caja1 prendidasCircuito1 prendidasCircuito2 caja2 =
        estaPrendida caja1 + estaPrendida caja2 + prendidasCircuito1 + prendidasCircuito2


-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito
    (: [])
    (++)
    cajasDeParalelo
  where
    cajasDeParalelo caja1 cajasCircuito1 cajasCircuito2 caja2 =
      [caja1] ++ cajasCircuito1 ++ cajasCircuito2 ++ [caja2]

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito
    (const True)
    esProlijoSerie
    esProlijoParalelo
  where
    esProlijoSerie _ _ (Serie _ _) _ = False
    esProlijoSerie _ rec1 _ rec2 = rec1 && rec2

    esProlijoParalelo _ _ rec1 _ rec2 _ = rec1 && rec2

-- 8: circuitoEmprolijado

circuitoEmprolijado :: Circuito -> Circuito
circuitoEmprolijado = foldCircuito Caja emprolijarSerie Paralelo
  where
    emprolijarSerie c1 (Serie c2 c3) = emprolijarSerie (Serie c1 c2) c3
    emprolijarSerie c1 c2            = Serie c1 c2

-- 9: tienenLaMismaEstructura

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura = foldCircuito 
    mismaEstructuraCaja 
    mismaEstructuraSerie 
    mismaEstructuraParalelo
  where
    mismaEstructuraCaja _ (Caja _) = True
    mismaEstructuraCaja _ _        = False

    mismaEstructuraSerie recIzq recDer (Serie c2Izq c2Der) = recIzq c2Izq && recDer c2Der
    mismaEstructuraSerie _      _      _                   = False

    mismaEstructuraParalelo _ recIzq recDer _ (Paralelo _ c2Izq c2Der _) = recIzq c2Izq && recDer c2Der
    mismaEstructuraParalelo _ _      _      _ _                          = False

-- 10: subCircuitoMásResistente

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente circuito = fst (recCircuito fCaja fSerie fParalelo circuito)
  where
    fCaja caja = (Caja caja, resistenciaCircuito (Caja caja))
    
    fSerie c1 rec1 c2 rec2 = 
        maxResistencia (Serie c1 c2, resistenciaCircuito (Serie c1 c2)) 
                       (maxResistencia rec1 rec2)
                       
    fParalelo c1 cIzq recIzq cDer recDer c2 = 
        maxResistencia (Paralelo c1 cIzq cDer c2, resistenciaCircuito (Paralelo c1 cIzq cDer c2))
                       (maxResistencia recIzq recDer)
                       
    maxResistencia (circ1, res1) (circ2, res2)
        | res1 >= res2 = (circ1, res1)
        | otherwise    = (circ2, res2)
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