module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: ... anotar el tipo ...
--recrExpr = error "COMPLETAR EJERCICIO 7"
recrExpr :: (Float -> b) -> (Float -> Float -> b) -> (Expr -> Expr -> b -> b -> b) -> (Expr -> Expr -> b -> b -> b) -> (Expr -> Expr -> b -> b -> b) -> (Expr -> Expr -> b -> b -> b) -> Expr -> b
recrExpr cnst rng sum rst mul dv x = case x of
      Const f -> cnst f
      Rango f1 f2 -> rng f1 f2
      Suma e1 e2 -> sum e1 e2 (rec e1) (rec e2)
      Resta e1 e2 -> rst e1 e2(rec e1) (rec e2)
      Mult e1 e2 -> mul e1 e2 (rec e1) (rec e2) 
      Div e1 e2 -> dv e1 e2 (rec e1) (rec e2)
    where rec = recrExpr cnst rng sum rst mul dv 

-- foldExpr :: ... anotar el tipo ...
--foldExpr = error "COMPLETAR EJERCICIO 7"
foldExpr :: (Float -> b) -> (Float -> Float -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Expr -> b
foldExpr cnst rng sum rst mul dv x = case x of
      Const f -> cnst f
      Rango f1 f2 -> rng f1 f2
      Suma e1 e2 -> sum (rec e1) (rec e2)
      Resta e1 e2 -> rst (rec e1) (rec e2)
      Mult e1 e2 -> mul (rec e1) (rec e2) 
      Div e1 e2 -> dv (rec e1) (rec e2)
    where rec = foldExpr cnst rng sum rst mul dv 


-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr
  (\valor generador -> (valor, generador))
  (\limInf limSup -> dameUno (limInf, limSup))
  (operarAvanzandoGen (+))
  (operarAvanzandoGen (-))
  (operarAvanzandoGen (*))
  (operarAvanzandoGen (/))
  where
    operarAvanzandoGen op evalIzq evalDer genInicial = (op valorIzq valorDer, genTrasDer)
      where
        (valorIzq, genTrasIzq) = evalIzq genInicial   --El valor izquierdo se obtiene evaluando la izquierda con genInicial
        (valorDer, genTrasDer) = evalDer genTrasIzq   --El generador avanzo. La derecha se evalua despues de la izquierda

-- | @armarHistograma n m genMuestras genInicial@
-- arma un histograma con @n@ casilleros a partir del resultado de tomar
-- @numMuestras@ muestras de @genMuestras@ usando el @genInicial@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma n m genMuestras genInicial =
  (histograma n rangoConfianza muestras, genFinal)
  where
    (muestras, genFinal) = muestra genMuestras m genInicial
    rangoConfianza = rango95 muestras

-- | @evalHistograma n m expresion@
-- evalúa la @expresion@ @m@ veces,
-- devuelve un histograma con @n@ casilleros y rango calculado con @rango95@
-- para abarcar el 95% de confianza de los valores.
-- @m@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma n m expresion = armarHistograma n m (eval expresion)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = recrExpr
  show
  (\inf sup -> show inf ++ "~" ++ show sup)
  (binOp "+" CESuma)
  (binOp "-" CEResta)
  (binOp "*" CEMult)
  (binOp "/" CEDiv)
  where
    binOp op consPadre e1 e2 s1 s2 =
      maybeParen (necesitaParen consPadre Izq (constructor e1)) s1 ++
      " " ++ op ++ " " ++
      maybeParen (necesitaParen consPadre Der (constructor e2)) s2

    necesitaParen consPadre lado consHijo =
      case (consPadre, lado, consHijo) of
        (CESuma, _, CEResta) -> True
        (CESuma, _, CEMult) -> True
        (CESuma, _, CEDiv) -> True
        (CEResta, _, CESuma) -> True
        (CEResta, _, CEResta) -> True
        (CEMult, _, CESuma) -> True
        (CEMult, _, CEResta) -> True
        (CEDiv, Izq, CESuma) -> True
        (CEDiv, Izq, CEResta) -> True
        (CEDiv, Der, CEConst) -> False
        (CEDiv, Der, CERango) -> False
        (CEDiv, Der, _) -> True
        _ -> False

data Lado = Izq | Der
  deriving (Show, Eq)

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
