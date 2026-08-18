{-
i - Tipo de las funciones
max2 (x, y) | x >= y = x | otherwise = y
    Tipo: max2 :: (Float, Float) -> Float
    Análisis: Recibe una tupla de dos floats y devuelve un float. No está currificada.
normaVectorial (x, y) = sqrt (x^2 + y^2)
    Tipo: normaVectorial :: (Float, Float) -> Float
    Análisis: Al igual que la anterior, recibe sus argumentos empaquetados en una tupla de tipo (Float, Float). Tampoco está currificada.
subtract = flip (-)
    Tipo: subtract :: Float -> Float -> Float
    Análisis: Es la resta pero con sus argumentos invertidos. Recibe los floats de a uno (currificada).
predecesor = subtract 1
    Tipo: predecesor :: Float -> Float
    Análisis: Es una aplicación parcial de subtract con el argumento 1. Recibe un float y devuelve el float anterior.
evaluarEnCero = \f -> f 0
    Tipo: evaluarEnCero :: (Float -> b) -> b
    Análisis: Recibe una función f (cuyo dominio debe ser numérico, en este caso Float) y devuelve el resultado de evaluar f en 0. El retorno tiene tipo genérico b.
dosVeces = \f -> f . f
    Tipo: dosVeces :: (a -> a) -> a -> a
    Análisis: Recibe una función f y la compone consigo misma. Para que la composición f . f sea válida, el dominio y el codominio de f deben ser idénticos (a -> a).
flipAll = map flip
    Tipo: flipAll :: [a -> b -> c] -> [b -> a -> c]
    Análisis: map aplica una función a cada elemento de una lista. Como flip toma una función y da su versión invertida, flipAll toma una lista de funciones currificadas binarias y retorna otra lista con sus versiones invertidas.
flipRaro = flip flip
    Tipo: flipRaro :: b -> (a -> b -> c) -> a -> c
    Análisis (Este es el que suele trabar a todos): Descompongamos la firma de flip :: (p -> q -> r) -> q -> p -> r. Si aplicamos flip a sí mismo, el primer flip toma al segundo flip como su primer argumento. El tipo del argumento esperado por el primer flip es (p -> q -> r). El tipo del segundo flip es (x -> y -> z) -> y -> x -> z. Al unificar:
        p = (x -> y -> z)
        q = y
        r = (x -> z) Por lo tanto, la firma resultante de flip flip (que es q -> p -> r) se convierte en: y -> (x -> y -> z) -> x -> z. Reescribiendo con variables limpias: b -> (a -> b -> c) -> a -> c. Comportamiento: Recibe un valor y, una función f, un valor x, y evalúa f x y 146].

ii. Funciones no currificadas y sus versiones currificadas [cite: 429]
Las funciones de la lista que no están currificadas son max2 y normaVectorial, ya que reciben sus argumentos agrupados en una tupla en lugar de a uno por vez [cite: 429].
Aquí tenés sus equivalentes currificadas junto con sus firmas de tipo:

Versión currificada de max2

max2Curry :: Float -> Float -> Float
max2Curry x y = max2 (x, y)

Versión currificada de normaVectorial

normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = normaVectorial (x, y)
-}