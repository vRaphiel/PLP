{-- Programación Funcional en Haskell --}
{-- Cuando definimos el paradigma no necesitmos agregar una especificación para
N argumentos, con la currificación ya sabemos que se puede.
Siempre se toma un solo argumento a tiempo. 

Los tipos asocian a derecha. Tomamos el primer a derecha, luego el segundo a derecha,
hasta el n-esimo, luego el n+1 es el que se devuelve.
a -> b -> c entonces toma a, toma b
--}

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x, y)

-- prod' = ((curry prod) 2) 2
-- ((\x -> \y -> prod (x, y)) 2) 2
-- (\y -> prod (2, y)) 2


uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x, y) -> f x y

-- Compone dos funciones
-- ((\x -> x * 4) . (\y -> y - 3))
-- Primero aplica g
-- Despues aplica f al resultado de aplicar g
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g = (\x -> f (g x))

-- Invierte los argumentos de una función
-- flip (\x y -> x - y) 1 5 devuelve 4
-- Sabemos que recibe una funcion y devuelve otra. Recibe dos argumentos y devuelve un resultado
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \x y -> f y x

-- Aplica una funcion a un argumento
-- id $ 6 devuelve 6
($) :: (a -> b) -> a -> b
($) f x = f x