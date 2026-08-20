pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | a <- [1 ..], b <- [1 ..], c <- [1 ..], a ^ 2 + b ^ 2 == c ^ 2]

-- No es correcto primero, porque se estan usando 3 generadores infinitos cuando se debe usar solo uno.
-- Segundo, aunque se permitiera, primero va a crear todos los a antes de generar los b, y creara ambos
-- antes de generar los c. Esto causa un problema, porque se va a quedar colgado en los c, va a generar
-- a = 1, b = 1 y luego va a generar infinitamente c's, lo cual hara que no termine.

pitagoricasNew :: [(Integer, Integer, Integer)]
pitagoricasNew =
  [ (a, b, c)
  | s <- [3 ..],
    a <- [1 .. s - 2],
    b <- [1 .. s - 3],
    let c = s - a - b,
    a ^ 2 + b ^ 2 == c ^ 2
  ]

{--
Para las triplas (a, b, c) usamos la misma regla que para tuplas:
Métrica (Suma total): (s in [3..]) (arranca en 3 porque (1 + 1 + 1 = 3)). s = a + b + c
Primera variable: (a in [1 .. s - 2]) (le dejamos espacio a (b) y (c))
Segunda variable: (b in [1 .. s - a - 1]) (le dejamos espacio a (c))
Por diferencia: (c = s - a - b)

Por diagonalizacion:
1. Generador Infinito Principal (La métrica):
    Generamos la suma (s) desde 3 al infinito: s <- [3..]
2. Acotar (a):
    Como (b) y (c) deben valer al menos 1 cada uno, (a) como máximo puede valer (s - 2). 
    Por lo tanto, el rango de (a) es finito: a <- [1 .. s - 2]
3. Acotar (b):
    Con (s) y (a) ya elegidos, y sabiendo que (c) debe valer al menos 1, 
    (b) solo puede llegar hasta (s - a - 1). Su rango finito es: b <- [1 .. s - a - 1]
4. Calcular (c) por diferencia:
    En lugar de generar (c) con un generador, lo calculamos de forma exacta usando una 
    ligadura `let`: let c = s - a - b
--}