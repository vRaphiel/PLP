% EJERCICIO 3

% Definición de números naturales en aritmética de Peano:
natural(0).
natural(suc(X)) :- natural(X).

% ------------------------------------------------------------------------------
% i. Explicar qué sucede al realizar la consulta: menorOIgual(0, X).
% ------------------------------------------------------------------------------
/*
Definición original dada en el enunciado:
  (R1) menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).
  (R2) menorOIgual(X, X) :- natural(X).

Al ejecutar ?- menorOIgual(0, X):
1. Prolog intenta resolver la meta utilizando la primera regla (R1).
   Unifica X con suc(Y1), generando el nuevo subobjetivo:
     ?- menorOIgual(0, Y1).
2. Para resolver este nuevo subobjetivo, vuelve a tomar la primera regla disponible (R1).
   Unifica Y1 con suc(Y2), generando:
     ?- menorOIgual(0, Y2).
3. Como Prolog utiliza una estrategia de búsqueda primero en profundidad (DFS)
   siguiendo el orden textual de las cláusulas (top-down), este proceso se repite
   indefinidamente.
4. En cada paso se construye un término cada vez más anidado suc(suc(suc(...)))
   sin llegar NUNCA a evaluar la regla base (R2).

Consecuencia:
  El programa entra en una recursión infinita y se cuelga (Stack Overflow / divergencia)
  sin devolver jamás ni siquiera la primera solución (que debería ser X = 0).
*/

% ------------------------------------------------------------------------------
% ii. Circunstancias en las que puede colgarse un programa en Prolog
% ------------------------------------------------------------------------------
/*
Un programa en Prolog puede ejecutarse infinitamente sin arrojar soluciones cuando
su árbol de resolución SLD presenta una rama infinita que el motor recorre antes
de encontrar nodos de éxito. Esto sucede principalmente por:

1. Orden inadecuado de cláusulas (Regla recursiva antes del caso base):
   Si la regla recursiva precede al caso base y puede aplicarse indefinidamente
   generando submetas sin acotar (como ocurrió en el ítem i), DFS descenderá
   por esa rama infinita y jamás retrocederá (backtracking) hacia el caso base.

2. Recursión a izquierda (Left-recursion):
   Reglas de la forma P(...) :- P(...), ... donde el primer literal del cuerpo
   es idéntico o mutuamente recursivo con la cabeza y sus argumentos no decrecen
   ni están acotados por hechos previos.

3. Generadores infinitos seguidos de pruebas que fallan:
   Si un generador infinito produce infinitos candidatos para los cuales los
   literales posteriores siempre fallan, Prolog seguirá generando infinitamente
   candidatos sin encontrar una solución.

4. Variables libres en la llamada recursiva sin acotar por el dominio:
   Cuando se pasa una variable sin instanciar que crece en profundidad de términos
   en lugar de descomponer una estructura de datos ya acotada.
*/

% ------------------------------------------------------------------------------
% iii. Corrección de la definición de menorOIgual
% ------------------------------------------------------------------------------
/*
Corrección:
  Basta con invertir el orden de las cláusulas, colocando el caso base ANTES
  del caso recursivo.
  
  (Importante: NO se deben usar operadores aritméticos como >= o is, ya que 0 y suc(X)
   son términos simbólicos de la aritmética de Peano, no enteros de máquina).

Lógica inductiva:
  - Caso base: Todo natural X es menor o igual a sí mismo (X <= X).
  - Caso recursivo: Si X <= Y, entonces X <= suc(Y).
*/

% menorOIgual(?X, ?Y)
% Caso base primero:
menorOIgual(X, X) :- 
    natural(X).

% Caso recursivo después:
menorOIgual(X, suc(Y)) :- 
    menorOIgual(X, Y).

/*
Comprobación de funcionamiento con ?- menorOIgual(0, X).:
  1) Prueba caso base: menorOIgual(0, 0) :- natural(0).
     -> Éxito: X = 0.
  2) Backtracking (;): prueba caso recursivo con X = suc(Y1)
     menorOIgual(0, suc(Y1)) :- menorOIgual(0, Y1).
     Resuelve menorOIgual(0, Y1) encontrando Y1 = 0 por el caso base.
     -> Éxito: X = suc(0).
  3) Backtracking (;):
     -> Éxito: X = suc(suc(0)), etc.
Genera uno a uno todos los números naturales en orden creciente.

Comprobación cuando falla:
  ?- menorOIgual(suc(0), 0).
  - Caso base: suc(0) y 0 no unifican -> falla.
  - Caso recursivo: 0 no unifica con suc(Y) -> falla.
  -> false inmediato (no se cuelga).
*/
