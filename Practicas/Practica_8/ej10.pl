% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 10
% Paradigmas de Programación (PLP)
% ==============================================================================

% intercalar(?L1, ?L2, ?L3)
%
% L3 es el resultado de intercalar uno a uno los elementos de las listas L1 y L2.
% Si una lista tiene longitud menor, entonces el resto de la lista más larga
% es pasado sin cambiar.
% Ejemplo:
% ?- intercalar([a,b,c], [d,e], [a,d,b,e,c]). -> true.

% ==============================================================================
% 1. SOLUCIÓN CANÓNICA (Alternancia por Swap de Argumentos)
% ==============================================================================
% Esta es la forma más idiomática y elegante en Prolog:
% En lugar de mantener una variable externa o flag para recordar a quién le toca,
% simplemente se invierte el orden de las listas en la llamada recursiva.
%
% 1. Caso base:
%    Si la primera lista es vacía [], no hay nada que intercalar:
%    todo lo que queda en la segunda lista pasa directamente al resultado L.
intercalar([], L, L).

% 2. Caso recursivo:
%    - Tomamos la cabeza X de la primera lista y la ubicamos al frente: [X|Ys].
%    - Para el siguiente elemento le toca el turno a L2, por lo que llamamos a
%      intercalar pasando L2 al frente y la cola Xs en segundo lugar.
intercalar([X|Xs], L2, [X|Ys]) :-
    intercalar(L2, Xs, Ys).


% ==============================================================================
% 2. SOLUCIÓN ALTERNATIVA (Enfoque en Pares / Lock-step)
% ==============================================================================
% En lugar de procesar de a un elemento por vez, consume simultáneamente
% un elemento de cada lista en cada paso recursivo: [X, Y | Zs].
%
% 1. Caso base A: Ambas listas o la primera se vacían:
intercalarPares([], L, L).

% 2. Caso base B: La segunda lista se vacía pero la primera aún tiene elementos:
intercalarPares([X|Xs], [], [X|Xs]).

% 3. Caso recursivo: Ambas tienen elementos, tomamos X de L1 e Y de L2:
intercalarPares([X|Xs], [Y|Ys], [X, Y | Zs]) :-
    intercalarPares(Xs, Ys, Zs).


% ==============================================================================
% 3. ANÁLISIS DE LA SOLUCIÓN PROPUESTA INICIAL Y POR QUÉ FALLA
% ==============================================================================
% Código de la propuesta inicial:
%
%   intercalarIntento([], L, L).
%   intercalarIntento([X|Xs], L2, [X|Ys]) :- intercalarIntento(Xs, L2, Ys).
%   intercalarIntento(L1, [X|Xs], [X|Ys]) :- intercalarIntento(L1, Xs, Ys).
%
% ¿Por qué esta solución termina comportándose como un append?
% -----------------------------------------------------------
% 1. Estrategia de búsqueda de Prolog (Top-down, Depth-First Search):
%    Prolog siempre prueba las cláusulas en estricto orden textual (de arriba a abajo).
% 2. Como la regla recursiva 1 (`intercalarIntento([X|Xs], L2, ...)`) unifica
%    siempre que la primera lista tenga al menos un elemento, Prolog SIEMPRE
%    elegirá esa regla primero.
% 3. Por lo tanto, consume TODO L1 de forma continua hasta vaciarlo.
% 4. Una vez que L1 se vacía ([]), recién ahí unifica con el caso base `([], L, L)`
%    y pega toda la lista L2 al final.
% 5. Resultado: en vez de intercalar [a,b,c] y [d,e] dando [a,d,b,e,c],
%    devuelve [a,b,c,d,e] (¡exactamente append!).
% 6. Al pedir más soluciones por backtracking con ';', recién ahí entra a la
%    segunda regla, generando mezclas desordenadas (shuffles no alternados).
%
% ¿Por qué la idea del flag 'F' era buena en concepto pero innecesaria en código?
% -----------------------------------------------------------------------------
% La intuición del usuario ("necesito una bandera F que valga 0 o 1 para alternar
% el turno de a qué lista le saco la cabeza") es lógicamente correcta para imponer
% alternancia estricta.
% Sin embargo, en el paradigma lógico no hace falta ensuciar el predicado con
% parámetros de estado o banderas: rotar los argumentos (Solución 1: pasar L2 al
% frente) cambia el turno de forma natural y estructural.


% ==============================================================================
% 4. ANÁLISIS DE REVERSIBILIDAD (Pregunta de la guía)
% ==============================================================================
% "Indicar la reversibilidad, es decir si es posible obtener L3 a partir de L1 y L2,
%  y viceversa."
%
% 1. De (L1, L2) -> L3 (+L1, +L2, -L3):
%    Es determinista y obtiene la única lista intercalada:
%    ?- intercalar([a,b,c], [d,e], L3).
%    L3 = [a, d, b, e, c].
%
% 2. Viceversa, de L3 -> (L1, L2) (-L1, -L2, +L3):
%    ES COMPLETAMENTE REVERSIBLE.
%    Dada una lista intercalada L3, el motor genera por backtracking todos los
%    pares de listas (L1, L2) que al intercalarse producen L3 (incluyendo casos
%    donde una lista terminó antes que la otra):
%    ?- intercalar(L1, L2, [a, d, b, e, c]).
%    L1 = [], L2 = [a, d, b, e, c] ;
%    L1 = [a, d, b, e, c], L2 = [] ;
%    L1 = [a], L2 = [d, b, e, c] ;
%    L1 = [a, b, e, c], L2 = [d] ;
%    L1 = [a, b], L2 = [d, e, c] ;
%    L1 = [a, b, c], L2 = [d, e] ;
%    false.
%
% 3. Modos mixtos (+L1, -L2, +L3) y (-L1, +L2, +L3):
%    También son reversibles: dada la lista final y una de las partes, deduce
%    exactamente cuál era la otra lista faltante.