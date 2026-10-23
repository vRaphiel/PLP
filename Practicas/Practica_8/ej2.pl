% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 2
% Paradigmas de Programación (PLP)
% ==============================================================================

% vecino(?X, ?Y, +Lista) / vecino(?X, ?Y, ?Lista)
% Verdadero si X e Y son elementos contiguos e inmediatos en Lista (X precede a Y).
vecino(X, Y, [X|[Y|_Ls]]).
vecino(X, Y, [_W|Ls]) :- 
    vecino(X, Y, Ls).

% ------------------------------------------------------------------------------
% i. Árbol de búsqueda de Prolog para la consulta:
%    ?- vecino(5, Y, [5, 6, 5, 3]).
% ------------------------------------------------------------------------------
/*
Reglas:
  (R1) vecino(X, Y, [X, Y | Ls]).
  (R2) vecino(X, Y, [W | Ls]) :- vecino(X, Y, Ls).

Árbol SLD:

                                ?- vecino(5, Y, [5, 6, 5, 3]).
                               /                              \
       (R1) {X1:=5, Y:=6, Ls1:=[5,3]}                      (R2) {X2:=5, Y2:=Y, W2:=5, Ls2:=[6,5,3]}
                     |                                                    |
                   true.                                        ?- vecino(5, Y, [6, 5, 3]).
             [Solución 1: Y = 6]                               /                           \
                                      (R1) {X3:=5, 5=6 FALLA}                   (R2) {X4:=5, W4:=6, Ls4:=[5,3]}
                                                |                                                  |
                                              false.                                     ?- vecino(5, Y, [5, 3]).
                                                                                        /                        \
                                                              (R1) {X5:=5, Y:=3, Ls5:=[]}         (R2) {X6:=5, W6:=5, Ls6:=[3]}
                                                                           |                                       |
                                                                         true.                           ?- vecino(5, Y, [3]).
                                                                   [Solución 2: Y = 3]                  /                     \
                                                                             (R1) {[3] no unifica con [X,Y|Ls]}  (R2) {W8:=3, Ls8:=[]}
                                                                                               |                            |
                                                                                             false.                ?- vecino(5, Y, []).
                                                                                                                  /                    \
                                                                                                            (R1) ([] falla)       (R2) ([] falla)
                                                                                                                  |                    |
                                                                                                                false.               false.

Valores de Y devueltos por el motor:
  1) Y = 6 ;
  2) Y = 3 ;
  false.
*/

% ------------------------------------------------------------------------------
% ii. Si se invierte el orden de las reglas:
% ------------------------------------------------------------------------------
/*
Consideremos el programa con las reglas invertidas:
  vecinoInv(X, Y, [W|Ls]) :- vecinoInv(X, Y, Ls).   % (R2 primero)
  vecinoInv(X, Y, [X|[Y|Ls]]).                      % (R1 segundo)

1. ¿Los resultados son los mismos?
   SÍ. Desde el punto de vista declarativo y semántico (conjunto de éxitos /
   modelo de Herbrand), las soluciones son exactamente las mismas: {Y = 6, Y = 3}.
   Dado que la lista [5, 6, 5, 3] es finita, el árbol de búsqueda sigue siendo
   finito (acotado por la longitud de la lista) y no se producen bucles infinitos.

2. ¿Y el orden de los resultados?
   NO, el orden de los resultados se invierte.
   Prolog utiliza una estrategia de búsqueda primero en profundidad (DFS)
   evaluando las cláusulas en orden top-down (de arriba hacia abajo).
   Al colocar la regla recursiva primero:
     - Prolog desciende recursivamente consumiendo la lista hasta alcanzar la cola.
     - Encuentra primero el vecino en la posición más profunda de la lista.
   Por lo tanto, las soluciones se obtienen en el orden inverso:
     1) Y = 3 ;
     2) Y = 6 ;
     false.
*/
