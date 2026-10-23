% EJERCICIO 4
% juntar(?Lista1, ?Lista2, ?Lista3)
%
% Verdadero si Lista3 es la concatenación en orden de Lista1 y Lista2. Equivale a la implementación canónica del predicado estándar append/3.
%
% Principio de inducción estructural sobre la primera lista (Lista1):
% 1. Caso base:
%    Concatenar la lista vacía [] con cualquier lista L da como resultado L.
%    juntar([], L, L).
%
% 2. Caso recursivo:
%    Concatenar una lista no vacía [X|Xs] con L da como resultado una lista
%    cuya cabeza es X y cuya cola Ys es el resultado de juntar el resto Xs con L.
%    juntar([X|Xs], L, [X|Ys]) :- juntar(Xs, L, Ys).
%
% NOTA SOBRE EVITAR CHOICE POINTS BASURA / REGLAS REDUNDANTES:
% No se debe incluir la cláusula redundante `juntar(L, [], L).` porque:
% - Cuando L = [], se solaparía con `juntar([], L, L)`, generando múltiples
%   soluciones idénticas para consultas como `juntar([], [], L)`.
% - El caso donde la segunda lista es vacía ya queda naturalmente cubierto
%   por la descomposición inductiva de la primera lista hasta alcanzar `[]`:
%   juntar([1, 2], [], L)
%     -> [1 | Ys1] con juntar([2], [], Ys1)
%     -> [1, 2 | Ys2] con juntar([], [], Ys2)  [unifica con el caso base: Ys2 = []]
%     -> L = [1, 2].
%   Agregar reglas adicionales genera ramas de backtracking innecesarias y duplicados.

juntar([], L, L).
juntar([X|Xs], L, [X|Ys]) :-
    juntar(Xs, L, Ys).

% ------------------------------------------------------------------------------
% ANÁLISIS DE REVERSIBILIDAD Y CONSULTAS TÍPICAS
% ------------------------------------------------------------------------------
%
% 1. Concatenación (+, +, -):
%    ?- juntar([a,b,c], [d,e], L).
%    L = [a,b,c,d,e].
%    (Determinista: tras unificar los 3 elementos de la primera lista y el caso base,
%     no quedan puntos de elección pendientes).
%
% 2. Verificación (+, +, +):
%    ?- juntar([a,b,c], [d,e], [a,b,c,d,e]).
%    true.
%
% 3. Búsqueda de sufijo (+, -, +):
%    ?- juntar([a,b,c], L, [a,b,c,d,e]).
%    L = [d,e].
%
% 4. Búsqueda de prefijo (-, +, +):
%    ?- juntar(L, [d,e], [a,b,c,d,e]).
%    L = [a,b,c] ;
%    false.
%
% 5. Generación de todas las particiones / cortes de una lista (-, -, +):
%    ?- juntar(L1, L2, [1, 2, 3]).
%    L1 = [],        L2 = [1, 2, 3] ;
%    L1 = [1],       L2 = [2, 3] ;
%    L1 = [1, 2],    L2 = [3] ;
%    L1 = [1, 2, 3], L2 = [] ;
%    false.
%
%    Árbol de resolución SLD simplificado para ?- juntar(L1, L2, [1, 2]):
%
%                               ?- juntar(L1, L2, [1, 2]).
%                              /                          \
%           (R1: L1=[], L2=[1,2])                          (R2: L1=[1|Xs], Ys=[2])
%                     |                                              |
%                   true.                                  ?- juntar(Xs, L2, [2]).
%       [Sol 1: L1=[], L2=[1,2]]                          /                       \
%                                      (R1: Xs=[], L2=[2])                         (R2: Xs=[2|Xs2], Ys2=[])
%                                               |                                             |
%                                             true.                                 ?- juntar(Xs2, L2, []).
%                                 [Sol 2: L1=[1], L2=[2]]                          /                       \
%                                                               (R1: Xs2=[], L2=[])                 (R2: Xs2=[_|...], []=[_|_] FALLA)
%                                                                        |
%                                                                      true.
%                                                           [Sol 3: L1=[1,2], L2=[]]
%
% 6. Generación de pares infinitos (-, -, -):
%    ?- juntar(L1, L2, L3).
%    Genera todas las combinaciones posibles de listas en orden creciente
%    de longitud de L1:
%    L1 = [], L3 = L2 ;
%    L1 = [_A], L3 = [_A|L2] ;
%    L1 = [_A, _B], L3 = [_A, _B|L2] ;
%    ...