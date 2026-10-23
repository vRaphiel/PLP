% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 8
% Paradigmas de Programación (PLP)
% ==============================================================================

% parteQueSuma(+L, +S, -P)
%
% Verdadero cuando P es una subsecuencia de elementos de L cuyos valores suman S.
%
% Este problema corresponde al problema del subconjunto (Subset Sum) resuelto
% mediante búsqueda con retroceso (Backtracking) y poda:
%
% 1. Caso base (Éxito al alcanzar la suma exacta):
%    Cuando la suma restante es 0, no necesitamos agregar ningún elemento más:
%    el resultado a completar es la lista vacía [].
%    Notar el uso de la variable anónima (_) en el primer argumento:
%    - Si S llegó a 0, no nos importa si sobraron elementos en L o si L se consumió
%      entera ([]). En ambos casos la combinación encontrada es válida.
%    - Si L se vacía pero S > 0, ninguna regla unifica, por lo que Prolog falla
%      naturalmente (CWA - Supuesto de Mundo Cerrado) sin necesidad de reglas extra.
parteQueSuma(_, 0, []).

% 2. Caso recursivo A (Decidimos INCLUIR a X en la solución):
%    Para que tenga sentido tomar X:
%    - S debe ser positivo (S > 0).
%    - X no debe superar a la suma restante (X =< S), lo cual poda ramas inválidas.
%    - El nuevo objetivo de suma para la cola Xs será J is S - X.
%    - X se coloca en la cabeza de la lista de salida: [X|Ys].
parteQueSuma([X|Xs], S, [X|Ys]) :-
    S > 0,
    X =< S,
    J is S - X,
    parteQueSuma(Xs, J, Ys).

% 3. Caso recursivo B (Decidimos DESCARTAR a X de la solución):
%    Ignoramos a X (representado por _) y continuamos buscando que elementos de
%    la cola Xs sumen la misma cantidad S sin modificar.
parteQueSuma([_|Xs], S, P) :-
    S > 0,
    parteQueSuma(Xs, S, P).

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA Y ÁRBOL DE BACKTRACKING
% ------------------------------------------------------------------------------
%
% Consulta del enunciado:
% ?- parteQueSuma([1, 2, 3, 4, 5], 9, P).
% P = [1, 3, 5] ;
% P = [2, 3, 4] ;
% P = [4, 5] ;
% false.
%
% Esquema de decisión para cada elemento X ante la meta ?- parteQueSuma([X|Xs], S, ...):
%
%                              [X | Xs], S
%                             /           \
%               (Tomar X, si X =< S)      (Descartar X)
%                       /                       \
%           Xs, S' is S - X                    Xs, S
%
% - Si S llega a 0: éxito inmediato con [].
% - Si Xs se vacía y S > 0: la rama falla y retrocede (backtracking).