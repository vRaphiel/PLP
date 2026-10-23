% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 6
% Paradigmas de Programación (PLP)
% ==============================================================================

% aplanar(+Xs, -Ys)
%
% Verdadero sii Ys contiene los elementos de todos los niveles de Xs en el mismo
% orden de aparición. Xs puede contener enteros, átomos o listas anidadas con
% profundidad arbitraria.
%
% Casos disjuntos por estructura:
%
% 1. Caso base: La lista vacía aplanada es la lista vacía.
aplanar([], []).

% 2. Caso recursivo (cabeza es una lista):
%    Si la cabeza Zs es a su vez una lista (incluso []), aplanar(Zs, AZs)
%    tendrá éxito. Se aplana la cabeza en AZs, se aplana la cola Xs en Rs,
%    y se concatenan ambos resultados con append/3.
aplanar([Zs|Xs], Ys) :-
    aplanar(Zs, AZs),
    aplanar(Xs, Rs),
    append(AZs, Rs, Ys).

% 3. Caso recursivo (cabeza es un átomo / entero / no es lista):
%    Si X no se puede aplanar (not(aplanar(X, _))), significa que X es un
%    elemento atómico. En este caso, X se conserva directamente en la cabeza
%    del resultado y se continúa aplanando la cola Xs.
aplanar([X|Xs], [X|Rs]) :-
    not(aplanar(X, _)),
    aplanar(Xs, Rs).

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% ?- aplanar([a, [3, b, []], [2]], L).
% L = [a, 3, b, 2].
%
% ?- aplanar([[1, [2, 3], [a]], [[[]]]], L).
% L = [1, 2, 3, a].
%
% ?- aplanar([[], [], [[]]], L).
% L = [].