% - last(?L, ?U)
last([X], X).
last([_|Ls], Y) :- last(Ls, Y). 

% - last(L, U) :- append(_, [U], L).

% - reverse(+L, ?R)
reverse([], []).
reverse([X|L], P) :- reverse(L, R), append(R, [X], P).

% - prefijo(?P, +L) donde P es prefijo de la lista L.
prefijo([], _).
% - prefijo(P, L) :- append(P, _, L).
prefijo([H|T1], [H|T2]) :- prefijo(T1, T2).

% - sufijo
sufijo(Y, X) :- reverse(X, Z), prefijo(R,Z),reverse(R, Y).

% - sublista(?S, +L) donde S es Sublista de L
sublista([], _).
sublista([H|T1], [H|T2]) :- sublista(T1, T2).
sublista([H|T1], [_|T2]) :- sublista([H|T1], T2).

% - pertenece(?X, +L): True sii X pertenece a L.
pertenece(X, [X|_]).
pertenece(X, [_|T]) :- pertenece(X, T).