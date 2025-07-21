% filaQueSumaK(+N, +K, -F): Instancia F en todas las filas de largo N que suman K.
filaQueSumaK(0, 0, []).
filaQueSumaK(N, K, [X|Xs]) :-
    N > 0,
    between(0, K, X),
    K2 is K - X,
    N2 is N - 1,
    filaQueSumaK(N2, K2, Xs).

filasQueSumaK(_, _, []).
filasQueSumaK(N, K, [X|Xs]) :-
    filaQueSumaK(N, K, X),
    filasQueSumaK(N, K, Xs).

cuadradoSemiMagico(N, L) :-
    desde(0, K),
    length(L, N),
    filasQueSumaK(N, K, L).

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).