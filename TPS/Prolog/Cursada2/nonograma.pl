% Ejercicio 6
pintarObligatorias(R) :-
    R = r(_, Celdas),
    findall(Celdas, pintadasValidas(R), Soluciones),
    maplist(mismoLen(Celdas), Soluciones),
    transponer(Soluciones, SolucionesT),
    maplist(unificarSiSonIguales, SolucionesT, Celdas).

mismoLen(L1, L2) :-
    length(L1, N),
    length(L2, N).

unificarSiSonIguales([X|XS], X) :- sonTodosDeL(X, XS).
unificarSiSonIguales([X|XS], _) :- not(sonTodosDeL(X, XS)).
     
% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
% ... existing code ...
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B).