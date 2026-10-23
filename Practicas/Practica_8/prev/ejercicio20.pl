desde(X, X).
desde(X, Y) :- var(Y), N is X+1, desde(N, Y).
desde(X, Y) :- nonvar(Y), X < Y.

esPrimo(P) :- P > 1, P2 is P-1, not((between(2, P2, D), mod(P, D) =:= 0)).

divisores(N, D) :-
    between(2, N, D),
    N mod D =:= 0.

divisoresPrimos(N, Lista) :-
    findall(D, (divisores(N, D), esPrimo(D)), Lista).

elCuadradoDivide(N, X) :- 
    J is X * X,
    N mod J =:= 0.

esNumeroPoderoso(N) :- 
    divisoresPrimos(N, L),
    forall(member(X, L), elCuadradoDivide(N, X)).

proximoNumeroPoderoso(X, Y) :-
    NX is X + 1,
    esNumeroPoderoso(NX),
    Y is NX.
proximoNumeroPoderoso(X, Y) :-
    NX is X + 1,
    not(esNumeroPoderoso(NX)),
    proximoNumeroPoderoso(NX, Y).