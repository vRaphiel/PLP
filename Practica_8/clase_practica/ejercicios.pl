% iesimo(+I, +L, -X)
iesimo(0, [X | _], X).
iesimo(I, [_ | L], X) :- J is (I - 1), iesimo(J, L, X).

desde(X, X).
desde(X, Y) :- N is X + 1, desde(N ,Y).

generar(X, Y) :- 
    Max = 100,
    between(1, Max, X),
    between(1, Max, Y).

test(X,Y) :-
    1 is gcd(X,Y).

coprimos(X,Y) :-
    generar(X,Y),
    test(X,Y).