desde(X,X).
desde(X,Y) :- N is X+1, desde(N, Y).

desdeReversible(X, X).
desdeReversible(X, Y) :- var(Y), N is X+1, desdeReversible(N, Y).
desdeReversible(X, Y) :- nonvar(Y), X < Y.
