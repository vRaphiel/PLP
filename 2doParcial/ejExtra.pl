parPositivo(X, Y) :- mayor(X, 0), mayor(Y, 0).
natural(0).
natural(succ(N)) :- natural(N).
mayor(succ(N), 0) :- natural(N).
mayor(succ(X), succ(Y)) :- mayor(X, Y).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

esSublista(_, []).
esSublista(L, [X|XS]) :- member(X, L), esSublista(L, XS).

seFormaCon([], _).
seFormaCon([A|AS], B) :- member(A, B), seFormaCon(AS, B).

collatz(N, N) :- N > 0.
collatz(N, S) :- N > 1,
    esPar(N),
    N1 is N // 2,
    collatz(N1, S).
collatz(N, S) :- N > 1,
    esImpar(N),
    N1 is 3 * N + 1,
    collatz(N1, S).

esPar(N) :- N mod 2 =:= 0.
esImpar(N) :- N mod 2 =:= 1.

palabra(_, 0, []).
palabra(A, N, P) :-
    length(P, N),
    hechoDeAlfabeto(A, P).

hechoDeAlfabeto(_, []).
hechoDeAlfabeto(A, [I|IS]) :- member(I, A), hechoDeAlfabeto(A, IS).

desde(X, X).
desde(X, Y) :- var(Y), N is X+1, desde(N, Y).
desde(X, Y) :- nonvar(Y), X < Y.

frase(A, P) :-
    desde(0, N),
    fraseConNPalabras(A, N, P).

% fraseConNPalabras(+A, +N, -Frase)
% Genera una frase de N palabras no vacías
fraseConNPalabras(_, 0, []).
fraseConNPalabras(A, N, [P | Resto]) :-
    N > 0,
    desde(1, L),               % longitud de palabra: mínimo 1 (no vacía)
    palabra(A, L, P),         % palabra de longitud L con letras en A
    N1 is N - 1,
    fraseConNPalabras(A, N1, Resto).