arbol(bin(bin(bin(nil,4,nil),2,bin(nil,5,bin(nil,6,nil))),1,bin(bin(nil,7,nil),3,nil))).

% rama(+A, -C).
rama(nil, []).
% Solo nos importa que el primer elemento sea la raiz del subarbol
rama(bin(I, X, _), [X | Xs]) :- rama(I, Xs).
rama(bin(_, X, D), [X | Xs]) :- rama(D, Xs).

% b. Es reversible? No se que es reversible.

% ramaMasLarga(+A, -C).
ramaMasLarga(nil, []).
ramaMasLarga(A, C) :-
    rama(A, C),
    length(C, L1),
    not((
        rama(A, C2),
        length(C2, L2),
        L2 > L1
    )).

% ramaUnicaDeLong(+A, +N, -C).
ramaUnicaDeLong(nil, _, []).
ramaUnicaDeLong(_, 0, []).
ramaUnicaDeLong(A, N, C) :-
    rama(A, C),
    length(C, N),
    not((
        rama(A, C2),
        length(C2, L2),
        L2 == N,
        C2 \= C
    )).