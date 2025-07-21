desde(X, X).
desde(X, Y) :- var(Y), N is X+1, desde(N, Y).
desde(X, Y) :- nonvar(Y), X < Y.

esTriangulo(tri(A, B, C)) :- A < B+C, B < A+C, C < A+B, A > B-C.

% perimetro(?T, ?P).
% Esta el triangulo instanciado. P esta ligada (Instanciada).
% Esta el triangulo instanciado. P esta libre (No Instanciada).
perimetro(tri(A,B,C), P) :- 
    ground(tri(A,B,C)), 
    esTriangulo(tri(A, B, C)),
    P is A + B + C.

% El triangulo no esta instanciado. P esta ligada (Instanciada).
perimetro(tri(A,B,C), P) :- 
    not(ground(tri(A, B, C))),
    armarTriplas(A, B, C, P),
    esTriangulo(tri(A,B,C)).

armarTriplas(A, B, C, P) :-
    desde(3, P),
    between(1, P, A),
    between(1, P, B),
    between(1, P, C),
    P =:= A+B+C.

triangulo(T) :- perimetro(T, _).