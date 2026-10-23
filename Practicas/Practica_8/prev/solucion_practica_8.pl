% Ejercicio 1
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X, Y) :- padre(X,Z) , padre(Z,Y).

abuelo(X, manuel).

hijo(X, X) :- false.
hijo(X, Y) :- padre(Y, X).
hermano(X, Y) :- hijo(X, Z), padre(Z, Y).
descendiente(X, Y) :- hijo(Z, Y), hijo(X, Z).

% - Para encontrar a los nietos de juan debo 
% - devolver los hijos de los hijos de juan

nietos(X, Y) :- hijo(Z, Y), hijo(X, Z), not(hijo(X, Y)).

% - ancestro(X, X).
% - ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% - Si se pide mas de un resultado lo va a dar, hasta que ya no haya nadie en la
% - cadena del arbol genealogico. Luego se cuelga.

% - Una posible solucion es ver que si ya no hay mas hijos, no se devuelva nada
ancestro(X, X).
ancestro(X, Y) :- padre(X, Y).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).

% Ejercicio 2
vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X,Y,Ls).

/*
?- vecino(5, Y, [5,6,5,3]).
├─ ?- vecino(5, 6, [5|[6|[5,3]]]).          {X := 5, Y := 6, Ls := [5,3]}
│  └─ true.                                 {Y := 6}
└─ ?- vecino(5, Y, [6,5,3]).                {X := 5, W := 5, Ls := [6,5,3]}
   └─ ?- vecino(5, Y, [5,3]).               {X := 5, W := 6, Ls := [5,3]}
      ├─ ?- vecino(5, 3, [5|[3|[]]]).       {X := 5, Y := 3, Ls := []}
      │  └─ true.                           {Y := 3}
      └─ ?- vecino(5, Y, [3]).              {X := 5, W := 5, Ls := [3]}
         └─ ?- vecino(5, Y, []).            {X := 5, W := 3, Ls := []}
            └─ false.
*/

/*
Si pero en orden inverso. Porque primero va a hacer los casos recursivos y luego va a hacer cada caso donde encuentre los vecinos.
*/

vecino2(X, Y, [W|Ls]) :- vecino(X,Y,Ls).
vecino2(X, Y, [X|[Y|Ls]]).

% Ejercicio 4
%- juntar(?Lista1, ?Lista2, ?Lista3).
%- juntar([], [], []).
%- juntar([X|Ls], [], [X|Ls]).
%- juntar([], [Y|Ls], [Y|Ls]).
%- juntar([X|Ls1], Ls2, [X|Ls3]) :- juntar(Ls1, Ls2, Ls3). 

% - No esta mal, pero hice mas predicados
% - Lo correcto seria:
juntar([], L2, L2).
juntar([X|L1], L2, [X|L3]) :- juntar(L1, L2, L3).

% Ejercicio 5
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

% Ejercicio 6
% - aplanar(+XS, -YS). True sii YS contiene los elementos de todos los niveles de XS en el mismo orden de aparicion.
aplanar([], []).
% H puede ser cualquier cosa. Entero, Atomo, Lista.
% Si H es lista, el append se hace bien. Si no, es una nueva historia.
% Recursivamente tenemos que hacer uso del append, pero esta vez checkeando el tipo.
% Podemos juntar dos listas. Pero lo que no podemos hacer

% Caso H es un atomo o un entero:
% Como H es un atomo o entero, aplanamos la cola.
% Luego de aplanar la cola, tenemos el resultado en Z.
% Con el Z vamos a hacer un append de lo que tenemos hasta ahora a modo de lista con lista y ese sera nuestro L

% RARO, pero creo que veo la idea.

aplanar([Hs|Ts], L) :-
    aplanar(Hs, Zs),
    aplanar(Ts, Xs),
    append(Zs, Xs, L).

aplanar([H|T], L) :- 
    not(aplanar(H, _)),
    aplanar(T, Z),
    append([H], Z, L).

% Ejercicio 7
interseccion([], _, []). 

% - H no pertenece a L2, seguimos con nuestra historia
interseccion([H|L1], L2, L3) :- 
    not(member(H, L2)),
    interseccion(L1, L2, L3).

% - H pertenece a L2, entonces vemos que pasa con lo demas
interseccion([H|L1], L2, [H|L3]) :- 
    member(H, L2),
    interseccion(L1, L2, L3).

% - Partir(N,L,L1,L2) donde L1 tiene los primeros N elementos de L y L2 el resto.
partir(0, L, [], L).
partir(N, [H|T], [H|T1], L2) :- M is N - 1, partir(M, T, T1, L2).

% - borrar(+LO, +X, -LS): Elimina ocurrencias de X de la lista original
borrar([], _, []).
% Como en cada paso recursivo vemos la cabeza de la lista, si es el mismo elemento, lo quitamos.
borrar([X|LO], X, LS) :- borrar(LO, X, LS).
borrar([Y|LO], X, [Y|LS]) :- X \= Y, borrar(LO, X, LS).

% - sacarDuplicados(+L1, -L2): Saca duplicados de L1.
sacarDuplicados([], []).
sacarDuplicados([H|T], [H|L2]) :- member(H, T), borrar(T, H, Z), sacarDuplicados(Z, L2).
sacarDuplicados([H|T], [H|L2]) :- not(member(H, T)), sacarDuplicados(T, L2).

% - permutacion(+L1, ?L2) tiene exito si L2 es permutacion de L1
permutacion([], []).
permutacion([X|Xs], Ys) :- permutacion(Xs, Zs), insertar(X, Zs, Ys).

insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

% - Reparto(+L, +N, -LListas) que tiene exitos si LListas es de N listas de cualquier longitud tales que al concatenarlas se convierte en la lista L.
reparto([], 0, []).
reparto(L, N, [X|LL]) :- N > 0, append(X, L2, L), N2 is N-1, reparto(L2, N2, LL).

% - RepartoSinVacias
repartoSinVacias(L, Xs) :-
    length(L, N),
    between(1, N, K),   % Generamos todas los posibles K = cantidades de sublistas.
    reparto(L, K, Xs),  % Repartimos en K sublistas.
    not((member(X, Xs), length(X, 0))). % No pueden haber sublistas vacías.

% Ejercicio 9
desde(X,X).
desde(X,Y) :- N is X+1, desde(N, Y).

desdeReversible(X, X).
desdeReversible(X, Y) :- var(Y), N is X+1, desdeReversible(N, Y).
desdeReversible(X, Y) :- nonvar(Y), X < Y.

% Ejercicio 11
% Un árbol binario se representará en Prolog con:
% nil, si es vacio.
% bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.

% vacio(+X)
vacio(nil).

% raiz(bin(?I, +R, ?D))
raiz(bin(_, V, _), V).

% altura
altura(nil, 0).
altura(bin(I, _, D), AL) :- altura(I, AI), altura(D, AD), AL is 1+max(AI, AD).

% cantidadDeNodos
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I,_,D), CN) :- cantidadDeNodos(I, CI), cantidadDeNodos(D, CD), CN is 1 + CI + CD.

% Ejercicio 12
inorder(nil, []).
inorder(bin(I, R, D), L) :- inorder(I, I1), inorder(D, I2), append(I1, [R|I2], L).

arbolConInorder([], nil).
arbolConInorder(L, bin(I, R, D)) :- 
    append(LI, [X, LD], L),
    arbolConInorder(LI, I),
    arbolConInorder(LD, D).

aBB(nil).
aBB(bin(nil, 0, nil)).
aBB(bin(I, R, D)) :- 
    inorder(I, LI), 
    max_list(LI, M1), M1 < R, 
    inorder(D, L2),
    max_list(L2, M2),
    R < M2.

aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(X, bin(I, X, D), bin(I, X, D)).
aBBInsertar(X, bin(I, Y, D), bin(T, Y, D)) :- X < Y, aBBInsertar(X, I, T).
aBBInsertar(X, bin(I, Y, D), bin(I, Y, T)) :- X > Y, aBBInsertar(X, D, T).

% Ejercicio 14i
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

% Ejercicio 14ii
:- ensure_loaded('ejercicio14i.pl').

% cuadradoMagico(+N, -C)
cuadradoMagico(N, C) :-
    cuadradoSemiMagico(N, C),
    nth1(1, C, F1),
    sum_list(F1, K),
    not((between(1, N, I), columna(I, C, X), sum_list(X, K2), K2 \== K)).

% columna(+I, +Fs, -Xs): instancia Xs en la columna I-ésima de la matriz Fs.
columna(_, [], []).
columna(I, [F|Fs], [X|Xs]) :- nth1(I, F, X), columna(I, Fs, Xs).

% Ejercicio 15
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

% Ejercicio 16
frutal(frutilla).
frutal(banana).
frutal(manzana).
cremoso(banana).
cremoso(americana).
cremoso(frutilla).
cremoso(dulceDeLeche).
leGusta(X) :- frutal(X), cremoso(X).
cucurucho(X,Y) :- leGusta(X), leGusta(Y).

% Ejercicio 18
corteMasParejo(L, L1, L2) :-
    unCorte(L, L1, L2, D1),
    not((unCorte(L, _, _, D2), D2 < D1)).

unCorte(L, L1, L2, D) :-
    append(L1, L2, L),
    sum_list(L1, S1),
    sum_list(L2, S2),
    D is abs(S1 - S2).

% Ejercicio 20
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
