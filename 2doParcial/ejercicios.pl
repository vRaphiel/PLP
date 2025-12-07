% 2doParcial-1erCuat
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

% 2doRecu-2doCuat
% --------- BASE DE DATOS DE OBJETOS ---------

% objeto(ID, Peso, Valor).
objeto(1, 50, 10).
objeto(2, 75, 15).
objeto(3, 60, 5).
objeto(4, 10, 1).

% --------- MOCHILA PRINCIPAL ---------

% mochila(+Capacidad, -ListaDeIDs)
mochila(C, L) :-
    listaObjetos(C, ObjetosValidos),
    unaMochila(C, ObjetosValidos, L).

% --------- FILTRAR OBJETOS POR PESO ---------

% listaObjetos(+Capacidad, -ListaFiltrada)
listaObjetos(C, Lista) :-
    listaObjetosAux(C, Lista, []).

% acumulador: lista ya incluida
% caso base
listaObjetosAux(_, [], _).

% tomar objeto si no está repetido y entra por peso
listaObjetosAux(C, [(ID, P, V) | Resto], Acum) :-
    objeto(ID, P, V),
    P =< C,
    not(member((ID, P, V), Acum)),
    listaObjetosAux(C, Resto, [(ID, P, V) | Acum]).

% o no tomar el objeto actual
listaObjetosAux(C, Resto, Acum) :-
    objeto(ID, P, V),
    not(member((ID, P, V), Acum)),
    listaObjetosAux(C, Resto, [(ID, P, V) | Acum]).

% --------- SELECCIONAR UNA MOCHILA ---------

% unaMochila(+Capacidad, +Objetos, -ListaIDsElegidos)
unaMochila(_, [], []).

% incluir el objeto si entra
unaMochila(C, [(ID, P, _) | R], [ID | L]) :-
    P =< C,
    C1 is C - P,
    unaMochila(C1, R, L).

% o no incluirlo
unaMochila(C, [_ | R], L) :-
    unaMochila(C, R, L).

% Extras
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

% ----- RECUPERATORIO DEL SEGUNDO PARCIAL DE 2022
% sublistaMasLargaDePrimos(+L, ?P)
sublistaMasLargaDePrimos(L, P) :- 
    segmentoPrimosContiguos(L, P),
    length(P, L1),
    not((
        segmentoPrimosContiguos(L, P2),
        length(P2, L2),
        L2 > L1
    )).
segmentoPrimosContiguos(L, P) :- esUnaSubLista(P, L), P \= [], todosPrimos(P).
esUnaSubLista(P, L) :- append(_, R, L), append(P, _, R).
todosPrimos([]).
todosPrimos([X|XS]) :- esPrimo(X), todosPrimos(XS).

esPrimo(2).
esPrimo(N) :- 
    N > 1, 
    not((
        N2 is N - 1,
        between(2, N2, D),
        0 =:= N mod D
    )).

% simbolo(?S)
simbolo(a).
simbolo(b).

% clausura(-L)
clausura(L) :- desde(0, LN), length(L, LN), esClausuraKleene(L).

esClausuraKleene([]).
esClausuraKleene([S|R]) :- simbolo(S), esClausuraKleene(R).

% ------------ Montaña
% montaña(+L, -L1, C, -L2)
montania(L, L1, C, L2) :- 
    armarUnaMontania(L, L1, C, L2), 
    reverse(L1, L1A),
    esDecrecienteEstricto(L1A, C, C),
    esDecrecienteEstricto(L2, C, C).

armarUnaMontania(L, L1, C, L2) :-
    obtenerElMaximo(L, C),
    append([C], L2, L2AUX),
    append(L1, L2AUX, L).
    
obtenerElMaximo([X], X).
obtenerElMaximo([X,Y|XS], Max) :-
    mayor(X, Y, M),
    obtenerElMaximo([M|XS], Max).

mayor(A, B, A) :- A >= B.
mayor(A, B, B) :- B > A.

esDecrecienteEstricto([X], Y, _) :- Y > X.
esDecrecienteEstricto([X|XS], Y, C) :- C > X, Y > X, esDecrecienteEstricto(XS, X, C).

todasLasListas(A, L) :-
    desde(0, X ),
    length(L, X),
    esListaDeA(A, L).

% 3. Predicado de Verificación (Corregido)
esListaDeA(_, []).
esListaDeA(A, [X|XS]) :-
    member(X, A),
    esListaDeA(A, XS).