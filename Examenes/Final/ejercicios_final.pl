secuenciaRepetida([X], S) :- subLista(X, S).
secuenciaRepetida([X | XS], S) :- subLista(X, S), secuenciaRepetida(XS, S).

subLista([X | XS], [X | YS]) :- append(YS, _, XS).
subLista([_ | XS], L) :- subLista(XS, L).

secuenciaMaxima(M, S) :- secuenciaRepetida(M, S), length(S, LS), 
    not((
        secuenciaRepetida(M, L), 
        length(L, LL), 
        LL > LS
    )).

natural(0).
natural(succ(N)) :- natural(N).
mayor(succ(N), 0) :- natural(N).
mayor(succ(X), succ(Y)) :- mayor(X, Y).

todasLasMatrices(M) :- 
    length(_, X),
    X > 1,
    between(1, X, Y),
    length(M, Y),
    agregarFilas(M, X).

agregarFilas([], _).
agregarFilas([Fila | Resto], Columnas) :-
    length(Fila, Columnas),
    agregarFilas(Resto, Columnas).

% ------
nota(do). 
submelodia(sec(M,N) ,sec(M,N)). 
submelodia(sec(M,_),S) :- submelodia(M,S). 
submelodia(sec(_,N),S) :- submelodia(N,S). 
melodia(M) :- nota(M). 
melodia(sec(M,N)) :- melodia(M), melodia(N). 



% ------ Ancestros
progenitor(diego, dalma).
progenitor(diego, gianinna).
progenitor(tota, diego).
progenitor(chitoro, ana).
progenitor(ana, daniel).
pareja(gianinna, osvaldo).
pareja(chitoro, tota).
pareja(diego, claudio).
pareja(ana, pedro).

pareja(X, Y) :- pareja(Y, X).
ancestro(A, X) :- progenitor(A, X).
ancestro(A, X) :- progenitor(A, Y), ancestro(Y, X).

descendientes(P, L) :-
    buscar_descendientes(P, [], L_desordenada),
    sort(L_desordenada, L).

buscar_descendientes(P, Visitados, L) :-
    ancestro(P, X),
    not(member(X, Visitados)),
    buscar_descendientes(P, [X | Visitados], L).

buscar_descendientes(P, Visitados, Visitados) :-
    not((ancestro(P, X), not(member(X, Visitados)))).

ancestro_comun(P1, P2, A) :-
    ancestro(A, P1),
    ancestro(A, P2).

ancestroEnComunMasCercano(P1, P2, A) :-
    ancestro_comun(P1, P2, A),
    not((
        ancestro_comun(P1, P2, OtroA),
        ancestro(A, OtroA)
    )).


/* 
Ejercicio 3 (Programación Lógica). A las listas comunes de números enteros (e.g., [8,1,1,1, 10,10,2]) se le quiere sumar un nuevo tipo de elemento que son los comprimidos (anotados como c(N,K), donde N es el número y K es la cantidad de repeticiones, con K ≥ 1). Así, podremos generar listas comprimidas, donde se puedan agrupar los elementos contiguos repetidos, indicando la cantidad de valores iguales por medio un par de valores (obs.: si se comprime ciertos elementos contiguos, se deben incluir todos ellos, por lo que, por ejemplo, [8,c(1,2),1,10,10,2] no sería una compresión válida para el caso anterior). No es obligatorio comprimir toda la lista para que esta sea comprimida. Sólo por nombrar algunas, la lista anterior podría tener distintas versiones comprimidas: [c(8,1),c(1,3),10,10,2], [8,c(1,3),c(10,2),2],[c(8,1),c(1,3),c(10,2),c(2,1)],y[8,1,1,1,10,10,2].
- a) Definir un predicado comprimido (+L1, -L2) que dada una lista L1, L2 es una versión comprimida de L1. No deben generarse soluciones L2 idénticas más de una vez.
- b) ¿El predicado comprimido puede ser reversible en uno o ambos argumentos? Justificar detalladamente.
- c) Definir un predicado iguales (+L1, +L2), que indique si dos listas son observacionalmente iguales, considerando que ambas denotan la misma lista de números enteros. Se pueden agregar predicados auxiliares.
*/

% comprimido(+L1, -L2)
comprimido([], []).
comprimido([X | Xs], [c(X, K) | L2Resto]) :-
    tomar_bloque([X | Xs], X, K, _, L1Resto),
    comprimido(L1Resto, L2Resto).

tomar_bloque([X], X, 1, [X], []).
tomar_bloque([X, X | Xs], X, K, [X | Bloque], Resto) :- 
    tomar_bloque([X | Xs], X, K1, Bloque, Resto), 
    K is K1 + 1.
tomar_bloque([X, Y | Xs], X, 1, [X], [Y | Xs]) :- X \= Y.

iguales(L1, L2) :-
    descomprimir(L1, Base),
    descomprimir(L2, Base).

descomprimir([], []).
descomprimir([c(X, K) | Xs], Base) :-
    generar_repetidos(X, K, Bloque),
    descomprimir(Xs, RestoBase),
    append(Bloque, RestoBase, Base).
descomprimir([X | Xs], [X | RestoBase]) :-
    X \= c(_, _), 
    descomprimir(Xs, RestoBase).

generar_repetidos(_, 0, []).
generar_repetidos(X, K, [X | Xs]) :-
    K > 0,
    K1 is K - 1,
    generar_repetidos(X, K1, Xs).


/* 
Ejercicio 2: Programación Lógica y Resolución

Una estructura arbórea es o bien el símbolo o o bien una lista de estructuras arbóreas. Por ejemplo, las siguientes son estructuras arbóreas válidas:
- o
- []
- [o, o]
- [[o, o], o, [o, o]]
- [o, [o, o, [o, []], o], [o, []]]
*/

% estructuraArborea(-E)
estructuraArborea(o).
estructuraArborea(L) :- esListaDeEstructuras(L).

esListaDeEstructuras([]).
esListaDeEstructuras([X | Xs]) :-
    estructuraArborea(X),
    esListaDeEstructuras(Xs).

/*
Ejercicio 3: Arbol General es o bien el simbolo x, o una lista de arboles generales.
Vale x, [], [x, x]
*/
% arbolGeneral(-A)
arbolGeneral(A) :-
    % Usamos diagonalización sobre el "tamaño" N del árbol
    % (N = cantidad de hojas 'x' + cantidad de listas vacías al final)
    desde(1, N),
    arbolGen(N, A).

% arbolGen(+N, -A)
arbolGen(1, x).
arbolGen(1, []).
arbolGen(N, L) :- 
    N > 1, 
    listaGen(N, L).

% listaGen(+N, -L)
listaGen(1, []).
listaGen(N, [T | Ts]) :-
    N > 1,
    % Repartimos el tamaño N entre el primer árbol T y el resto de la lista Ts
    between(1, N, N1),
    N2 is N - N1,
    N2 > 0,
    arbolGen(N1, T),
    listaGen(N2, Ts).