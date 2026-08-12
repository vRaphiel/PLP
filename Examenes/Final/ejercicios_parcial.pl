% Auxiliar
desde(X, X).
desde(X, Y) :- var(Y), N is X+1, desde(N, Y).
desde(X, Y) :- nonvar(Y), X < Y.

% Segundo Recuperatorio - 1er Cuatrimestre de 2022
% montania(+L, -L1, -C, -L2)
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
obtenerElMaximo([X,Y|XS], M) :- mayor(X, Y, M1), obtenerElMaximo([M1|XS], M).

mayor(A, B, A) :- A >= B.
mayor(A, B, B) :- B > A.

esDecrecienteEstricto([X], Y, _) :- Y > X.
esDecrecienteEstricto([X | XS], Y, C) :-
    C > X,
    Y > X,
    esDecrecienteEstricto(XS, X, C).

esDecrecienteEstricto([]).
esDecrecienteEstricto([X | XS]) :-
    between(0, X, Y),
    X > Y,
    member(X, XS).
esDecrecienteEstricto([X | XS]) :-
    between(0, X, Y),
    Y > X,
    not(member(X, XS)).
    

% Segundo Recuperatorio - 2do Cuatrimestre de 2022
% sublistaMasLargaDePrimos(+L, ?P)
sublistaMasLargaDePrimos(L, P) :- 
    esUnaListaDePrimosContigua(L, P), 
    length(P, L1), 
    not((
        esUnaListaDePrimosContigua(L, P2), 
        length(P2, L2), 
        L2 > L1
    )).

esUnaListaDePrimosContigua(L, P) :- 
    esListaContinua(P, L),
    P \= [],
    todosPrimos(P).

esListaContinua([], []).
esListaContinua(P, L) :- append(_, R, L), append(P, _, R).

todosPrimos([]).
todosPrimos([X | XS]) :- esPrimo(X), todosPrimos(XS).

esPrimo(1).
esPrimo(N) :-
    N > 1,
    not((
        N2 is N - 1,
        between(2, N2, V),
        0 =:= N mod V
    )).

% simbolo(?)
% clausura(-L)

simbolo(a).
simbolo(b).

clausura(L) :- desde(0, Y), length(L, Y), esClausuraKleene(L).

esClausuraKleene([]).
esClausuraKleene([X | XS]) :- simbolo(X), esClausuraKleene(XS).

% Segundo Parcial - 1er Cuatrimestre de 2024
% estudiante(?E)
% notas(-XS) -> (Estudiante, Materia, Nota) A lo sumo 1 aprobado y muchos aplazos.

% tieneMateriaAprobada(+E, +M)

notas([(juan, plp, 3), (juan, plp, 9), (maria, tlen, 2), (maria, tlen, 10)]).
estudiante(juan).
estudiante(maria).

tieneMateriaAprobada(E, M) :-
    estudiante(E),
    notas(XS),
    between(4, 10, N),
    member((E, M, N), XS).

% eliminarAplazos(+NS, -L)
eliminarAplazos(NS, L) :-
    notas(XS),
    estaAprobado(XS, NS, L).

estaAprobado(_, [], []).
estaAprobado(XS, [(E, M, N) | XS2], [(E, M, N) | LXS]) :-
    member((E, M, N), XS),
    between(4, 10, N),
    estaAprobado(XS, XS2, LXS).
estaAprobado(XS, [(E, _, N) | XS2], L) :-
    member((E, _, N), XS),
    N =< 4,
    estaAprobado(XS, XS2, L).

% promedio(+A, -P)
promedio(A, P) :-
    estudiante(A),
    notas(XS),
    eliminarAplazos(XS, L),
    soloNotasDe(A, N, L),
    calcularPromedio(N, P).

soloNotasDe(_, [], []).
soloNotasDe(A, [N | XS], [(A, _, N) | Notas]) :- soloNotasDe(A, XS, Notas).
soloNotasDe(A, L, [(E, _, _) | Notas]) :- E \= A, soloNotasDe(A, L, Notas).

calcularPromedio([], 0).
calcularPromedio(N, P) :- length(N, L), calcularPromedio(N, L, 0, P).

calcularPromedio(_, 0, _, 0).
calcularPromedio([], L, Acc, P) :- P is Acc / L. 
calcularPromedio([X | XS], L, Acc, P) :- Acc2 is Acc + X, calcularPromedio(XS, L, Acc2, P).

% mejorEstudiante(-A)
mejorEstudiante(A) :-
    estudiante(A),
    promedio(A, P),
    not((
        estudiante(A2),
        promedio(A2, P2),
        P2 > P
    )).

% Segundo Recuperatorio - 1er Cuatrimestre de 2024
% generarCapicuas(-L)
generarCapicuas(L) :- desde(1, X), listaDeNDigitos(X, L), esCapicua(L).

listaDeNDigitos(0, []).
listaDeNDigitos(N, [X | XS]) :-
    N > 0,
    N1 is N - 1,
    listaDeNDigitos(N1, XS),
    between(0, N, X).

esCapicua(L) :- append(L1, L2, L), reverse(L1, L1R), L1R =:= L2.

% Segundo Parcial - 2do Cuatrimestre 2024
% subsecuenciaCreciente([4, 8, 1, 9], S)
subsecuenciaCreciente([], []).
subsecuenciaCreciente(L, S) :- esSubsecuencia(L, S), esCreciente(S).

esSubsecuencia([], []).
esSubsecuencia([X | XS], [X | XS2]) :- esSubsecuencia(XS, XS2).
esSubsecuencia([_ | XS], XS2) :- esSubsecuencia(XS, XS2).

esCreciente([]).
esCreciente(L) :- length(L, 1).
esCreciente([X, Y]) :- X < Y.
esCreciente([X, Y | XS]) :- X < Y, esCreciente([Y | XS]).

% subsecuenciaCreciente([4,8,1,9], S).
% esSubsecuenciaEnOrden([4,8,9], [4,8,1,9]).
% esCreciente([4,8,9])

% subsecuenciaCrecienteMasLarga(+L, -S)
subsecuenciaCrecienteMasLarga(L, S) :-
    subsecuenciaCreciente(L, S),
    length(S, L1),
    not((
        subsecuenciaCreciente(L, S2),
        length(S2, L2),
        L2 > L1
    )).

% subsecuenciaCrecienteMasLarga([4,8,1,9], S).

% fibonacci(-X) F0 = 0, F1 = 1, FN = FN-1 + FN-2

fibonacci(X) :- desde(0, Y), estadosFibonacci(Y, X).

estadosFibonacci(0, 0).
estadosFibonacci(1, 1).
estadosFibonacci(N, V) :- N \= 0, N \= 1, N1 is N - 1, N2 is N-2, estadosFibonacci(N1, V1), estadosFibonacci(N2, V2), V is V1 + V2.

% Segundo Recuperatorio - 2do Cuatrimestre 2024
% caminoDesde(+P, -C)
caminoDesde(P, [P]).
caminoDesde(P, C) :- desde(1, X), armamosUnCamino(P, C, X).

armamosUnCamino(_, [], 0).
armamosUnCamino(P, [P], 1).
armamosUnCamino(P, [P | XS], L) :- 
    L > 1,
    L1 is L - 1,
    movimientos(P, Mov),
    P \= Mov,
    armamosUnCamino(Mov, XS, L1).
    % P es el punto base, C es el camino, L es el tamaño del camino
    % Por cada paso, realizamos uno de los 5 movimientos y restamos uno en la longitud
    % Mientras no sea 0, seguimos agregando movimientos

movimientos((X, Y), (X2, Y)) :- X2 is X - 1.
movimientos((X, Y), (X2, Y)) :- X2 is X + 1.
movimientos((X, Y), (X, Y2)) :- Y2 is Y + 1.
movimientos((X, Y), (X, Y2)) :- Y2 is Y + 1.

% Problema de la mochila
objeto(1, 50, 10).
objeto(2, 75, 15).
objeto(3, 60, 5).
objeto(4, 10, 1).

objetos([ (1, 50, 10), (2, 75, 15), (3, 60, 5), (4, 10, 1) ]).

/* 
% mochila(+C, -L)
mochila(0, []).
mochila(C, L) :- objetos(XS), armarMochila(C, 0, L, XS).

% Armamos la mochila con 3 casos:
% Caso Base: Ya no quedan objetos, devolvemos la lista.
% Elegimos un objeto:
% Caso 1: si su carga + CargaActual es menor que la carga, lo agregamos.
% Caso 2: si su carga + CargaActual es mayor que la carga, no lo agregamos.
% Caso 3: no lo agregamos.

armarMochila(_, _, [], []).
armarMochila(Carga, CargaActual, [Id | RestoLista], [(Id, P, V) | RestoObjetos]) :-
    objeto(Id, P, V),
    CargaAuxiliar is CargaActual + P,
    CargaAuxiliar < Carga,
    armarMochila(Carga, CargaAuxiliar, RestoLista, RestoObjetos).

armarMochila(Carga, CargaActual, RestoLista, [(Id, P, V) | RestoObjetos]) :-
    objeto(Id, P, V),
    CargaAuxiliar is CargaActual + P,
    CargaAuxiliar > Carga,
    armarMochila(Carga, CargaActual, RestoLista, RestoObjetos).

armarMochila(Carga, CargaActual, RestoLista, [_ | RestoObjetos]) :-
    objeto(Id, P, V),
    armarMochila(Carga, CargaActual, RestoLista, RestoObjetos).
*/

% mochila(+C, -L)
mochila(C, L) :- objetos(XS), armarMochila(C, L, XS).

armarMochila(_, [], []).

% Caso 1: Agregamos el objeto a la mochila (si entra)
armarMochila(C, [Id | RestoMochila], [(Id, P, _) | RestoObjetos]) :-
    P =< C,
    NuevoC is C - P,
    armarMochila(NuevoC, RestoMochila, RestoObjetos).

% Caso 2: NO agregamos el objeto a la mochila y pasamos al siguiente
armarMochila(C, RestoMochila, [_ | RestoObjetos]) :- armarMochila(C, RestoMochila, RestoObjetos).

% MejorMochila. Idem a lo usual.

% Segundo Recuperatorio - 1er Cuatrimestre de 2025
% unico(+L, -U)
unico(L, U) :-
    append(Prefix, [U | Sufix], L),
    not(member(U, Prefix)),
    not(member(U, Sufix)).

% unico([a,b,c,a,a,b,d], U).

% sinRepetidos(+L)
sinRepetidos([]).
sinRepetidos(L) :- verSinRepetidos(L, L).

verSinRepetidos([], _).
verSinRepetidos([X | XS], L) :- unico(L, X), verSinRepetidos(XS, L).

% formula(+VS, -F)
formula(LS, F) :- desde(0, X), armarFormula(LS, F, X).

armarFormula(LS, F, 1) :- member(F, LS).
armarFormula(LS, neg(F), X) :-
    X > 1,
    X1 is X - 1,
    armarFormula(LS, F, X1).
armarFormula(LS, imp(FP, FQ), X) :-
    X > 2,
    X1 is X - 1,
    between(1, X1, XP),
    XQ is X1 - XP,
    armarFormula(LS, FP, XP),
    armarFormula(LS, FQ, XQ).

% Segundo Recuperatorio - 2do Cuatrimestre 2025
% secuenciaRepetida(+M, -S)
secuenciaRepetida([Fila | Filas], S) :-
    esSublista(S, Fila),
    todasTienen(Filas, S).

todasTienen([], _).
todasTienen([Fila | Filas], S) :-
    esSublista(S, Fila),
    todasTienen(Filas, S).

esSublista(S, L) :-
    append(_, Sufijo, L),
    append(S, _, Sufijo),
    S \= [].

/*
?- secuenciaRepetida([[1,2,3],[1,2,4],[3,1,2]], S).
S = [1];
S = [1,2];
S = [2];
false.
*/

% secuenciaMaxima(+M, -S).
secuenciaMaxima(M, S) :-
    secuenciaRepetida(M, S),
    length(S, L1),
    not((
        secuenciaRepetida(M, S2),
        length(S2, L2),
        L2 > L1
    )).

/*
?- secuenciaMaxima([[a,b,c,d],[b,c,d,e],[a,b,c,e]], S).
S = [b,c];
false.

?- secuenciaMaxima([[1,2,3],[1,3,5]], S).
S = [1];
S = [3];
false.
*/

% todasLasMatrices(-M).
todasLasMatrices(M) :- 
    desde(2, S),
    MaxFilas is S - 1,
    between(1, MaxFilas, Filas),
    Columnas is S - Filas,
    length(M, Filas),
    todasTienenLong(M, Columnas).

todasTienenLong([], _).
todasTienenLong([Fila | Filas], C) :-
    length(Fila, C),
    todasTienenLong(Filas, C).