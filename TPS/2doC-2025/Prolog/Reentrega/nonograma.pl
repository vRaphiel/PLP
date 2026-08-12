% Ejercicio 1
% matriz(+F, +C, -M)
matriz(F, C, M) :-  
    length(M, F), 
    maplist(filaLongitudC(C), M).
    
% filaLongitudC(+C, -Fila)
filaLongitudC(C, Fila) :- length(Fila, C).

% Ejercicio 2
% replicar(+X, +N, -L)
replicar(X, N, L) :- 
    length(L, N),
    listaDeTodasX(X, L).

listaDeTodasX(_, []).
listaDeTodasX(X, [X | XS]) :- listaDeTodasX(X, XS).

% Ejercicio 3
%  transponer(+M, -MT)
transponer([], []).
transponer([[]|_], []).

transponer(M, [X | XS]) :- 
    extraerPrimerColumna(M, X, MR), 
    MR \= [], 
    transponer(MR, XS).

% extraerPrimerColumna(+MatrizActual, -PrimerColumna, -MatrizSinPrimerCol):
extraerPrimerColumna([], [], []).
extraerPrimerColumna([[H|T]|XS], [H|RC], [T|RM]) :- extraerPrimerColumna(XS, RC, RM).

% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).

% Ejercicio 4
% pintadasValidas(+R)
pintadasValidas(r([], L)) :- listaDeTodasX(o, L).

pintadasValidas(r([R|Rs], [o|Resto])) :- pintadasValidas(r([R|Rs], Resto)).

pintadasValidas(r([R|Rs], L)) :-
    replicar(x, R, Bloque),
    append(Bloque, Resto, L),
    restoValido(Rs, Resto).

% restoVaido(+Restricciones, -Resto)
restoValido([], Resto) :- listaDeTodasX(o, Resto).
restoValido([R|Rs], [o|Resto]) :- pintadasValidas(r([R|Rs], Resto)).

% Ejercicio 5

% resolverNaive(+NN)
resolverNaive(nono(_, RS)) :- maplist(pintadasValidas, RS).

% Ejercicio 6

pintarObligatorias(r(Rs, C)) :-
    % Encuentro todas las soluciones validas a las restricciones
    findall(C, pintadasValidas(r(Rs, C)), [SolComunActual|Soluciones]),

    % Me quedo con una solucion con las celdas comunes a todas.
    % Cada pasada de foldl intesecta una solucion de Soluciones y la acumula en SolComunActual, que va guardando las celdas comunes a todas.
    foldl(celdasIgualesEnAmbos, Soluciones, SolComunActual, C).

% celdasIgualesEnAmbos(+L1,+L2,-L3)
celdasIgualesEnAmbos(L1, L2, L3) :- maplist(combinarCelda, L1, L2, L3).
     
% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
% deducir1Pasada(+NN)
deducir1Pasada(nono(_, RS)) :- maplist(pintarObligatorias, RS).

% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo más cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).

% Ejercicio 8
% restriccionConMenosLibres(+NN, -R)
restriccionConMenosLibres(nono(_, RS), R) :-
    member(R, RS),
    R = r(_, CR),
    cantidadVariablesLibres(CR, N),
    N > 0,
    % No existe ninguna otra restriccion con menos variables libres
    not((
        member(R2, RS),
        R2 = r(_, CR2),
        cantidadVariablesLibres(CR2, N2),
        N2 > 0,
        N2 < N
    )).

% Ejercicio 9
% resolverDeduciendo(+NN)
resolverDeduciendo(NN) :-
    deducirVariasPasadas(NN),
    revisarCorrectasOContinuar(NN).

% Cuando ya no tengo mas variables libres, resolvi.
revisarCorrectasOContinuar(NN) :-
    NN = nono(M, _),
    cantidadVariablesLibres(M, 0), 
    !.

revisarCorrectasOContinuar(NN) :-
    restriccionConMenosLibres(NN, R),
    !,
    pintadasValidas(R),
    resolverDeduciendo(NN).

% Ejercicio 10
% solucionUnica(+NN)
solucionUnica(NN) :-
    findall(NN, resolverDeduciendo(NN), [_]).

% Ejercicio 11 - Consultas para Analisis de nonogramas
% tam(+M,(-F,-C))
tam(N, (F, C)) :- 
    nn(N, nono(M, _)), 
    matriz(F, C, M).

% SolucionUnica(+N)
tieneSolUnica(N) :-
    nn(N, NN), 
    solucionUnica(NN),
    !.

% esDeducibleSinBackTracking(+N)
esDeducibleSinBackTracking(N) :-
    nn(N, NN),
    deducirVariasPasadas(NN),
    cantidadVariablesLibres(NN, 0),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0, NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1, NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2, NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3, NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4, NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5, NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6, NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7, NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8, NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9, NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').