% Ejercicio 1
% matriz(+F, +C, -M)
matriz(F, C, M) :-  length(M, F), maplist(filaLongitudC(C), M).
    
% filaLongitudC(+C, -Fila)
filaLongitudC(C, Fila) :- length(Fila, C).

% Ejercicio 2
% replicar(+X, +N, -L)
replicar(X, N, L) :- length(L, N), sonTodosDeL(X, L).

sonTodosDeL(_, []).
sonTodosDeL(X, [X | XS]) :- sonTodosDeL(X, XS).

% Ejercicio 3
% ?- transponer([[1, 2, 3], [4, 5, 6]], MT).
% MT = [[1, 4], [2, 5], [3, 6]].
% ?- transponer([[1, 2, A], [4, B, 6]], MT).
% MT = [[1, 4], [2, B], [A, 6]].

% Si lo vemos como tomar las cabezas de cada lista...
%  transponer(+M, -MT)
transponer([], []).
transponer([[]|_], []).
transponer(M, [X | XS]) :- extraerHeadersYReducir(M, X, MR), MR \= [], transponer(MR, XS).

extraerHeadersYReducir([], [], []).
extraerHeadersYReducir([[H|T]|XS], [H|RC], [T|RM]) :- extraerHeadersYReducir(XS, RC, RM).

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
% R sera de la forma r(Res, Celdas) donde 
% - Res es una lista de enteros que representan las restricciones.
% - Celdas es una lista de variables (parcialmente no instanciadas) que representan las celda.
% pintadasValidas(+R)
% Caso Base
pintadasValidas(r([], C)) :- length(C, N), replicar(o, N, F), C = F.

% Caso Recursivo
pintadasValidas(r(Restricciones, Celdas)) :-
	Restricciones \= [],
	length(Celdas, CantidadCeldas),
	length(Restricciones, CantBloques),
	sum_list(Restricciones, SumRestricciones),
	CeldasEmpty is CantidadCeldas - SumRestricciones,
	CeldasEmpty >= 0,
	SeparadoresMinimos is CantBloques - 1,
	CeldasLibres is CeldasEmpty - SeparadoresMinimos,
	CeldasLibres >= 0,
	AgujerosEnLaLista is CantBloques + 1,
	distribuirEspaciosLimpios(AgujerosEnLaLista, CeldasLibres, CeldasExtras),
	armarFila(Restricciones, CeldasExtras, Fila),
	Celdas = Fila.

% distribuirEspaciosLimpios(+N, +S, -L)
distribuirEspaciosLimpios(1, S, [S]) :- S >= 0.
distribuirEspaciosLimpios(N, S, [X|XS]) :-
	N > 1,
	S >= 0,
	N1 is N - 1,
	between(0, S, X),
	S1 is S - X,
	distribuirEspaciosLimpios(N1, S1, XS).

% armarFila(+Bloques, +BlancasExtras, -Fila)
armarFila([], [B], L) :- replicar(o, B, L).
armarFila([R], [B, BS], L) :-
	replicar(o, B, LimpiasInicio),
	replicar(x, R, NuevasOcupadas),
	replicar(o, BS, LimpiasFin),
	append(LimpiasInicio, NuevasOcupadas, TS),
	append(TS, LimpiasFin, L).

armarFila([R|RS], [B, BS|BSR], L) :-
	replicar(o, B, LimpiasInicio),
	replicar(x, R, NuevasOcupadas),
	Separadores is BS + 1,
	replicar(o, Separadores, BlancasEntrePosiciones),
	armarFila(RS, [0|BSR], RestoLista),
	append(LimpiasInicio, NuevasOcupadas, T1),
	append(T1, BlancasEntrePosiciones, T2),
	append(T2, RestoLista, L).

% Ejercicio 5
% resolverNaive(+NN)
% Dado un nono, usamos solo RS, que son las asignaciones con las restricciones
resolverNaive(nono(_, RS)) :- resolverNaiveNono(RS).

% Caso Base: Lista Vacia
resolverNaiveNono([]).

% Caso Recursivo.
resolverNaiveNono([R|RT]) :- pintadasValidas(R), resolverNaiveNono(RT).

% Ejercicio 6
% Caso Base
pintarObligatorias(r([], Celdas)) :-
    length(Celdas, N),
    replicar(o, N, FilasDeVacios),
    esConsistenteCon(Celdas, FilasDeVacios),
    aplicarObligatorias(Celdas, FilasDeVacios).

pintarObligatorias(r(Res, Celdas)) :-
	Res \= [],
	length(Celdas, CeldasL),
	length(Res, ResL),
	sum_list(Res, Pintadas),
	Blancas is CeldasL - Pintadas,
	Blancas >= 0,
	Separadores is ResL - 1,
	Libres is Blancas - Separadores,
	Libres >= 0,
	Huecos is ResL + 1,
	todasPartesNoNeg(Huecos, Libres, ExtrasTodas),
	length(Celdas, NCols),
	varsIndeterminadas(NCols, CombinadaInicial),
	combinarDesdeExtras(Res, ExtrasTodas, Celdas, CombinadaInicial, 0, FilaObligatoria, Encontradas),
	Encontradas = 1, % Tuve que agregar esto para que me ande bien deducir1Pasada
	aplicarObligatorias(Celdas, FilaObligatoria).

esConsistenteCon([], []).
esConsistenteCon([C|CS], [F|FS]) :- combinarCelda(C, F, _), esConsistenteCon(CS, FS).

combinarTodas([L], L).
combinarTodas([A, B|RS], Resultado) :- combinarListas(A, B, FCombParcial), combinarTodas([FCombParcial | RS], Resultado).

% Tuve que recurrir a hacer esto porque por algun motivo me generaba muchos repetidos y los test no pasaban.
% Los repetidos al final me terminaban generando problemas de memoria. O Stack Overflow, o generaba infinitas no instancias y no finalizaba nunca.
combinarListas([], [], []).
combinarListas([x|AS], [x|BS], [x|CS]) :- combinarListas(AS, BS, CS).
combinarListas([o|AS], [o|BS], [o|CS]) :- combinarListas(AS, BS, CS).
combinarListas([x|AS], [o|BS], [_|CS]) :- combinarListas(AS, BS, CS).
combinarListas([o|AS], [x|BS], [_|CS]) :- combinarListas(AS, BS, CS).
combinarListas([A|AS], [_|BS], [_|CS]) :- var(A), combinarListas(AS, BS, CS).
combinarListas([A|AS], [B|BS], [_|CS]) :- nonvar(A), var(B), combinarListas(AS, BS, CS).

aplicarObligatorias([], []).
aplicarObligatorias([C|CS], [O|OS]) :- nonvar(O), C = O, aplicarObligatorias(CS, OS).
aplicarObligatorias([_|CS], [O|OS]) :- var(O), aplicarObligatorias(CS, OS).

todasPartesNoNeg(1, S, [[S]]) :- S >= 0.
todasPartesNoNeg(H, S, R) :- H > 1, S >= 0, todasPartesNegativas(H, S, 0, R).

todasPartesNegativas(_, S, X, []) :- X > S.
todasPartesNegativas(H, S, X, R) :-
	X =< S,
	H1 is H - 1,
	S1 is S - X,
	todasPartesNoNeg(H1, S1, Sufijo),
	prefijoEnTodas(X, Sufijo, Prefijo),
	X1 is X + 1,
	todasPartesNegativas(H, S, X1, R2),
	append(Prefijo, R2, R).

prefijoEnTodas(_, [], []).
prefijoEnTodas(X, [L|LS], [[X|L]|RS]) :- prefijoEnTodas(X, LS, RS).

% Estos predicados se agregaron para pasar los tests de deducir1pasada y acelerar los deducirVariasPasadas.
% No se me ocurrio como hacer otros predicados mas simples.

filasValidasDesdeExtras(_, [], _, []).
filasValidasDesdeExtras(Res, [E|ES], C, [F|R]) :-
	armarFila(Res, E, F),
	esConsistenteCon(C, F),
	filasValidasDesdeExtras(Res, ES, C, R).
filasValidasDesdeExtras(Res, [E|ES], C, R) :-
	armarFila(Res, E, F),
	inconsistente(C, F),
	filasValidasDesdeExtras(Res, ES, C, R).

combinarDesdeExtras(_, [], _, Combinada, Encontradas, Combinada, Encontradas).
combinarDesdeExtras(Res, [E|_], C, CombinadaIn, 1, CombinadaOut, 1) :-
	armarFila(Res, E, F),
	esConsistenteCon(C, F),
	combinarListas(CombinadaIn, F, CombinadaTmp),
	todosVars(CombinadaTmp),
	CombinadaOut = CombinadaTmp.

combinarDesdeExtras(Res, [E|ES], C, CombinadaIn, 1, CombinadaOut, 1) :-
	armarFila(Res, E, F),
	esConsistenteCon(C, F),
	combinarListas(CombinadaIn, F, CombinadaTmp),
	hayDeterminada(CombinadaTmp),
	combinarDesdeExtras(Res, ES, C, CombinadaTmp, 1, CombinadaOut, 1).

combinarDesdeExtras(Res, [E|ES], C, _, 0, CombinadaOut, 1) :-
	armarFila(Res, E, F),
	esConsistenteCon(C, F),
	combinarDesdeExtras(Res, ES, C, F, 1, CombinadaOut, 1).

combinarDesdeExtras(Res, [E|ES], C, CombinadaIn, EncontradasIn, CombinadaOut, EncontradasOut) :-
	armarFila(Res, E, F),
	inconsistente(C, F),
	combinarDesdeExtras(Res, ES, C, CombinadaIn, EncontradasIn, CombinadaOut, EncontradasOut).

varsIndeterminadas(0, []).
varsIndeterminadas(N, [_|R]) :- N > 0, N1 is N - 1, varsIndeterminadas(N1, R).

todosVars([]).
todosVars([H|T]) :- var(H), todosVars(T).

hayDeterminada([H|_]) :- nonvar(H).
hayDeterminada([H|T]) :- var(H), hayDeterminada(T).

% Repeti el combinar celda pero para headers. Lo mismo con los problemas de arriba de instanciacion y memory allocation.
inconsistente([C|_], [F|_]) :- nonvar(C), nonvar(F), C \== F.
inconsistente([_|CS], [_|FS]) :- inconsistente(CS, FS).
     
% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
% deducir1Pasada(+NN)
% Recibimos el NONO y se lo pasamos a aplicarATodas para que deduzca en 1 pasada.
deducir1Pasada(nono(_, RS)) :- aplicarATodas(RS).

aplicarATodas([]).
aplicarATodas([r(_, C) | RT]) :-
	cantidadVariablesLibres(C, 0),
	aplicarATodas(RT).
    
aplicarATodas([R | RT]) :-
	R = r(_, C),
	cantidadVariablesLibres(C, N),
	N > 0,
	pintarObligatorias(R),
	aplicarATodas(RT).

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
    NN = nono(M, _),
    cantidadVariablesLibres(M, 0).

resolverDeduciendo(NN) :-
    deducirVariasPasadas(NN),
    NN = nono(M, _),
    cantidadVariablesLibres(M, N),
    N > 0,
    restriccionConMenosLibres(NN, R),
    pintadasValidas(R),
    resolverDeduciendo(NN),
    !.

% Ejercicio 10
% solucionUnica(+NN)
solucionUnica(NN) :-
    reconstruir(NN, RF, RC),
    findall(M,(armarNono(RF, RC, nono(M, RS)), generarSoluciones(nono(M, RS))),ListaDeSoluciones),
    length(ListaDeSoluciones, 1).

generarSoluciones(NN) :- deducirVariasPasadas(NN), NN = nono(M, _), cantidadVariablesLibres(M, 0).

generarSoluciones(NN) :-
    deducirVariasPasadas(NN),
    NN = nono(M, _),
    cantidadVariablesLibres(M, N),
    N > 0,
    restriccionConMenosLibres(NN, R),
    pintadasValidas(R),
    generarSoluciones(NN).

reconstruir(nono(M, RS), RF, RC) :-
    length(M, F),
    length(Filas, F),
    append(Filas, RCS, RS),
    remover(Filas, RF),
    remover(RCS, RC).

remover([], []).
remover([r(Res, _)|RestoRs], [Res | RestoRes]) :-
    remover(RestoRs, RestoRes).

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
