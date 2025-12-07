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

% Correccion: 
% Usen unificación nativa en vez de C = F y Celdas = Fila.
% La 2da y 3er regla de armarFila/3 se pisan entre sí. Además hay código repetido y es un poco díficil de seguir.
% Intenten simplificar la solución por favor. No hace falta resolver aparte el caso de una única restricción. 
% Si el caso base pinta de blanco todas las celdas restantes de la lista, entonces basta con que el caso recursivo solo pruebe todas las posibles formas de
% pintar blancos y luego replicar los negros para la restricción que está en la cabeza.

pintadasValidas(r(Restricciones, Fila)) :- generarFila(Restricciones, Fila).

% Caso Base:
generarFila([], []).
generarFila([], [o|Resto]) :- generarFila([], Resto).

% Casos Recursivos:
generarFila(Restricciones, [o|RestoFila]) :- generarFila(Restricciones, RestoFila).
generarFila([Bloque|OtrasRestricciones], Fila) :-
    colocarBloque(Bloque, Fila, RestoFila),
    colocarSeparador(OtrasRestricciones, RestoFila, FilaFinal),
    generarFila(OtrasRestricciones, FilaFinal).

colocarBloque(0, Fila, Fila).
colocarBloque(N, [x|Resto], FilaSalida) :-
    N > 0,
    N1 is N - 1,
    colocarBloque(N1, Resto, FilaSalida).

colocarSeparador([], Fila, Fila).
colocarSeparador([_|_], [o|Resto], Resto). 

% Ejercicio 5
% resolverNaive(+NN)
% Dado un nono, usamos solo RS, que son las asignaciones con las restricciones

% Correccion
% Podrían haber aprovechado los metapredicados (maplist).

resolverNaive(nono(_, RS)) :- maplist(pintadasValidas, RS).

% Ejercicio 6

% Correccion
% La solución es demasiado compleja y muy poco declarativa. 
% Si aprovechan la unificación nativa no tendrían que hacer tantos chequeos de si las filas son o no son de cierta forma, simplemente si no unifica entonces falla. 
% Intenten usar el mecanismo de unificación y backtracking de prolog para construir una solución más idiomática. 
% El predicado combinarCelda/3 que viene dado debería ser central en la resolución de pintarObligatorias/1.
% Vengan a consultar y lo charlamos. Les voy a pedir que me expliquen lo que hicieron así que vengan preparadxs.

pintarObligatorias(r(Restricciones, FilaOriginal)) :-
    length(FilaOriginal, Len),
    findall(Posible, 
            (length(Posible, Len), pintadasValidas(r(Restricciones, Posible)), compatible(FilaOriginal, Posible)), 
            Posibilidades),
    Posibilidades \= [], 
    intersectarTodas(Posibilidades, FilaDeducida),
    FilaOriginal = FilaDeducida.

compatible(A, B) :- not(A \= B).

intersectarTodas([L], L).
intersectarTodas([L1, L2|Resto], Resultado) :-
    intersectarDos(L1, L2, ResParcial),
    intersectarTodas([ResParcial|Resto], Resultado).

intersectarDos([], [], []).
intersectarDos([C1|T1], [C2|T2], [Res|T3]) :-
    interseccionCelda(C1, C2, Res),
    intersectarDos(T1, T2, T3).

interseccionCelda(x, x, x).
interseccionCelda(o, o, o).
interseccionCelda(x, o, _).
interseccionCelda(o, x, _).
     
% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7
% deducir1Pasada(+NN)
% Recibimos el NONO y se lo pasamos a aplicarATodas para que deduzca en 1 pasada.
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
    continuarResolucion(NN).

continuarResolucion(NN) :-
    NN = nono(M, _),
    cantidadVariablesLibres(M, 0), 
    !. 

continuarResolucion(NN) :-
    restriccionConMenosLibres(NN, RestriccionElegida),
    pintadasValidas(RestriccionElegida),
    resolverDeduciendo(NN).

% Ejercicio 10
% solucionUnica(+NN)
solucionUnica(NN) :-
    findall(NN, resolverDeduciendo(NN), Soluciones),
    length(Soluciones, 1).

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
