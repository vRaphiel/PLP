% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 14 (Ítem i)
% Paradigmas de Programación (PLP)
% ==============================================================================
% Consigna:
% Un cuadrado semi-mágico es una matriz cuadrada de naturales (incluido el cero)
% donde todas las filas de la matriz suman lo mismo.
%
% i. Definir el predicado cuadradoSemiMágico(+N, -XS). El predicado debe ir
%    devolviendo matrices de dimensión N x N que sean cuadrados semi-mágicos.
%    Dichas matrices deben devolverse de manera ordenada: primero aquellas
%    cuyas filas suman 0, luego 1, luego 2, etc.
%    (No es necesario utilizar la técnica Generate & Test).
%
% Ejemplo para N = 2:
% ?- cuadradoSemiMágico(2, X).
% X = [[0, 0], [0, 0]] ;
% X = [[0, 1], [0, 1]] ;
% X = [[0, 1], [1, 0]] ;
% X = [[1, 0], [0, 1]] ;
% X = [[1, 0], [1, 0]] ;
% X = [[0, 2], [0, 2]] ; ...
% ==============================================================================

% ------------------------------------------------------------------------------
% 1. ANÁLISIS DE LA SOLUCIÓN PROPUESTA Y POR QUÉ FALLABA
% ------------------------------------------------------------------------------
% Tu intuición inicial fue excelente:
% 1) Fijar la dimensión de la matriz con length/2.
% 2) Usar un generador infinito 'desde(0, K)' para iterar ordenadamente en la suma: 0, 1, 2, ...
% 3) Pedir que cada una de las N filas sume K.
%
% Sin embargo, surgieron dos problemas críticos de flujo en el código:
%
% [Falla A: Bucle infinito por caso base incompleto en cadaFilaSuma]
%   Al escribir:
%       cadaFilaSuma(0, []).
%       cadaFilaSuma(0, [X|XS]) :- sonTodosCeros(X), cadaFilaSuma(0, XS).
%       cadaFilaSuma(Y, [X|XS]) :- Y > 0, laFilaSuma(Y, X), cadaFilaSuma(Y, XS).
%   - Para suma 0: la matriz [[0,0],[0,0]] unifica bien y llega a cadaFilaSuma(0, []),
%     devolviendo la primera solución.
%   - Para suma Y > 0 (ej: Y = 1): procesa las filas, pero al llegar a la lista
%     vacía llama a 'cadaFilaSuma(1, [])'. Como la regla exige el literal 0 en
%     el primer argumento, 'cadaFilaSuma(1, [])' ¡FALLA!
%   - Prolog descarta la matriz válida y vuelve a 'desde(0, Y)', probando Y = 2,
%     Y = 3, Y = 4... eternamente. Al fallar siempre al final de cada matriz,
%     Prolog entra en un bucle infinito en el árbol SLD sin devolver soluciones > 0.
%   -> Corrección: El caso base debe aceptar cualquier suma: cadaFilaSuma(_, []).
%
% [Falla B: Multiplicación de soluciones (los 4 trues en laFilaSuma)]
%   Al escribir:
%       laFilaSuma(V, [X | Xs]) :- V > 0, between(0, V, X), X =< V, VRes is V - X, laFilaSuma(VRes, Xs).
%       laFilaSuma(V, [0 | Xs]) :- V > 0, between(0, V, _), laFilaSuma(V, Xs).
%   - 'between(0, V, X)' en la primera regla ya incluye X = 0 (pues arranca en 0).
%   - La segunda regla volvía a matchear la cabeza 0, y al poner 'between(0, V, _)'
%     con la variable anónima '_', Prolog generaba V + 1 choice points espurios
%     para cada 0, multiplicando las soluciones idénticas (1 de la primera regla +
%     3 de la segunda para V = 2, dando 4 trues).
%   -> Corrección: Eliminar la segunda regla; la primera cubre X = 0 de forma natural.
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% 2. SOLUCIÓN CANÓNICA Y DECLARATIVA (Ítem i)
% ------------------------------------------------------------------------------
% En lugar de pre-construir la matriz con 'todasDeLongitud' y chequear ceros con
% 'sonTodosCeros', consumimos en simultáneo la dimensión N y la suma K en cada fila.
% Esto unifica el caso K = 0 y K > 0 sin necesidad de bifurcaciones artificiales.

% desde(+X, -Y)
% Generador infinito de sumas: 0, 1, 2, 3, ...
desde(X, X).
desde(X, Y) :-
    N is X + 1,
    desde(N, Y).

% cuadradoSemiMagico(+N, -XS)
% Instancia XS en cuadrados semi-mágicos de orden N x N, ordenados por suma de filas.
cuadradoSemiMagico(N, XS) :-
    desde(0, K),            % Genera ordenadamente la suma K = 0, 1, 2, ...
    length(XS, N),          % La matriz tiene exactamente N filas
    filasQueSumanK(N, K, XS). % Cada una de las filas debe tener longitud N y sumar K

% filasQueSumanK(+N, +K, ?Filas)
% - Caso base: La matriz sin filas siempre cumple.
filasQueSumanK(_, _, []).

% - Caso recursivo: La primera fila tiene longitud N y suma K, y el resto también.
filasQueSumanK(N, K, [Fila | Filas]) :-
    filaQueSumaK(N, K, Fila),
    filasQueSumanK(N, K, Filas).

% filaQueSumaK(+N, +K, ?Fila)
% Genera todas las listas de longitud N cuyos elementos son naturales y suman K.
%
% - Caso base: Una fila de longitud 0 solo puede sumar 0 y es la lista vacía [].
filaQueSumaK(0, 0, []).

% - Caso recursivo: Elegimos para la cabeza un natural X en [0, K].
%   El resto de la fila debe tener longitud N - 1 y sumar K - X.
filaQueSumaK(N, K, [X | Xs]) :-
    N > 0,
    between(0, K, X),
    K_restante is K - X,
    N_restante is N - 1,
    filaQueSumaK(N_restante, K_restante, Xs).

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% ?- cuadradoSemiMagico(2, XS).
% XS = [[0, 0], [0, 0]] ; % K = 0
% XS = [[0, 1], [0, 1]] ; % K = 1
% XS = [[0, 1], [1, 0]] ; % K = 1
% XS = [[1, 0], [0, 1]] ; % K = 1
% XS = [[1, 0], [1, 0]] ; % K = 1
% XS = [[0, 2], [0, 2]] ; % K = 2
% XS = [[0, 2], [1, 1]] ; % K = 2
% XS = [[0, 2], [2, 0]] ; % K = 2
% XS = [[1, 1], [0, 2]] ; % K = 2
% ...
% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 14 (Ítem ii)
% ==============================================================================
% Consigna:
% Definir utilizando Generate & Test el predicado cuadradoMagico(+N, -XS),
% que instancia XS con cuadrados cuyas filas y columnas suman todas un mismo valor.
% ==============================================================================

% ------------------------------------------------------------------------------
% ARQUITECTURA DEL PATRÓN GENERATE & TEST EN ESTE PROBLEMA
% ------------------------------------------------------------------------------
% 1. GENERATE (Generador):
%    Reutilizamos 'cuadradoSemiMagico(N, C)'.
%    - Como C entra como variable libre (-C), 'desde(0, K)' propone sumas
%      0, 1, 2, 3, ... y genera matrices donde TODAS las filas ya suman K.
%
% 2. TEST (Verificador / Filtro):
%    Una vez que el generador propone una matriz C, ¡C YA ESTÁ INSTANCIADA (+C)!
%    - Obtenemos el valor de la suma fija K leyendo la primera fila:
%          C = [Fila1 | _], sum_list(Fila1, K).
%    - Por ende, en la etapa de TEST NO usamos ningún generador 'desde/2'.
%      Simplemente verificamos de forma estrictamente determinística y finita
%      que todas las columnas sumen exactamente K.
%    - Si alguna columna no suma K, el TEST falla -> Prolog hace backtracking
%      al generador y pide la siguiente matriz.
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% ENFOQUE 1: CHEQUEO DIRECTO DE COLUMNAS (Solución canónica en PLP)
% ------------------------------------------------------------------------------
% cuadradoMagico(+N, -C)
cuadradoMagico(N, C) :-
    cuadradoSemiMagico(N, C),        % GENERATE (filas ya suman K)
    C = [Fila1 | _],
    sum_list(Fila1, K),              % K es la suma fija que produjo el generador
    columnasSumanK(C, N, K).         % TEST: todas las columnas deben sumar K

% columnasSumanK(+Matriz, +J, +K)
% Caso base: Si J llega a 0, ya verificamos todas las columnas de 1 a N.
columnasSumanK(_, 0, _).

% Caso recursivo: Extraemos la columna J, verificamos que sume K, y continuamos con J - 1.
columnasSumanK(M, J, K) :-
    J > 0,
    columna(J, M, Col),
    sum_list(Col, K),
    J_ant is J - 1,
    columnasSumanK(M, J_ant, K).

% columna(+J, +Matriz, -Columna)
% Extrae en Columna los elementos de la posición J de cada fila de la Matriz.
% - Caso base: Una matriz vacía no tiene columnas.
columna(_, [], []).

% - Caso recursivo: Tomamos el J-ésimo elemento de la primera fila con nth1/3
%   y llamamos recursivamente con el resto de las filas.
columna(J, [Fila | Filas], [X | RestoCol]) :-
    nth1(J, Fila, X),
    columna(J, Filas, RestoCol).

% ------------------------------------------------------------------------------
% ENFOQUE 2: VÍA MATRIZ TRASPUESTA (Tu propuesta matemática)
% ------------------------------------------------------------------------------
% Fundamento:
% Las columnas de una matriz C son exactamente las filas de su traspuesta CT.
% Por lo tanto:
%   C es un cuadrado mágico  <=>  C es semi-mágico Y CT es semi-mágico (con la misma suma K).
%
% cuadradoMagicoTranspuesta(+N, -C)
cuadradoMagicoTranspuesta(N, C) :-
    cuadradoSemiMagico(N, C),        % GENERATE: C tiene filas que suman K
    C = [Fila1 | _],
    sum_list(Fila1, K),              % Suma fijada por el generador
    trasponer(C, CT),                % Obtenemos la matriz traspuesta CT
    filasQueSumanK(N, K, CT).        % TEST: reutilizamos directamente filasQueSumanK!

% trasponer(+Matriz, -Traspuesta)
% La traspuesta de una matriz de N x N es la lista formada por sus N columnas (de 1 a N).
trasponer(M, T) :-
    length(M, N),
    todasLasColumnas(1, N, M, T).

todasLasColumnas(I, N, _, []) :-
    I > N.
todasLasColumnas(I, N, M, [Col | Cols]) :-
    I =< N,
    columna(I, M, Col),
    I2 is I + 1,
    todasLasColumnas(I2, N, M, Cols).

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA (Ítem ii)
% ------------------------------------------------------------------------------
% ?- cuadradoMagico(2, C).
% C = [[0, 0], [0, 0]] ; % K = 0
% C = [[0, 1], [1, 0]] ; % K = 1
% C = [[1, 0], [0, 1]] ; % K = 1
% C = [[0, 2], [2, 0]] ; % K = 2
% C = [[1, 1], [1, 1]] ; % K = 2
% C = [[2, 0], [0, 2]] ; % K = 2
% ...
% (Notar que matrices semi-mágicas como [[0, 1], [0, 1]] cuyas columnas sumaban
% 0 y 2 fueron correctamente descartadas por el TEST de columnas).
