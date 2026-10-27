% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 13
% Paradigmas de Programación (PLP)
% ==============================================================================
% Consigna:
% Definir el predicado coprimos(-X, -Y), que genere uno a uno todos los pares
% de números naturales coprimos (es decir, cuyo máximo común divisor es 1),
% sin repetir resultados. Usar la función gcd del motor aritmético.
% ==============================================================================

% ------------------------------------------------------------------------------
% 1. ANÁLISIS TEÓRICO: GENERACIÓN INFINITA EN DOS DIMENSIONES
% ------------------------------------------------------------------------------
% El objetivo es explorar el producto cartesiano infinito N+ x N+:
%
%   Y \ X |   1       2       3       4       5    ...
%  -------+------------------------------------------------
%     1   | (1,1)   (2,1)   (3,1)   (4,1)   (5,1)  ...
%     2   | (1,2)   (2,2)   (3,2)   (4,2)   (5,2)  ...
%     3   | (1,3)   (2,3)   (3,3)   (4,3)   (5,3)  ...
%     4   | (1,4)   (2,4)   (3,4)   (4,4)   (5,4)  ...
%    ...  |  ...
%
% ¿Por qué fallan los enfoques directos?
%
% [Error 1: Dos generadores infinitos en cascada]
%   generarPares(X, Y) :- desde(1, X), desde(1, Y).
%   - Prolog fija X = 1 y pasa al segundo 'desde', generando infinitamente
%     Y = 1, 2, 3, 4, ... bajando eternamente por la primera columna.
%   - Búsqueda en profundidad (DFS / SLD) incompleta: NUNCA backtraquea a X = 2.
%
% [Error 2: Acotar Y con X]
%   coprimos(X, Y) :- desde(2, X), between(2, X, Y), gcd(X, Y) =:= 1.
%   - 'between(2, X, Y)' obliga a que Y <= X, cubriendo solo el triángulo inferior.
%   - Pierde todos los pares con X < Y (ej: (2, 3), (3, 4), (2, 5), etc.).
%   - Al empezar en 2, omite completamente el número 1, que es coprimo con TODOS
%     los números naturales (gcd(1, N) = 1).
%
% ------------------------------------------------------------------------------
% 2. LA SOLUCIÓN: DIAGONALES DE CANTOR (SUMA CONSTANTE S = X + Y)
% ------------------------------------------------------------------------------
% Georg Cantor demostró que N x N es numerable recorriendo la grilla por diagonales.
% En lugar de dejar libre a X o a Y, fijamos la SUMA S = X + Y con un único
% generador infinito: 'desde(2, S)' (el menor par de N+ es (1,1) cuya suma es 2).
%
% Grilla por diagonales de suma constante:
%
%   Y \ X |   1       2       3       4
%  -------+------------------------------
%     1   |  S=2     S=3     S=4     S=5   ...
%     2   |  S=3     S=4     S=5   ...
%     3   |  S=4     S=5   ...
%     4   |  S=5   ...
%
% - Para S = 2: (1, 1)                      -> 1 par
% - Para S = 3: (1, 2), (2, 1)              -> 2 pares
% - Para S = 4: (1, 3), (2, 2), (3, 1)      -> 3 pares
% - Para S = 5: (1, 4), (2, 3), (3, 2), (4, 1) -> 4 pares
%
% Propiedades de este diseño:
% 1. Unicidad: Cada par (X, Y) tiene una suma única S y una coordenada X única;
%    no hay duplicados.
% 2. Completitud: Cualquier par (X, Y) se alcanzará cuando S = X + Y.
% 3. Finitud por nivel: Cada diagonal tiene exactamente S - 1 elementos, por lo
%    que Prolog siempre agota una diagonal antes de pasar a la siguiente sin colgarse.
% ------------------------------------------------------------------------------

% desde(+X, -Y)
% Generador infinito de números enteros a partir de X.
desde(X, X).
desde(X, Y) :-
    N is X + 1,
    desde(N, Y).

% ------------------------------------------------------------------------------
% generarPares(-X, -Y)
% ------------------------------------------------------------------------------
% Genera uno a uno todos los pares de números naturales positivos (X, Y) in N+ x N+.
generarPares(X, Y) :-
    desde(2, S),          % 1. Generador infinito: Suma S = 2, 3, 4, 5, ...
    S_ant is S - 1,       % 2. Límite superior: X puede llegar a lo sumo a S - 1 (para que Y >= 1)
    between(1, S_ant, X), % 3. Generador finito: X recorre [1, S - 1]
    Y is S - X.           % 4. Y queda determinado unívocamente por resta aritmética

% ------------------------------------------------------------------------------
% coprimos(-X, -Y)
% ------------------------------------------------------------------------------
% Patrón canónico GENERATE & TEST:
% - GENERATE: 'generarPares(X, Y)' produce pares (X, Y) en orden por diagonales.
% - TEST: 'gcd(X, Y) =:= 1' filtra aquellos pares cuyo máximo común divisor es 1.
coprimos(X, Y) :-
    generarPares(X, Y),
    gcd(X, Y) =:= 1.

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% ?- coprimos(X, Y).
% X = 1, Y = 1 ;  % S = 2
% X = 1, Y = 2 ;  % S = 3
% X = 2, Y = 1 ;  % S = 3
% X = 1, Y = 3 ;  % S = 4
% X = 3, Y = 1 ;  % S = 4 (el par (2,2) fue descartado porque gcd(2,2) = 2)
% X = 1, Y = 4 ;  % S = 5
% X = 2, Y = 3 ;  % S = 5
% X = 3, Y = 2 ;  % S = 5
% X = 4, Y = 1 ;  % S = 5
% X = 1, Y = 5 ;  % S = 6
% X = 5, Y = 1 .  % S = 6