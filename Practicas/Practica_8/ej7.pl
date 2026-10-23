% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 7
% Paradigmas de Programación (PLP)
% ==============================================================================

% ------------------------------------------------------------------------------
% i. intersección(+L1, +L2, -L3)
% ------------------------------------------------------------------------------
% L3 es la intersección sin repeticiones de las listas L1 y L2, respetando en
% L3 el orden en que aparecen los elementos en L1.
%
% Esquema inductivo sobre L1:
% Recorremos L1 de izquierda a derecha. Esto garantiza que el orden de los
% elementos en L3 sea estrictamente el orden de su primera aparición en L1.
%
% Casos disjuntos:
% 1. Caso base: La intersección con una lista vacía es vacía.
interseccion([], _, []).

% 2. X está en L2 y NO aparece repetido más adelante en L1 (es la última o única aparición):
%    Se agrega X a la cabeza del resultado L3.
interseccion([X|Xs], L2, [X|L3]) :-
    member(X, L2),
    not(member(X, Xs)),
    interseccion(Xs, L2, L3).

% 3. X está en L2 pero aparece repetido más adelante en Xs:
%    Salteamos esta ocurrencia para no duplicar elementos en L3.
interseccion([X|Xs], L2, L3) :-
    member(X, L2),
    member(X, Xs),
    interseccion(Xs, L2, L3).

% 4. X no pertenece a L2:
%    Se descarta y se continúa con la cola Xs.
interseccion([X|Xs], L2, L3) :-
    not(member(X, L2)),
    interseccion(Xs, L2, L3).

% ------------------------------------------------------------------------------
% partir(+N, +L, -L1, -L2)
% ------------------------------------------------------------------------------
% L1 tiene los N primeros elementos de L, y L2 tiene el resto.
% Si L tiene menos de N elementos, el predicado debe fallar.
%
% Esquema inductivo sobre N y L:
%
% 1. Caso base:
%    Tomar los 0 primeros elementos de cualquier lista L.
%    - L1 queda vacía: [].
%    - L2 conserva todos los elementos restantes: L.
partir(0, L, [], L).

% 2. Caso recursivo:
%    Para tomar N > 0 elementos de una lista no vacía [X|Xs]:
%    - Tomamos X como primer elemento de la partición: [X|L1].
%    - Restamos 1 a N (M is N - 1).
%    - Hacemos la llamada recursiva con M y el resto de la lista Xs.
%    (La condición N > 0 es obligatoria: si N llega a 0 y no coincide con el caso
%     base, evita que Prolog reste hacia los negativos y caiga en bucle infinito).
partir(N, [X|Xs], [X|L1], L2) :-
    N > 0,
    M is N - 1,
    partir(M, Xs, L1, L2).

% ------------------------------------------------------------------------------
% ANÁLISIS DE REVERSIBILIDAD DE partir/4
% (Pregunta de la guía: ¿Cuán reversible es? ¿Qué parámetros pueden estar indefinidos?)
% ------------------------------------------------------------------------------
% 1. (+N, +L, -L1, -L2) -> Determinista:
%    ?- partir(2, [a, b, c, d], L1, L2).
%    L1 = [a, b], L2 = [c, d].
%
% 2. (+N, -L, +L1, +L2) -> Reversible (reconstruye L):
%    ?- partir(2, L, [a, b], [c, d]).
%    L = [a, b, c, d].
%
% 3. (-N, +L, +L1, +L2) -> Reversible (deduce N contando elementos):
%    ?- partir(N, [a, b, c, d], [a, b], [c, d]).
%    N = 2.
%
% 4. (+N, +L, +L1, +L2) -> Verificación pura:
%    ?- partir(2, [a, b, c, d], [a, b], [c, d]).
%    true.
%
% 5. Falla correctamente si L tiene menos de N elementos:
%    ?- partir(5, [a, b, c], L1, L2).
%    false.

% ------------------------------------------------------------------------------
% ii. borrar(+ListaOriginal, +X, -ListaSinXs)
% ------------------------------------------------------------------------------
% Elimina todas las ocurrencias del elemento X de ListaOriginal.
%
% Esquema inductivo sobre ListaOriginal:
%
% 1. Caso base: Borrar X de una lista vacía da como resultado la lista vacía [].
borrar([], _, []).

% 2. Caso cabeza coincide con X:
%    Descartamos X y el resultado final es Ys, que es el resultado de borrar
%    todas las ocurrencias de X en la cola Xs.
borrar([X|Xs], X, Ys) :-
    borrar(Xs, X, Ys).

% 3. Caso cabeza distinta de X (Y \= X):
%    Conservamos Y al frente del resultado: [Y|Ys], donde Ys es el resultado
%    de borrar todas las ocurrencias de X en la cola Xs.
%    (La condición Y \= X garantiza que las dos reglas recursivas sean disjuntas).
borrar([Y|Xs], X, [Y|Ys]) :-
    Y \= X,
    borrar(Xs, X, Ys).


% ------------------------------------------------------------------------------
% iii. sacarDuplicados(+L1, -L2)
% ------------------------------------------------------------------------------
% Obtiene la lista L2 a partir de L1 eliminando todas las apariciones duplicadas,
% conservando el orden de la primera aparición de cada elemento.
%
% Esquema inductivo reutilizando borrar/3:
%
% 1. Caso base:
%    Sacar duplicados de una lista vacía es la lista vacía [].
sacarDuplicados([], []).

% 2. Caso recursivo:
%    Ante una lista [X|Xs]:
%    - Conservamos a X en el resultado: [X|Ys] (manteniendo el orden de primera aparición).
%    - Usamos borrar/3 para eliminar cualquier otra copia de X en la cola Xs,
%      obteniendo la lista limpia SinX.
%    - Continuamos recursivamente deduciendo duplicados sobre SinX.
sacarDuplicados([X|Xs], [X|Ys]) :-
    borrar(Xs, X, SinX),
    sacarDuplicados(SinX, Ys).

% ------------------------------------------------------------------------------
% iv. permutacion(+L1, ?L2)
% ------------------------------------------------------------------------------
% Tiene éxito cuando L2 es una permutación de L1.
% Funciona como generador (si L2 es libre) generando las N! permutaciones,
% y como verificador (si L2 está instanciada).
%
% Esquema inductivo:
% 1. Caso base: La única permutación de la lista vacía [] es [].
permutacion([], []).

% 2. Caso recursivo:
%    Para permutar [X|Xs]:
%    - Obtenemos recursivamente una permutación Zs de la cola Xs.
%    - Insertamos X en cualquier posición posible de Zs mediante insertar/3.
permutacion([X|Xs], Ys) :-
    permutacion(Xs, Zs),
    insertar(X, Zs, Ys).

% insertar(+X, +L, -Resultado)
% Inserta el elemento X en cualquier posición posible de la lista L.
% Se descompone L en dos partes (P y S) con append/3, y se intercala [X]
% entre ellas (P ++ [X] ++ S). El backtracking de append genera todas las
% posiciones de inserción posibles (desde la cabeza hasta el final).
insertar(X, L, LX) :-
    append(P, S, L),
    append(P, [X|S], LX).

% ------------------------------------------------------------------------------
% RESPUESTA A LA PREGUNTA TEÓRICA DE LA GUÍA:
% "¿Hay una manera más eficiente de definir este predicado para cuando L2 está instanciada?"
% ------------------------------------------------------------------------------
% SÍ, rotunda y categóricamente:
%
% 1. En la versión general (permutacion/2):
%    Tiene complejidad temporal factorial O(N * N!). Si L2 ya está instanciada (+L2)
%    y queremos verificar si es permutación, en el peor caso (o si no lo es)
%    Prolog genera una a una las N! permutaciones de L1 para compararlas con L2.
%
% 2. Cuando L2 está instanciada (+L1, +L2):
%    No necesitamos generar todas las permutaciones en memoria. Podemos:
%    a) Verificar que ambas listas tengan la misma longitud en O(N).
%    b) Eliminar elemento por elemento cada X de L1 en L2 (extrayendo una ocurrencia
%       con select/3) en O(N^2).
%    c) O bien ordenar ambas listas (por ejemplo con msort/2) en O(N log N)
%       y verificar si los resultados ordenados son idénticos.
%    Cualquiera de estos métodos pasa de complejidad factorial O(N!) a tiempo
%    polinomial (O(N^2) o O(N log N)).
%
% Implementación de verificación eficiente O(N^2) usando select/3:
permutacionEficiente([], []).
permutacionEficiente([X|Xs], L2) :-
    select(X, L2, RestoL2),
    permutacionEficiente(Xs, RestoL2).



% ------------------------------------------------------------------------------
% v. reparto(+L, +N, -LListas)
% ------------------------------------------------------------------------------
% LListas es una lista de N listas (N >= 1) de cualquier longitud (incluso vacías)
% tales que al concatenarlas en orden se obtiene la lista L.
%
% Esquema inductivo sobre N:
%
% 1. Caso base (N = 1):
%    Si tenemos que repartir en una única lista, toda la lista L debe quedar
%    en esa única posición: [L].
reparto(L, 1, [L]).

% 2. Caso recursivo (N > 1):
%    - Extraemos un prefijo X de L mediante append(X, Resto, L). X puede ser []
%      o tener elementos.
%    - Decrementamos N (N1 is N - 1).
%    - Repartimos recursivamente lo que sobra (Resto) en las N1 listas restantes.
%    - La lista de listas resultante es [X|LL].
reparto(L, N, [X|LL]) :-
    N > 1,
    append(X, Resto, L),
    N1 is N - 1,
    reparto(Resto, N1, LL).

% ------------------------------------------------------------------------------
% vi. repartoSinVacías(+L, -LListas)
% ------------------------------------------------------------------------------
% Similar a reparto/3, pero ninguna de las listas de LListas puede ser vacía,
% y la cantidad de particiones (longitud de LListas) puede variar libremente.
%
% Esquema inductivo puro sobre la lista L:
%
% 1. Caso base:
%    Repartir una lista vacía en listas no vacías solo admite 0 listas: [].
repartoSinVacías([], []).

% 2. Caso recursivo:
%    Dada una lista no vacía [H|T]:
%    - Tomamos un corte X no vacío (X \= []) de la cabeza mediante append/3.
%    - Continuamos recursivamente repartiendo el Resto en listas no vacías.
repartoSinVacías([H|T], [X|LL]) :-
    append(X, Resto, [H|T]),
    X \= [],
    repartoSinVacías(Resto, LL).

