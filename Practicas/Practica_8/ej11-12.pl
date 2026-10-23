% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 11
% Paradigmas de Programación (PLP)
% ==============================================================================

% Representación de árboles binarios:
% - nil: representa el árbol vacío.
% - bin(Izq, V, Der): representa un nodo con valor V, subárbol izquierdo Izq
%   y subárbol derecho Der.
% (Se asume siempre que el árbol de entrada está instanciado: +T).

% ------------------------------------------------------------------------------
% 1. vacio(+T)
% ------------------------------------------------------------------------------
% Verdadero sii el árbol T es el árbol vacío.
vacio(nil).

% ------------------------------------------------------------------------------
% 2. raiz(+T, -R)
% ------------------------------------------------------------------------------
% Obtiene el valor R del nodo raíz del árbol T.
% Falla naturalmente si T es nil (un árbol vacío no tiene raíz).
% Notar que R es una variable (inicia con mayúscula) para que unifique con
% cualquier valor almacenado (número, átomo o término compuesto).
raiz(bin(_, R, _), R).

% ------------------------------------------------------------------------------
% 3. altura(+T, -A)
% ------------------------------------------------------------------------------
% A es la altura del árbol binario T.
% La altura se define como la longitud del camino más largo desde la raíz
% hasta una hoja:
% - Caso base: Un árbol vacío tiene altura 0.
altura(nil, 0).

% - Caso recursivo: La altura de un nodo es 1 más el máximo entre las alturas
%   de sus subárboles izquierdo y derecho (usando la función aritmética max/2).
altura(bin(I, _, D), A) :-
    altura(I, AI),
    altura(D, AD),
    A is 1 + max(AI, AD).

% ------------------------------------------------------------------------------
% 4. cantidadDeNodos(+T, -C)
% ------------------------------------------------------------------------------
% C es la cantidad total de nodos del árbol binario T.
%
% Esquema inductivo puro sobre la estructura del árbol:
%
% - Caso base: El árbol vacío contiene 0 nodos.
cantidadDeNodos(nil, 0).

% - Caso recursivo: Cualquier nodo bin(I, _, D) aporta 1 nodo por sí mismo más
%   la suma de los nodos de sus dos subárboles.
%
% Nota sobre la disyunción:
% Los functores 'nil' y 'bin/3' son constructores algebraicos estrictamente
% disjuntos. Por ende, no se necesita ninguna regla especial para hojas
% (bin(nil, _, nil)) ni controles con not/1, ya que las hojas resuelven
% directamente CI = 0 y CD = 0 dando 1 + 0 + 0 = 1 de forma natural y unívoca.
cantidadDeNodos(bin(I, _, D), C) :-
    cantidadDeNodos(I, CI),
    cantidadDeNodos(D, CD),
    C is 1 + CI + CD.

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% ?- Arbol = bin(bin(nil, 2, nil), 5, bin(nil, 7, bin(nil, 9, nil))),
%    raiz(Arbol, R),
%    altura(Arbol, Alt),
%    cantidadDeNodos(Arbol, Cant).
%
% R = 5,
% Alt = 3,
% Cant = 4.

% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 12
% ==============================================================================

% ------------------------------------------------------------------------------
% i. inorder(+AB, -Lista)
% ------------------------------------------------------------------------------
% Tiene éxito si Lista es el recorrido en orden (inorder: Izq, Raíz, Der) de AB.
%
% - Caso base: Un árbol vacío tiene recorrido vacío [].
inorder(nil, []).

% - Caso recursivo: Obtenemos el inorder del subárbol izquierdo (IO) y del
%   derecho (DO). Al colocar [R|DO] directamente en el segundo argumento de
%   append/3, concatenamos IO ++ [R] ++ DO en una única pasada O(|IO|),
%   evitando llamadas intermedias redundantes a append.
inorder(bin(I, R, D), L) :-
    inorder(I, IO),
    inorder(D, DO),
    append(IO, [R|DO], L).

% ------------------------------------------------------------------------------
% ii. arbolConInorder(+Lista, -AB)
% ------------------------------------------------------------------------------
% Genera todos los árboles binarios AB cuyo recorrido inorder es Lista.
%
% ¿Por qué NO funciona 'inorder(I, L1), inorder(D, L2), append(L1, [R|L2], L)'?
% - Si intentáramos llamar a inorder/2 antes del append, las variables I y L1
%   estarían completamente libres (unbound). Prolog intentaría generar árboles
%   a ciegas (Generate & Test descontrolado) y caería en una rama infinita
%   generando subárboles izquierdos infinitos en el árbol de resolución SLD.
%
% - Solución canónica (reducir por la lista):
%   Como la Lista L viene instanciada (+), ejecutamos PRIMERO 'append(L1, [R|L2], L)'.
%   Esto particiona L en todas sus formas posibles de manera estrictamente finita.
%   En cada opción, L1 y L2 tienen longitud estrictamente menor que L. Luego,
%   las llamadas recursivas 'arbolConInorder(L1, I)' y 'arbolConInorder(L2, D)'
%   operan sobre sublistas más cortas, garantizando terminación absoluta.
%
% - Caso base: Una lista vacía genera únicamente el árbol vacío nil.
arbolConInorder([], nil).

% - Caso recursivo:
arbolConInorder(L, bin(I, R, D)) :-
    append(L1, [R|L2], L),
    arbolConInorder(L1, I),
    arbolConInorder(L2, D).

% --- PREGUNTA TEÓRICA DE LA GUÍA ---
% "¿Cuántos árboles con dicho recorrido existen para una lista dada?"
% Para una lista de longitud N, existen exactamente C_N árboles distintos,
% donde C_N es el N-ésimo Número de Catalán:
%
%         C_N = (1 / (N + 1)) * (2N sobre N)
%
% Esto se debe a que cualquier elemento de la lista puede ser elegido como raíz R,
% dejando k elementos a la izquierda y (N - 1 - k) a la derecha:
%
%         C_N = sum_{k=0}^{N-1} (C_k * C_{N-1-k})
%
% Valores para listas de longitud N:
% - N = 0: C_0 = 1 árbol  (nil)
% - N = 1: C_1 = 1 árbol  (bin(nil, X, nil))
% - N = 2: C_2 = 2 árboles
% - N = 3: C_3 = 5 árboles
% - N = 4: C_4 = 14 árboles
% ------------------------------------------------------------------------------

% ------------------------------------------------------------------------------
% iii. aBB(+T)
% ------------------------------------------------------------------------------
% Tiene éxito si T es un Árbol Binario de Búsqueda (ABB / BST).
%
% Propiedad matemática fundamental:
% Un árbol binario T es un ABB si y solo si su recorrido inorder(T, L)
% genera una secuencia L estrictamente creciente (sin repetidos y ordenada
% de menor a mayor).
%
% Ventajas de reutilizar inorder/2 del ítem (i):
% - Evita tener que lidiar con subárboles vacíos (nil) al buscar mínimos/máximos.
% - No recalcula subárboles repetidamente (complejidad óptima O(N)).
% - Valida en un único recorrido que toda la estructura del árbol y sus subárboles
%   respetan la invariante de orden.
aBB(T) :-
    inorder(T, L),
    estrictamenteCreciente(L).

% estrictamenteCreciente(+Lista)
% Verifica que los elementos de Lista estén en orden estrictamente ascendente (X < Y).
estrictamenteCreciente([]).
estrictamenteCreciente([_]).
estrictamenteCreciente([X, Y | Xs]) :-
    X < Y,
    estrictamenteCreciente([Y | Xs]).

% ------------------------------------------------------------------------------
% iv. aBBInsertar(+X, +T1, -T2)
% ------------------------------------------------------------------------------
% Inserta el elemento X en orden en el árbol binario de búsqueda T1, obteniendo T2.
% Si X ya pertenece al árbol, la estructura no cambia (un ABB no contiene duplicados).
%
% - Caso base: Insertar en árbol vacío crea una hoja con X.
aBBInsertar(X, nil, bin(nil, X, nil)).

% - Caso elemento repetido: Si la raíz coincide con X, el árbol no muta.
aBBInsertar(X, bin(I, X, D), bin(I, X, D)).

% - Caso X < R: Se inserta recursivamente en el subárbol izquierdo.
aBBInsertar(X, bin(I, R, D), bin(I2, R, D)) :-
    X < R,
    aBBInsertar(X, I, I2).

% - Caso X > R: Se inserta recursivamente en el subárbol derecho.
aBBInsertar(X, bin(I, R, D), bin(I, R, D2)) :-
    X > R,
    aBBInsertar(X, D, D2).

% --- ANÁLISIS DE REVERSIBILIDAD ---
% Pregunta de la guía: "Este predicado ¿es reversible en alguno de sus argumentos? Justificar."
%
% 1. Reversibilidad en X (aBBInsertar(-X, +T1, +T2)):
%    NO es reversible en X. Si X entra como variable libre, al llegar a las
%    comparaciones aritméticas 'X < R' o 'X > R', Prolog aborta con error:
%    "Arguments are not sufficiently instantiated" (error de instanciación).
%
% 2. Reversibilidad en T1 (aBBInsertar(+X, -T1, +T2)):
%    SÍ es parcialmente reversible para reconstruir el árbol original T1 a partir
%    de T2 y X. Como X y las raíces R de T2 están instanciadas, las comparaciones
%    'X < R' y 'X > R' son evaluables.
%    - Si X era una hoja de T2, Prolog puede deducir que T1 tenía nil en su lugar.
%    - Si X ya estaba en T1, devuelve T1 = T2.
%
% 3. Generación pura (aBBInsertar(-X, -T1, -T2)):
%    NO es reversible para generación libre por la dependencia estricta de orden
%    aritmético en X.
% ------------------------------------------------------------------------------