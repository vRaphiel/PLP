:- use_module(piezas).

% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :- 
    append(P, S, L), 
    length(P, Descartar), 
    append(R, _, S), 
    length(R, Tomar).

% Vamos a analizar si sublista es reversible en el primer y cuarto argumento, es decir, si pueden consultarse instanciados y como variables. Si ambos son reversibles entonces
% cualquier combinación de instancia/variable entre ambos debería devolver una unificación para las variables en caso de que las haya o debería devolver que el predicado es verdadero o falso en 
% caso que todos los argumentos estén instanciados. Las combinaciones posibles son: (+Descartar, +R), (+Descartar, -R), (-Descartar, +R), (-Descartar, -R). El segundo y tercer argumento se
% asume instanciado siempre. El segundo caso no lo analizamos porque los argumentos vienen con el patrón de instanciación original.

% (+Descartar, +R): En este caso, los cuatro argumentos están instanciados y el resultado será True o False dependiendo si R es el resultado de descartar los primeros Descartar elementos
% de L y tomar los primeros Tomar elementos del resto. En cualquier caso, el predicado devuelve una solución sin colgarse ya que se van a tomar todos los prefijos y sufijos posibles de L, luego el único prefijo que
% va a unificar es aquel con longitud Descartar. Por último R va a unificar solo si es un prefijo del resto y tiene longitud Tomar, que también está fija. Por lo tanto, va a 
% devolver true si unifica y false en caso contrario.

% (-Descartar, +R): En este caso tenemos una sola solución si la sublista R aparece una sola vez en L pero múltiples soluciones en caso contrario. 
% Se generarán todas las combinaciones de prefijos y sufijos de L con el primer predicado append. Para cada uno de los prefijos, en el primer predicado length se va a instanciar su 
% longitud correspondiente en Descartar. Esto funciona porque length es reversible en su segundo argumento. Una vez hecho esto, se revisa si el sufijo contiene a R como prefijo y si
% su longitud es Tomar, mediante los predicados append y length. Para aquellos casos en donde esto último es verdadero se va a devolver Descartar instanciado con el valor que tomó para cada caso.
% A modo ilustrativo podemos ver que el ejemplo sublista(Descartar, 3, [5,1,2,3,7,1,2,3,10], [1,2,3]) devuelve dos soluciones: 
%   Descartar = 1, que surge de eliminar el 5 y luego quedarnos con los 3 primeros elementos del resto.
%   Descartar = 5, que surge de eliminar todos los elementos hasta el 7 y luego quedarnos con los 3 primeros elementos del resto.

% (-Descartar, -R): En este caso va a haber muchas soluciones. Queremos a partir de una lista L y un valor Tomar, encontrar todas las sublistas de L que surgen de descartar
% los primeros Descartar elementos de L y tomar los primeros Tomar elementos del resto. Las soluciones que se devuelvan van a ser aquellas instanciaciones de Descartar y R que hagan verdadero al predicado. 
% Se generan todas las combinaciones de prefijos y sufijos de L. Descartar va a unificar con la longitud de cada prefijo y al final R también va a unificar siempre y cuando después de eliminar
% los primeros Descartar elementos, la lista restante sea de longitud mayor o igual a Tomar, donde R va a terminar siendo el prefijo de longitud Tomar de la lista restante. Todos aquellos prefijos
% de longitud Descartar que estén asociados a sufijos de longitud menor a Tomar no van a terminar unificando porque el predicado final length(R, Tomar) no va a unificar ya que todos los prefijos generados 
% por el predicado append(R, _, S) van a tener menor longitud que Tomar. 
% A modo ilustrativo podemos ver que el ejemplo sublista(Descartar, 3, [1, 2, 3, 4, 5], R) devuelve tres soluciones:
% Descartar = 0, R = [1, 2, 3], que surge de tomar el prefijo vacío, el sufijo [1, 2, 3, 4, 5] y tomar los primeros 3 elementos de esa lista.
% Descartar = 1, R = [2, 3, 4], que surge de tomar el prefijo [1], el sufijo [2, 3, 4, 5] y tomar los primeros 3 elementos de la lista.
% Descartar = 2, R = [3, 4, 5], que surge de tomar el prefijo [1, 2], el sufijo [3, 4, 5] y tomar los primeros 3 elementos de la lista.
% No hay más soluciones porque después se toma el prefijo [1, 2, 3], el sufijo [4, 5] y no tiene suficientes elementos (3) para tomar. Lo mismo pasa al tomar los prefijos [1, 2, 3, 4] y [1, 2, 3, 4, 5].

% Tablero(+K, -T)
tablero(K, T) :-
    length(T, 5),                   % Definimos que nuestro tablero tiene dimensión 5
    maplist(filaLongitudK(K), T).   % Mapeamos a cada elemento de nuestro tablero una fila de longitud K.

% filaLongitudK(+K, -Fila)
filaLongitudK(K, Fila) :- length(Fila, K).

% tamano(+M, -F, -C): True si M tiene F filas y C columnas.
tamano([], 0, 0).
tamano([H|M], F, C) :-
    length(M, DimM),
    F is DimM + 1,
    length(H, C).

% Coordenadas(+T, -IJ): True para todo IJ que sea coordenada de elementos del tablero. Indices(1...5, 1...K).
coordenadas(T, IJ) :-
    tamano(T, F, C),
    between(1, F, X),
    between(1, C, Y),
    IJ = (X,Y).

% K-piezas(+K, -PS): True si PS es lista de longitud K de identificadores de piezas que no repita soluciones y corte lo antes posible.
% Si 0 <= K <= 12, podemos agregar piezas. Generamos las combinaciones.
kPiezas(K, PS) :-
    between(0,12,K),
    nombrePiezas(L),
    unaLineaDePiezas(K, L, PS).

% Caso Base. Necesitamos 0 piezas. Devolvemos vacío.
unaLineaDePiezas(0, _, []).

% Caso recursivo:
% Agregamos la nueva pieza a la lista:
unaLineaDePiezas(K, [H|RestoL], [H|RestoPS]) :-
    K > 0,                                      % Verificamos si aún podemos agregar piezas.
    NuevoK is K - 1,                            % Como agregamos una pieza, calculamos el nuevo K con eso.
    unaLineaDePiezas(NuevoK, RestoL, RestoPS).  % Buscamos el resto de piezas.

% No agregamos la nueva pieza a la lista
unaLineaDePiezas(K, [_|RestoL], RestoPS) :-
    K > 0,                                      % Aún podemos agregar piezas
    length([_|RestoL], LN),
    LN >= K,
    unaLineaDePiezas(K, RestoL, RestoPS).       % Agregamos piezas con lo demas.

% SeccionTablero(+T,+ALTO,+ANCHO,+IJ,?ST). True si ST es una sección de ALTO x ANCHO de T a partir de IJ.

seccionTablero(T,ALTO,ANCHO,(I,J),ST) :- 
    DescartarFilas is I - 1,
    DescartarColumnas is J - 1,
    sublista(DescartarFilas, ALTO, T, Filas),               % Aca descarto I-1 filas, y me quede con ALTO filas.                                       
    maplist(sublista(DescartarColumnas, ANCHO), Filas, ST). % Aca descarto J-1 columnas y me quedo con ANCHO columnas. Para eso le saco misma longitud a cada fila.

% ubicarPieza(+Tablero, +Identificador): Genera todas las ubicaciones de pieza dada en el tablero.
ubicarPieza(Tablero, Identificador) :-
    pieza(Identificador, I),
    tamano(I, F, C),
    coordenadas(Tablero, IJ),
    seccionTablero(Tablero, F, C, IJ, I).

% ubicarPiezas(+Tablero, +Poda, +Identificadores). Debe listar todas las posibles opciones de ubicar las piezas mencionadas en identificadores
% identificadores: Una lista ordenada.
% poda(+Poda, +Tablero).
% Poda indica que estrategia usar para podar el espacio de búsqueda.
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

% todosGruposLibresModulo5(+T)
todosGruposLibresModulo5(T) :- 
    findall(Casilla , esLibre(T, Casilla), L), 
    agrupar(L, G), 
    maplist(grupoMod5, G).

% grupoMod5(+L)
grupoMod5(L) :- 
    length(L, N), 
    mod(N,5) =:= 0.

% esLibre(+T, ?C)
esLibre(T, (I,J)) :- nth1(I, T, Fila), nth1(J, Fila, Casilla), var(Casilla).

ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [H|ID]) :-
    ubicarPieza(Tablero, H),
    poda(Poda, Tablero),
    ubicarPiezas(Tablero, Poda, ID).

% llenarTablero(+Poda, +Columnas, -Tablero): Enumera todas las posibles formas de llenar el tablero con
% cantidad de columnas (y piezas indicadas). Es decir, columnas = piezas = K.
% Columnas = K por facilidad. Tablero = T por facilidad

llenarTablero(Poda, K, T) :-
    tablero(K, T),                  % Creamos un nuevo tablero.
    kPiezas(K, PS),                 % Obtenemos para el tablero todas las posibles combinaciones de piezas que vamos a usar.
    ubicarPiezas(T, Poda, PS).      % Ubicamos todas las piezas.      

cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

% Sin poda:
% ?- time(cantSoluciones(sinPoda, 3, N)).
% 20,386,642 inferences, 1.734 CPU in 1.729 seconds (100% CPU, 11754460 Lips)
% N = 28.
% ?- time(cantSoluciones(sinPoda, 4, N)).
% 921,376,736 inferences, 79.781 CPU in 79.875 seconds (100% CPU, 11548788 Lips)
% N = 200.

% Con podaMod5:
%?- time(cantSoluciones(podaMod5, 3, N)).
% 10,510,254 inferences, 0.875 CPU in 0.878 seconds (100% CPU, 12011719 Lips)
% N = 28.
%?- time(cantSoluciones(podaMod5, 4, N)).
% 251,753,707 inferences, 21.438 CPU in 21.434 seconds (100% CPU, 11743613 Lips)
% N = 200.
% ?- time(cantSoluciones(podaMod5, 5, N)).
% 4,075,387,176 inferences, 341.031 CPU in 341.126 seconds (100% CPU, 11950187 Lips)
% N = 856.
% ?- time(cantSoluciones(podaMod5, 6, N)). 
% 45,133,820,875 inferences, 3839.031 CPU in 3844.787 seconds (100% CPU, 11756565 Lips) 
% N = 2164.