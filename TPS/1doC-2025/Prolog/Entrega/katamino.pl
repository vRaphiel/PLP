:- use_module(piezas).

% sublista(+Descartar, +Tomar, +L, -R)
sublista(Descartar, Tomar, L, R) :- 
    append(P, S, L), 
    length(P, Descartar), 
    append(R, _, S), 
    length(R, Tomar).

% Vamos a analizar si sublista es reversible en el primer y cuarto argumento, es decir, si pueden consultarse instanciados y como variables. Si ambos son reversibles entonces
% cualquier combinación de instancia/variable entre ambos deberia ser válida: (+Descartar, +R), (+Descartar, -R), (-Descartar, +R), (-Descartar, -R). El segundo y tercer argumento se
% asume instanciado siempre.
% (+Descartar, +R): En este caso, los cuatro argumentos están instanciados y el resultado será True o False dependiendo si R es el resultado de descartar los primeros Descartar elementos
% de L y tomar los primeros Tomar elementos del resto. En cualquier caso, el predicado es válido ya que se van a tomar todos los prefijos y sufijos posibles de L, luego el único prefijo que
% va a unificar es aquel con longitud Descartar. Por último R va a unificar solo si es un prefijo del resto y tiene longitud Tomar, que también está fija. Entonces concluimos que es válido.

% (+Descartar, -R): Este caso es trivial porque es la manera en la que fue pensado el predicado originalmente. Es válido y la solución es única porque luego de descartar Descartar elementos de L
% y tomar Tomar elementos del resto, obtenemos una lista determinada.

% (-Descartar, +R): Este caso tenemos una sola solución si la sublista R aparece una sola vez en L pero multiples soluciones en caso contrario. Por ejemplo sublista(Descartar, 3, [5,1,2,3,7,1,2,3,10], [1,2,3])
% devuelve dos soluciones: 
%   Descartar = 1, que surge de eliminar el 5 y luego quedarnos con los 3 primeros elementos del resto.
%   Descartar = 5, que surge de eliminar todos los elementos hasta el 7 y luego quedarnos con los 3 primeros elementos del resto.
% El predicado va a encontrar todas estas soluciones y por lo tanto es válido. Se generarán todas las combinaciones de prefijos y sufijos de L, donde cada prefijo unificará su longitud Descartar
% y al final solo unificarán por completo aquellos Descartar que hayan provocado que el sufijo de L tenga a su vez un prefijo que sea igual a R y tenga longitud Tomar. Aquellos Descartar que no unifiquen
% por completo serán los que estén asociados a un prefijo, cuyo sufijo asociado no tenga a su vez un prefijo igual a R de longitud Tomar y esto va a pasar porque en esos casos se descartó una cantidad 
% incorrecta de elementos.

% (-Descartar, -R): En este caso las soluciones van a ser abundantes. Queremos a partir de una lista L y un valor Tomar, encontrar todas las sublistas de L que surgen de descartar
% los primeros Descartar elementos de L y tomar los primeros Tomar elementos del resto. También va a ser válido y nuestro predicado va a devolver todas estas soluciones. 
% Se generan todas las combinaciones de prefijos y sufijos de L. Descartar va a unificar con la longitud de cada prefijo y al final R también va a unificar siempre y cuando despues de eliminar
% los primeros Descartar elementos, la lista restante sea de longitud mayor o igual a Tomar, donde R va a terminar siendo el prefijo de longitud Tomar de la lista restante. Todos aquellos prefijos
% de longitud Descartar que estén asociados a sufijos de longitud menor a Tomar no van a terminar unificando porque el predicado final length(R, Tomar) no va a unificar ya que todos los prefijos generados 
% por el predicado append(R, _, S) van a tener menor longitud que Tomar. Por ejemplo sublista(Descartar, 3, [1, 2, 3, 4, 5], R) devuelve tres soluciones:
% Descartar = 0, R = [1, 2, 3], que surge de tomar el prefijo vacío, el sufijo [1, 2, 3, 4, 5] y tomar los primeros 3 elementos de esa lista.
% Descartar = 1, R = [2, 3, 4], que surge de tomar el prefijo [1], el sufijo [2, 3, 4, 5] y tomar los primeros 3 elementos de la lista.
% Descartar = 2, R = [3, 4, 5], que surge de tomar el prefijo [1, 2], el sufijo [3, 4, 5] y tomar los primeros 3 elementos de la lista.
% No hay mas soluciones porque después se toma el prefijo [1, 2, 3], el sufijo [4, 5] y no tiene suficientes elementos (3) para tomar. Lo mismo pasa al tomar los prefijos [1, 2, 3, 4] y [1, 2, 3, 4, 5].

% Tablero(+K, -T)
tablero(K, T) :-
    length(T, 5),                   % Definimos que nuestro tablero tiene dimensión 5
    maplist(filaLongitudK(K), T).   % Mapeamos a cada elemento de nuestro tablero una fila de longitud K.

% filaLongitudK(+K, -Fila)
filaLongitudK(K, Fila) :- length(Fila, K).

% tamaño(+M, -F, -C): True si M tiene F filas y C columnas.
tamaño(M, F, C) :-
    length(M, F),
    maplist(filaLongitudK(C), M).

% Coordenadas(+T, -IJ): True para todo IJ que sea coordenada de elementos del tablero. Indices(1...5, 1...K).
coordenadas(T, IJ) :-
    tamaño(T, F, C),
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
    unaLineaDePiezas(K, RestoL, RestoPS).       % Agregamos piezas con lo demas.

% SeccionTablero(+T,+ALTO,+ANCHO,+IJ,?ST). True si ST es una sección de ALTO x ANCHO de T a partir de IJ.

% Sabemos que T = Kx5. Entonces caso base, que ALTO, ANCHO e IJ=(i,j) estén dentro de los límites.
% Esto implica, ALTO < 5, ANCHO < K, i + ALTO < 5, j + ANCHO < K.
% dentroDeLosLimites(+T, +ALTO, +ANCHO, +C)
dentroDeLosLimites([H|_], ALTO, ANCHO,(I, J)) :-
    INuevo is I + ALTO - 1,
    JNuevo is J + ANCHO - 1,   
    INuevo =< 5,
    length(H, K),
    JNuevo =< K.

seccionTablero(T,ALTO,ANCHO,(I,J),ST) :- 
    dentroDeLosLimites(T, ALTO, ANCHO, (I,J)),
    DescartarFilas is I - 1,
    DescartarColumnas is J - 1,
    sublista(DescartarFilas, ALTO, T, Filas),               % Aca descarto I-1 filas, y me quede con ALTO filas.                                       
    maplist(sublista(DescartarColumnas, ANCHO), Filas, ST). % Aca descarto J-1 columnas y me quedo con ANCHO columnas. Para eso le saco misma longitud a cada fila.

% ubicarPieza(+Tablero, +Identificador): Genera todas las ubicaciones de pieza dada en el tablero.
ubicarPieza(Tablero, Identificador) :-
    pieza(Identificador, I),
    tamaño(I, F, C),
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
esLibre(T, (I,J)) :- nth1(I, T, Fila), nth1(J, Fila, Casilla), not(ground(Casilla)).

ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [H|ID]) :-
    ubicarPieza(Tablero, H),
    poda(Poda, Tablero),
    ubicarPiezas(Tablero, Poda, ID).

% llenarTablero(+Poda, +Columnas, -Tablero): Enumera todas las posibles formas de llenar el tablero con
% cantidad de columnas (y piezas indicadas). Es decir, columnas = piezas = K.
% Columnas = K por facilidad. Tablero = T por facilidad

llenarTablero(_, 0, T) :- tablero(0, T).
llenarTablero(Poda, K, T) :-
    tablero(K, T),                  % Creamos un nuevo tablero.
    kPiezas(K, PS),                 % Obtenemos para el tablero todas las posibles combinaciones de piezas que vamos a usar.
    ubicarPiezas(T, Poda, PS),      % Ubicamos todas las piezas.
    ground(T).                      % Nos aseguramos que las piezas ubicadas llenan por completo el tablero viendo que toda casilla eset instanciada.           

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