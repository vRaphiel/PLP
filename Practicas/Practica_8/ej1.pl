% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 1
% Paradigmas de Programación (PLP)
% ==============================================================================

% Base de conocimiento dada
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).

% abuelo(?Abuelo, ?Nieto)
abuelo(X, Y) :- 
    padre(X, Z), 
    padre(Z, Y).

% ------------------------------------------------------------------------------
% i. ¿Cuál es el resultado de la consulta abuelo(X, manuel)?
% ------------------------------------------------------------------------------
% Consulta:
%   ?- abuelo(X, manuel).
%
% Resultado:
%   X = juan ;
%   false.
%
% Justificación:
%   Prolog busca Z tal que padre(Z, manuel), unificando con Z = luis.
%   Luego busca X tal que padre(X, luis), unificando con X = juan.
%   Al pedir más soluciones con ';', no hay más alternativas para Z ni para X,
%   por lo que la consulta termina con false.

% ------------------------------------------------------------------------------
% ii. Definición de predicados binarios: hijo, hermano y descendiente
% ------------------------------------------------------------------------------

% hijo(?Hijo, ?Padre)
% Verdadero si Padre es padre de Hijo.
% (No se incluye ninguna regla que fuerce false; por el supuesto de mundo cerrado
%  CWA de Prolog, cualquier consulta que no se deduzca falla directamente).
hijo(Hijo, Padre) :-
    padre(Padre, Hijo).

% hermano(?X, ?Y)
% Verdadero si X e Y son personas distintas que comparten el mismo padre.
hermano(X, Y) :-
    padre(P, X),
    padre(P, Y),
    X \= Y.

% descendiente(?Descendiente, ?Ancestro)
% Definición inductiva pura:
% - Caso base: Hijo directo.
% - Caso recursivo: Descendiente a través de una cadena de paternidad.
% Notar que colocar padre(Ancestro, Z) antes de la llamada recursiva acota
% la búsqueda y previene recursiones infinitas a izquierda.
descendiente(Descendiente, Ancestro) :-
    padre(Ancestro, Descendiente).
descendiente(Descendiente, Ancestro) :-
    padre(Ancestro, Z),
    descendiente(Descendiente, Z).

% ------------------------------------------------------------------------------
% iii. Árbol de búsqueda de Prolog para la consulta descendiente(Alguien, juan)
% ------------------------------------------------------------------------------
/*
Árbol SLD para ?- descendiente(Alguien, juan).

                       ?- descendiente(Alguien, juan).
                        /                           \
(Regla 1: desc(D, A) :- padre(A, D))           (Regla 2: desc(D, A) :- padre(A, Z), desc(D, Z))
{A := juan, D := Alguien}                      {A := juan, D := Alguien}
            |                                               |
  ?- padre(juan, Alguien).                        ?- padre(juan, Z), descendiente(Alguien, Z).
    /                   \                          /                                         \
{Alguien := carlos}   {Alguien := luis}      {Z := carlos}                                 {Z := luis}
    |                     |                        |                                             |
  true.                 true.          ?- descendiente(Alguien, carlos).             ?- descendiente(Alguien, luis).
[Sol 1: Alguien=carlos] [Sol 2: Alguien=luis]     /                  \                          /                \
                                                (R1)                (R2)                      (R1)              (R2)
                                                 |                   |                         |                 |
                                      ?- padre(carlos, Alguien) ?- padre(carlos, Z1),    ?- padre(luis, Alguien) ?- padre(luis, Z2),
                                        /             \            desc(Alguien, Z1)        /    |     \            desc(Alguien, Z2)
                                 {Alg := daniel}  {Alg := diego}         |           {pablo} {manuel} {ramiro}           |
                                       |                 |          {Z1 := daniel}      |       |        |           {Z2 := pablo, ...}
                                     true.             true.             |            true.   true.    true.             |
                              [Sol 3: daniel]   [Sol 4: diego]  ?- desc(Alg, daniel)   [Sol 5] [Sol 6]  [Sol 7]   ?- desc(Alg, pablo)...
                                                                    /             \                                 |
                                                                  (R1)           (R2)                             (ambas fallan:
                                                                    |              |                              pablo no es padre)
                                                            ?- padre(daniel, Alg)  ?- padre(daniel, Z2), ...        |
                                                                    |                       |                     false.
                                                                  false.                  false.
                                                            (daniel no es padre)   (daniel no es padre)
                                                                    |                       |
                                                               (ídem diego)            (ídem diego)

Respuestas obtenidas en orden:
  1. Alguien = carlos
  2. Alguien = luis
  3. Alguien = daniel
  4. Alguien = diego
  5. Alguien = pablo
  6. Alguien = manuel
  7. Alguien = ramiro ;
  false.
*/

% ------------------------------------------------------------------------------
% iv. ¿Qué consulta habría que hacer para encontrar a los nietos de juan?
% ------------------------------------------------------------------------------
% Consulta:
%   ?- abuelo(juan, Nieto).
%
% Alternativamente, usando el predicado padre directamente:
%   ?- padre(juan, Hijo), padre(Hijo, Nieto).
%
% Genera sucesivamente:
%   Nieto = daniel ;
%   Nieto = diego ;
%   Nieto = pablo ;
%   Nieto = manuel ;
%   Nieto = ramiro.

% ------------------------------------------------------------------------------
% v. ¿Cómo se puede definir una consulta para conocer a todos los hermanos de pablo?
% ------------------------------------------------------------------------------
% Consulta:
%   ?- hermano(Hermano, pablo).
%
% (O simétricamente: ?- hermano(pablo, Hermano).)
% Genera sucesivamente:
%   Hermano = manuel ;
%   Hermano = ramiro ;
%   false.

% ------------------------------------------------------------------------------
% vi, vii y viii. Análisis del predicado ancestro
% ------------------------------------------------------------------------------
/*
vi. Considerar el agregado de:
    ancestro(X, X).
    ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

vii. Explicar la respuesta a la consulta ?- ancestro(juan, X).
     ¿Qué sucede si se pide más de un resultado?

     Respuesta y Explicación:
     1) Primer resultado:
        Unifica con el hecho ancestro(X, X) con la sustitución {X := juan}.
        Prolog devuelve inmediatamente:
          X = juan.

     2) Al pedir más resultados (backtracking mediante ';'):
        Prolog intenta la segunda regla:
          ancestro(juan, X) :- ancestro(Z, X), padre(juan, Z).
        El primer subobjetivo a resolver es:
          ?- ancestro(Z, X).
        Como Z y X son variables no instanciadas, Prolog vuelve a probar:
          a) El hecho ancestro(X, X): unifica Z = X.
             Luego evalúa el segundo subobjetivo padre(juan, X), obteniendo:
               X = carlos ;
               X = luis.
          b) La regla recursiva:
             ?- ancestro(Z1, X), padre(Z, Z1).
             Nuevamente, el primer subobjetivo es ancestro(Z1, X) con variables libres.
             Esto genera un bucle de recursión a izquierda (left-recursion):
             Prolog sigue expandiendo infinitamente metas de la forma:
               ancestro(Z2, X), ancestro(Z3, X), ancestro(Z4, X)...
             sin consumir ningún hecho de la base de conocimiento que acote la búsqueda.
             Como consecuencia, el programa se cuelga (divergencia infinita / Stack Overflow).

viii. Sugerir una solución reescribiendo el programa de ancestro:
      Para evitar la recursión a izquierda y garantizar que la búsqueda sea finita:
      1. Se debe instanciar la variable intermedia mediante un hecho concreto (padre/2)
         ANTES de realizar la llamada recursiva.
      2. De esa forma, en cada paso recursivo se desciende un nivel en el árbol
         genealógico hasta llegar a hojas sin hijos, donde falla finitamente.
*/

% Versión corregida manteniendo la reflexividad (X es ancestro de sí mismo):
ancestro(X, X).
ancestro(X, Y) :-
    padre(X, Z),
    ancestro(Z, Y).

% Nota: Si la cátedra prefiere ancestro estricto (no reflexivo):
% ancestroEstricto(X, Y) :- padre(X, Y).
% ancestroEstricto(X, Y) :- padre(X, Z), ancestroEstricto(Z, Y).
