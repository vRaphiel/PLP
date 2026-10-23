% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 9
% Paradigmas de Programación (PLP)
% ==============================================================================

% ------------------------------------------------------------------------------
% Código base dado en la consigna:
% ------------------------------------------------------------------------------
desde(X, X).
desde(X, Y) :- N is X + 1, desde(N, Y).

% ------------------------------------------------------------------------------
% i. ¿Cómo deben instanciarse los parámetros para que el predicado funcione?
%    (Es decir, para que no se cuelgue ni produzca un error). ¿Por qué?
% ------------------------------------------------------------------------------
% El predicado debe invocarse estrictamente con el patrón de instanciación:
%
%                             desde(+X, -Y)
%
% Justificación detallada:
%
% 1. Sobre el primer argumento (+X):
%    Debe estar obligatoriamente instanciado. La segunda cláusula evalúa la
%    expresión aritmética `N is X + 1`. El operador `is/2` en Prolog exige que
%    todas las variables del lado derecho estén ligadas a números en el momento
%    de la evaluación. Si X fuese una variable libre (-X), la llamada arroja:
%      "ERROR: Arguments are not sufficiently instantiated".
%
% 2. Sobre el segundo argumento (-Y):
%    Debe ser una variable libre. Analicemos qué ocurre si Y viene instanciado (+Y):
%    - Si X > Y (ej: ?- desde(5, 3).):
%      La primera regla falla (5 \= 3). Entra a la segunda regla y calcula N = 6,
%      luego N = 7, etc. Como N aumenta monótonamente hacia +infinito, jamás
%      alcanzará a Y (que es menor). El motor cae en un bucle infinito y se cuelga
%      de inmediato sin devolver ninguna solución.
%
%    - Si X =< Y (ej: ?- desde(3, 5).):
%      Alcanza eventualmente la meta `desde(5, 5)` y la primera cláusula tiene
%      éxito, mostrando `true`. SIN EMBARGO, en Prolog un hecho NO es un return:
%      la segunda regla `desde(X, Y) :- ...` también unifica con `desde(5, 5)`,
%      por lo que Prolog deja un Choice Point (punto de elección) pendiente.
%      Si el usuario pide más soluciones con ';' o si esta meta forma parte de una
%      regla mayor que retrocede por backtracking, Prolog explora la alternativa,
%      calcula N = 6, 7, 8... y diverge para siempre en una rama infinita.
%
% Por lo tanto, el único modo donde el predicado no arroja errores ni genera ramas
% infinitas espurias en el árbol SLD es como generador: desde(+X, -Y).

% ------------------------------------------------------------------------------
% ii. desdeReversible(+X, ?Y)
% ------------------------------------------------------------------------------
% Nueva versión que admite ?Y:
% - Si Y está instanciada (+Y): es verdadero sii Y >= X (y termina finitamente).
% - Si Y no está instanciada (-Y): genera todos los enteros sucesivos desde X en adelante.
%
% Para separar ambos comportamientos se utilizan los metapredicados de inspección:
% - var(Y): tiene éxito sii Y es una variable libre (no ligada).
% - nonvar(Y): tiene éxito sii Y ya está instanciada.

% 1. Caso base (cubre el caso X = Y para cualquier instanciación):
desdeReversible(X, X).

% 2. Modo generador (-Y):
%    Solo se explora si Y es libre. Genera X + 1, X + 2, ... mediante backtracking.
desdeReversible(X, Y) :-
    var(Y),
    N is X + 1,
    desdeReversible(N, Y).

% 3. Modo verificación (+Y):
%    Solo se explora si Y ya vino instanciado. Verifica si X < Y directamente.
%    (Notar que el caso X = Y ya fue satisfecho por la regla 1; al exigir X < Y
%     garantizamos que esta regla sea estrictamente disjunta y no genere duplicados).
%    Al no hacer recursión, termina inmediatamente con éxito y sin choice points infinitos.
desdeReversible(X, Y) :-
    nonvar(Y),
    X < Y.

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% 1. Modo verificación exitoso (no se cuelga al pedir más soluciones):
%    ?- desdeReversible(3, 5).
%    true.
%
% 2. Modo verificación fallido (falla inmediatamente, no se cuelga):
%    ?- desdeReversible(5, 3).
%    false.
%
% 3. Modo generador (genera de a uno hacia el infinito):
%    ?- desdeReversible(3, Y).
%    Y = 3 ;
%    Y = 4 ;
%    Y = 5 ;
%    ...
