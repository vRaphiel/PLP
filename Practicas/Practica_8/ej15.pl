% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 15
% Paradigmas de Programación (PLP)
% ==============================================================================
% Representación:
% La expresión tri(A, B, C) denota un triángulo con lados de longitudes naturales
% A, B y C.
% ==============================================================================

% ------------------------------------------------------------------------------
% i. esTriángulo(+T)
% ------------------------------------------------------------------------------
% Tiene éxito si la estructura tri(A, B, C) representa un triángulo válido.
%
% Propiedad geométrica (Desigualdad Triangular):
% En todo triángulo no degenerado con lados estrictamente positivos (A, B, C > 0):
% Cada lado debe ser estrictamente menor que la suma de los otros dos y
% estrictamente mayor que el valor absoluto de su diferencia:
%
%   A < B + C  y  A > |B - C|
%   B < A + C  y  B > |A - C|
%   C < A + B  y  C > |A - B|
%
% Nota sobre Prolog:
% Los operadores de comparación aritmética (<, >) evalúan expresiones matemáticas
% automáticamente. Por ende, la función 'abs/1' está disponible nativamente
% sin necesidad de descomponer en casos B > C o B =< C manualmente.
esTriangulo(tri(A, B, C)) :-
    A > 0, B > 0, C > 0,
    A < B + C, A > abs(B - C),
    B < A + C, B > abs(A - C),
    C < A + B, C > abs(A - B).

% ------------------------------------------------------------------------------
% ii. perímetro(?T, ?P)
% ------------------------------------------------------------------------------
% Verdadero cuando T es un triángulo válido y P es su perímetro (P = A + B + C).
%
% Debe funcionar para los 4 modos de instanciación:
% 1. (+T, +P): Ambos instanciados -> verifica validez y coincidencia del perímetro.
% 2. (+T, -P): Triángulo dado, perímetro libre -> calcula P.
% 3. (-T, +P): Perímetro dado, triángulo libre -> genera los triángulos de perímetro P.
% 4. (-T, -P): Ambos libres -> genera infinitamente todos los triángulos y sus perímetros.
%
% ARQUITECTURA EN 2 CLÁUSULAS (Patrón de Reversibilidad Aritmética):
%
% [Cláusula 1: T está completamente instanciado (+T)]
% Si el triángulo ya viene dado, no necesitamos generar nada. Verificamos que sea
% un triángulo válido y evaluamos 'P is A + B + C'.
% - Si P venía libre (-P), 'is' le asigna la suma.
% - Si P venía dado (+P), 'is' compara numéricamente si coincide con la suma.
perimetro(tri(A, B, C), P) :-
    ground(tri(A, B, C)),
    esTriangulo(tri(A, B, C)),
    P is A + B + C.

% [Cláusula 2: T NO está completamente instanciado (-T)]
% Usamos el patrón GENERATE & TEST:
% 1. GENERATE: 'generarTripla(A, B, C, P)' propone enteros positivos que suman P.
% 2. TEST: 'esTriangulo(tri(A, B, C))' filtra las triplas que cumplan la desigualdad.
perimetro(tri(A, B, C), P) :-
    not(ground(tri(A, B, C))),
    generarTripla(A, B, C, P),
    esTriangulo(tri(A, B, C)).

% ------------------------------------------------------------------------------
% GENERADOR DE TRIPLAS Y PERÍMETRO (Auxiliares de ii)
% ------------------------------------------------------------------------------
% generarP(?P)
% Asegura que la suma P tenga un valor numérico concreto antes de acotar los lados:
% - Si P ya viene instanciado (nonvar): usamos ese valor directamente (finito).
% - Si P viene libre (var): 'desde(3, P)' propone sumas crecientes 3, 4, 5, ...
% (El menor triángulo posible con lados naturales positivos es tri(1,1,1) de P = 3).
generarP(P) :- nonvar(P).
generarP(P) :- var(P), desde(3, P).

% generarTripla(-A, -B, -C, ?P)
% Genera todas las triplas de naturales positivos (A, B, C) que suman P.
% Para evitar choice points infinitos, fijamos P y acotamos A y B finitamente:
% - A varía entre 1 y P.
% - B varía entre 1 y P.
% - C queda unívocamente determinado por resta aritmética: C = P - (A + B).
generarTripla(A, B, C, P) :-
    generarP(P),
    between(1, P, A),
    between(1, P, B),
    C is P - (A + B),
    C > 0.

% desde(+X, -Y)
% Generador infinito de números naturales a partir de X.
desde(X, X).
desde(X, Y) :-
    N is X + 1,
    desde(N, Y).

% ------------------------------------------------------------------------------
% iii. triángulo(-T)
% ------------------------------------------------------------------------------
% Genera uno a uno todos los triángulos válidos, sin repetir resultados.
% Como 'perimetro(?T, ?P)' en el modo (-T, -P) ya genera exhaustivamente todos
% los triángulos del universo ordenados por perímetro creciente, simplemente
% delegamos en perimetro/2 ignorando la variable del perímetro con '_'.
triangulo(T) :-
    perimetro(T, _).

% ------------------------------------------------------------------------------
% CONSULTAS DE PRUEBA
% ------------------------------------------------------------------------------
% 1. (+T, +P):
%    ?- perimetro(tri(3, 4, 5), 12).
%    true.
%
% 2. (+T, -P):
%    ?- perimetro(tri(2, 2, 2), P).
%    P = 6.
%
% 3. (-T, +P) [Generación finita para perímetro 5]:
%    ?- perimetro(T, 5).
%    T = tri(1, 2, 2) ;
%    T = tri(2, 1, 2) ;
%    T = tri(2, 2, 1) ;
%    false.
%
% 4. (-T, -P) [Generación infinita global]:
%    ?- perimetro(T, P).
%    T = tri(1, 1, 1), P = 3 ;
%    T = tri(1, 2, 2), P = 5 ;
%    T = tri(2, 1, 2), P = 5 ;
%    T = tri(2, 2, 1), P = 5 ;
%    T = tri(2, 2, 2), P = 6 ; ...
%
% 5. triángulo(-T):
%    ?- triangulo(T).
%    T = tri(1, 1, 1) ;
%    T = tri(1, 2, 2) ;
%    T = tri(2, 1, 2) ;
%    T = tri(2, 2, 1) ; ...

% ==============================================================================
% APÉNDICE: MAPA MENTAL DEFINITIVO DE PROLOG (LOS 3 PATRONES DE PLP)
% ==============================================================================
%
%                              ¿QUÉ TE PIDE EL PROBLEMA?
%                                          │
%       ┌──────────────────────────────────┼──────────────────────────────────┐
%       ▼                                  ▼                                  ▼
% [1. Inducción Estructural]       [2. Generación Infinita]        [3. Reversibilidad Aritmética]
%   Listas y Árboles                 Espacios N o N x N              Signaturas con '?'
%   (inorder, aplanar,               (desde, coprimos,               (ej 9 desdeReversible,
%    intercalar, aBB)                 cuadradoSemiMagico)             ej 15 perimetro(?T, ?P))
%
% ------------------------------------------------------------------------------
% 1. INDUCCIÓN ESTRUCTURAL (Listas y Árboles)
% ------------------------------------------------------------------------------
% - Cuándo se aplica: Cuando el dato de entrada ya tiene una estructura algebraica
%   recursiva definida ([] vs [X|Xs], nil vs bin(I, R, D)).
% - Cómo se piensa:
%     * Caso base: la estructura vacía ([] o nil).
%     * Caso recursivo: se consume un elemento y se delega en la subestructura.
% - Herramientas: NO necesitás var, nonvar, ground ni desde. La estructura misma
%   guía la ejecución y garantiza terminación finita.
%
% ------------------------------------------------------------------------------
% 2. GENERACIÓN INFINITA (El plano cartesiano N x N)
% ------------------------------------------------------------------------------
% - Cuándo se aplica: Cuando te piden generar infinitas soluciones numéricas que
%   no vienen dadas en una lista (coprimos, sumas crecientes, matrices mágicas).
% - Regla de Oro: UN ÚNICO generador infinito ('desde/2').
%   Todo lo demás se acota finitamente con 'between' o restas aritméticas.
%   * En 1D: desde(0, K).
%   * En 2D (Diagonales de Cantor): fijar la suma S = X + Y con 'desde(2, S)',
%     y luego 'between(1, S - 1, X)' e 'Y is S - X'.
%
% ------------------------------------------------------------------------------
% 3. REVERSIBILIDAD ARITMÉTICA Y GENERATE & TEST (?Entrada, ?Salida)
% ------------------------------------------------------------------------------
% - Por qué existe este patrón:
%   Los operadores matemáticos de Prolog (is, <, >, =<, >=) son UNIDIRECCIONALES;
%   exigen que sus operandos ya contengan números concretos.
%   * Si los datos entran instanciados (+), hacemos la cuenta directa: P is A + B + C.
%   * Si los datos entran libres (-), 'is' falla; debemos primero GENERAR candidatos
%     (con between o desde) y luego TESTEAR si cumplen las condiciones.
% - Molde canónico de 2 cláusulas disjuntas (usando ground y not):
%
%     predicado(Dato, Salida) :-
%         ground(Dato),                  % 1. Entrada completa -> Cálculo directo
%         chequeoValidez(Dato),
%         Salida is ...
%
%     predicado(Dato, Salida) :-
%         not(ground(Dato)),             % 2. Entrada libre -> Generate & Test
%         generarCandidatos(Dato, Salida),
%         chequeoValidez(Dato).
%
% ==============================================================================