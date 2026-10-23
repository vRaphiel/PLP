% ==============================================================================
% PRÁCTICA 8 - EJERCICIO 5
% Paradigmas de Programación (PLP)
% ==============================================================================

% ------------------------------------------------------------------------------
% i. last(?L, ?U)
% ------------------------------------------------------------------------------
% U es el último elemento de la lista L.
% Definido exclusivamente usando append/3:
% Una lista L tiene a U como último elemento sii L se puede descomponer en
% un prefijo cualquiera (denotado por _) seguido de una lista de un solo
% elemento conteniendo a U: [U].
last(L, U) :-
    append(_, [U], L).

% ------------------------------------------------------------------------------
% ii. reverse(+L, ?R)
% ------------------------------------------------------------------------------
% R contiene los mismos elementos que L, pero en orden inverso.
% Definición inductiva sobre L usando append/3:
% - Caso base: La inversa de la lista vacía [] es [].
% - Caso recursivo: Para invertir [X|Xs], se invierte primero la cola Xs
%   obteniendo RevXs, y luego se agrega el elemento X al final mediante append.
reverse([], []).
reverse([X|Xs], R) :-
    reverse(Xs, RevXs),
    append(RevXs, [X], R).

% ------------------------------------------------------------------------------
% ÁRBOL DE BÚSQUEDA (SLD) PARA: ?- reverse([a, b, c], [c, b, a]).
% ------------------------------------------------------------------------------
%
% Reglas utilizadas:
%   (Rev1) reverse([], []).
%   (Rev2) reverse([X|Xs], R) :- reverse(Xs, RevXs), append(RevXs, [X], R).
%
% Reglas de append:
%   (App1) append([], L, L).
%   (App2) append([H|T], L, [H|R]) :- append(T, L, R).
%
% Derivación SLD (Estrategia Prolog: Leftmost goal first):
%
% 1. ?- reverse([a, b, c], [c, b, a]).
%    No unifica con (Rev1).
%    Unifica con (Rev2) con: {X1 = a, Xs1 = [b, c], R1 = [c, b, a]}
%    -> Nuevo resolvente:
%       ?- reverse([b, c], RevXs1), append(RevXs1, [a], [c, b, a]).
%
% 2. Submeta izquierda: ?- reverse([b, c], RevXs1).
%    Unifica con (Rev2) con: {X2 = b, Xs2 = [c], RevXs1 = R2}
%    -> Nuevo resolvente:
%       ?- reverse([c], RevXs2), append(RevXs2, [b], RevXs1), append(RevXs1, [a], [c, b, a]).
%
% 3. Submeta izquierda: ?- reverse([c], RevXs2).
%    Unifica con (Rev2) con: {X3 = c, Xs3 = [], RevXs2 = R3}
%    -> Nuevo resolvente:
%       ?- reverse([], RevXs3), append(RevXs3, [c], RevXs2), append(RevXs2, [b], RevXs1), append(RevXs1, [a], [c, b, a]).
%
% 4. Submeta izquierda: ?- reverse([], RevXs3).
%    Unifica con (Rev1) con: {RevXs3 = []}
%    -> Nuevo resolvente:
%       ?- append([], [c], RevXs2), append(RevXs2, [b], RevXs1), append(RevXs1, [a], [c, b, a]).
%
% 5. Submeta izquierda: ?- append([], [c], RevXs2).
%    Unifica con (App1) con: {RevXs2 = [c]}
%    -> Nuevo resolvente:
%       ?- append([c], [b], RevXs1), append(RevXs1, [a], [c, b, a]).
%
% 6. Submeta izquierda: ?- append([c], [b], RevXs1).
%    Unifica con (App2) con: {H1 = c, T1 = [], L1 = [b], RevXs1 = [c|R_app1]}
%    -> Submeta: ?- append([], [b], R_app1).
%       Unifica con (App1) con: {R_app1 = [b]} => RevXs1 = [c, b].
%    -> Nuevo resolvente:
%       ?- append([c, b], [a], [c, b, a]).
%
% 7. Submeta izquierda: ?- append([c, b], [a], [c, b, a]).
%    Unifica con (App2) consumiendo 'c':
%    -> Submeta: ?- append([b], [a], [b, a]).
%       Unifica con (App2) consumiendo 'b':
%       -> Submeta: ?- append([], [a], [a]).
%          Unifica con (App1): Éxito.
%
% 8. Resolvente vacío ([]).
%    Éxito: true.

% ------------------------------------------------------------------------------
% iii. prefijo(?P, +L)
% ------------------------------------------------------------------------------
% P es prefijo de la lista L.
% Una lista P es prefijo de L sii existe una lista sufijo (denotada por _) tal que concatenar P con dicho sufijo da como resultado L.
% (Nota: No se agrega el hecho prefijo([], _) porque append([], L, L) ya cubre el caso P = [] de forma natural sin duplicar soluciones).
prefijo(P, L) :- append(P, _, L).

% ------------------------------------------------------------------------------
% iv. sufijo(?S, +L)
% ------------------------------------------------------------------------------
% S es sufijo de la lista L.
% Una lista S es sufijo de L sii existe un prefijo (denotado por _) tal que
% concatenar dicho prefijo con S da como resultado L.
sufijo(S, L) :- append(_, S, L).

% ------------------------------------------------------------------------------
% v. sublista(?S, +L)
% ------------------------------------------------------------------------------
% S es sublista contigua de L.
% Una lista S es sublista de L si L se puede partir en tres partes:
%   L = Prefijo + S + Sufijo
% Es decir:
% 1. Primer corte: Se descarta un Prefijo inicial y se obtiene Resto (append(_, Resto, L)).
% 2. Segundo corte: S es prefijo de ese Resto (append(S, _, Resto)).
%
% (Para evitar que la lista vacía [] se genere múltiples veces —ya que [] es sublista en cada posición de corte—, se define [] como caso base y luego se
%  exige que la sublista buscada sea no vacía [X|Xs]).
sublista([], _).
sublista([X|Xs], L) :-
    append(_, Resto, L),
    append([X|Xs], _, Resto).

% ------------------------------------------------------------------------------
% vi. pertenece(?X, +L)
% ------------------------------------------------------------------------------
% X pertenece a la lista L (análogo al predicado member/2).
% Un elemento X pertenece a L sii la lista unitaria [X] es sublista contigua de L.
% Alternativamente con append directo:
%   append(_, [X|_], L).
pertenece(X, L) :- sublista([X], L).