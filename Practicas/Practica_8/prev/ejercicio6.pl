% - aplanar(+XS, -YS). True sii YS contiene los elementos de todos los niveles de XS en el mismo orden de aparicion.
aplanar([], []).
% H puede ser cualquier cosa. Entero, Atomo, Lista.
% Si H es lista, el append se hace bien. Si no, es una nueva historia.
% Recursivamente tenemos que hacer uso del append, pero esta vez checkeando el tipo.
% Podemos juntar dos listas. Pero lo que no podemos hacer

% Caso H es un atomo o un entero:
% Como H es un atomo o entero, aplanamos la cola.
% Luego de aplanar la cola, tenemos el resultado en Z.
% Con el Z vamos a hacer un append de lo que tenemos hasta ahora a modo de lista con lista y ese sera nuestro L

% RARO, pero creo que veo la idea.

aplanar([Hs|Ts], L) :-
    aplanar(Hs, Zs),
    aplanar(Ts, Xs),
    append(Zs, Xs, L).

aplanar([H|T], L) :- 
    not(aplanar(H, _)),
    aplanar(T, Z),
    append([H], Z, L).