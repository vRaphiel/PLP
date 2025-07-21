% --------- BASE DE DATOS DE OBJETOS ---------

% objeto(ID, Peso, Valor).
objeto(1, 50, 10).
objeto(2, 75, 15).
objeto(3, 60, 5).
objeto(4, 10, 1).

% --------- MOCHILA PRINCIPAL ---------

% mochila(+Capacidad, -ListaDeIDs)
mochila(C, L) :-
    listaObjetos(C, ObjetosValidos),
    unaMochila(C, ObjetosValidos, L).

% --------- FILTRAR OBJETOS POR PESO ---------

% listaObjetos(+Capacidad, -ListaFiltrada)
listaObjetos(C, Lista) :-
    listaObjetosAux(C, Lista, []).

% acumulador: lista ya incluida
% caso base
listaObjetosAux(_, [], _).

% tomar objeto si no está repetido y entra por peso
listaObjetosAux(C, [(ID, P, V) | Resto], Acum) :-
    objeto(ID, P, V),
    P =< C,
    not(member((ID, P, V), Acum)),
    listaObjetosAux(C, Resto, [(ID, P, V) | Acum]).

% o no tomar el objeto actual
listaObjetosAux(C, Resto, Acum) :-
    objeto(ID, P, V),
    not(member((ID, P, V), Acum)),
    listaObjetosAux(C, Resto, [(ID, P, V) | Acum]).

% --------- SELECCIONAR UNA MOCHILA ---------

% unaMochila(+Capacidad, +Objetos, -ListaIDsElegidos)
unaMochila(_, [], []).

% incluir el objeto si entra
unaMochila(C, [(ID, P, _) | R], [ID | L]) :-
    P =< C,
    C1 is C - P,
    unaMochila(C1, R, L).

% o no incluirlo
unaMochila(C, [_ | R], L) :-
    unaMochila(C, R, L).
