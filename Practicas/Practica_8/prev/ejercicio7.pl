interseccion([], _, []). 

% - H no pertenece a L2, seguimos con nuestra historia
interseccion([H|L1], L2, L3) :- 
    not(member(H, L2)),
    interseccion(L1, L2, L3).

% - H pertenece a L2, entonces vemos que pasa con lo demas
interseccion([H|L1], L2, [H|L3]) :- 
    member(H, L2),
    interseccion(L1, L2, L3).

% - Partir(N,L,L1,L2) donde L1 tiene los primeros N elementos de L y L2 el resto.
partir(0, L, [], L).
partir(N, [H|T], [H|T1], L2) :- M is N - 1, partir(M, T, T1, L2).

% - borrar(+LO, +X, -LS): Elimina ocurrencias de X de la lista original
borrar([], _, []).
% Como en cada paso recursivo vemos la cabeza de la lista, si es el mismo elemento, lo quitamos.
borrar([X|LO], X, LS) :- borrar(LO, X, LS).
borrar([Y|LO], X, [Y|LS]) :- X \= Y, borrar(LO, X, LS).

% - sacarDuplicados(+L1, -L2): Saca duplicados de L1.
sacarDuplicados([], []).
sacarDuplicados([H|T], [H|L2]) :- member(H, T), borrar(T, H, Z), sacarDuplicados(Z, L2).
sacarDuplicados([H|T], [H|L2]) :- not(member(H, T)), sacarDuplicados(T, L2).

% - permutacion(+L1, ?L2) tiene exito si L2 es permutacion de L1
permutacion([], []).
permutacion([X|Xs], Ys) :- permutacion(Xs, Zs), insertar(X, Zs, Ys).

insertar(X, L, LX) :- append(P, S, L), append(P, [X|S], LX).

% - Reparto(+L, +N, -LListas) que tiene exitos si LListas es de N listas de cualquier longitud tales que al concatenarlas se convierte en la lista L.
reparto([], 0, []).
reparto(L, N, [X|LL]) :- N > 0, append(X, L2, L), N2 is N-1, reparto(L2, N2, LL).

% - RepartoSinVacias
repartoSinVacias(L, Xs) :-
    length(L, N),
    between(1, N, K),   % Generamos todas los posibles K = cantidades de sublistas.
    reparto(L, K, Xs),  % Repartimos en K sublistas.
    not((member(X, Xs), length(X, 0))). % No pueden haber sublistas vacías.
