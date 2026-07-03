secuenciaRepetida([X], S) :- subLista(X, S).
secuenciaRepetida([X | XS], S) :- subLista(X, S), secuenciaRepetida(XS, S).

subLista([X | XS], [X | YS]) :- append(YS, _, XS).
subLista([_ | XS], L) :- subLista(XS, L).

secuenciaMaxima(M, S) :- secuenciaRepetida(M, S), length(S, LS), 
    not((
        secuenciaRepetida(M, L), 
        length(L, LL), 
        LL > LS
    )).

natural(0).
natural(succ(N)) :- natural(N).
mayor(succ(N), 0) :- natural(N).
mayor(succ(X), succ(Y)) :- mayor(X, Y).

todasLasMatrices(M) :- 
    length(_, X),
    X > 1,
    between(1, X, Y),
    length(M, Y),
    agregarFilas(M, X).

agregarFilas([], _).
agregarFilas([Fila | Resto], Columnas) :-
    length(Fila, Columnas),
    agregarFilas(Resto, Columnas).

% ------
nota(do). 
submelodia(sec(M,N) ,sec(M,N)). 
submelodia(sec(M,_),S) :- submelodia(M,S). 
submelodia(sec(_,N),S) :- submelodia(N,S). 
melodia(M) :- nota(M). 
melodia(sec(M,N)) :- melodia(M), melodia(N). 