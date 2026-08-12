padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X, Y) :- padre(X,Z) , padre(Z,Y).

abuelo(X, manuel).

hijo(X, X) :- false.
hijo(X, Y) :- padre(Y, X).
hermano(X, Y) :- hijo(X, Z), padre(Z, Y).
descendiente(X, Y) :- hijo(Z, Y), hijo(X, Z).

% - Para encontrar a los nietos de juan debo 
% - devolver los hijos de los hijos de juan

nietos(X, Y) :- hijo(Z, Y), hijo(X, Z), not(hijo(X, Y)).

% - ancestro(X, X).
% - ancestro(X, Y) :- ancestro(Z, Y), padre(X, Z).

% - Si se pide mas de un resultado lo va a dar, hasta que ya no haya nadie en la
% - cadena del arbol genealogico. Luego se cuelga.

% - Una posible solucion es ver que si ya no hay mas hijos, no se devuelva nada
ancestro(X, X).
ancestro(X, Y) :- padre(X, Y).
ancestro(X, Y) :- padre(X, Z), ancestro(Z, Y).