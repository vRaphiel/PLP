% Un árbol binario se representará en Prolog con:
% nil, si es vacio.
% bin(izq, v, der), donde v es el valor del nodo, izq es el subárbol izquierdo y der es el subárbol derecho.

% vacio(+X)
vacio(nil).

% raiz(bin(?I, +R, ?D))
raiz(bin(_, V, _), V).

% altura
altura(nil, 0).
altura(bin(I, _, D), AL) :- altura(I, AI), altura(D, AD), AL is 1+max(AI, AD).

% cantidadDeNodos
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(I,_,D), CN) :- cantidadDeNodos(I, CI), cantidadDeNodos(D, CD), CN is 1 + CI + CD.