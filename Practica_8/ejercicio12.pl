inorder(nil, []).
inorder(bin(I, R, D), L) :- inorder(I, I1), inorder(D, I2), append(I1, [R|I2], L).

arbolConInorder([], nil).
arbolConInorder(L, bin(I, R, D)) :- 
    append(LI, [X, LD], L),
    arbolConInorder(LI, I),
    arbolConInorder(LD, D).

aBB(nil).
aBB(bin(nil, 0, nil)).
aBB(bin(I, R, D)) :- 
    inorder(I, LI), 
    max_list(LI, M1), M1 < R, 
    inorder(D, L2),
    max_list(L2, M2),
    R < M2.

aBBInsertar(X, nil, bin(nil, X, nil)).
aBBInsertar(X, bin(I, X, D), bin(I, X, D)).
aBBInsertar(X, bin(I, Y, D), bin(T, Y, D)) :- X < Y, aBBInsertar(X, I, T).
aBBInsertar(X, bin(I, Y, D), bin(I, Y, T)) :- X > Y, aBBInsertar(X, D, T).