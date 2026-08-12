% P01 (*) Find the last element of a list.
% Example:
% ?- my_last(X,[a,b,c,d]).
% X = d

my_last(X, [X | []]).
my_last(X, [_ | YS]) :- my_last(X, YS).

% P03 (*) Find the K'th element of a list.
% The first element in the list is number 1.
% Example:
% ?- element_at(X,[a,b,c,d,e],3).
% X = c

element_at(X, [X | _], 0).
element_at(X, [_ | YS], N) :- N > 0, N1 is N -1, element_at(X, YS, N1).

% P04 (*) Find the number of elements of a list.
size(X, L) :- length(X, L).

% P05 (*) Reverse a list.
reverse_l([], []).
reverse_l([X | XS], R) :- reverse(XS, RS), append(RS, [X], R).

% P06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
palindrome(X) :- reverse_l(X, RS), isPalindrome(X, RS).
isPalindrome(X, X).

% P07 (**) Flatten a nested list structure.
% Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
% Example:
% ?- my_flatten([a, [b, [c, d], e]], X).
% X = [a, b, c, d, e]
% Hint: Use the predefined predicates is_list/1 and append/3
my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([],[]).
my_flatten([X|Xs],Zs) :- my_flatten(X,Y), my_flatten(Xs,Ys), append(Y,Ys,Zs).

% P08 (**) Eliminate consecutive duplicates of list elements.
% If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
% Example:
% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [a,b,c,a,d,e]
compress([], []).
compress([X], [X]).
compress([X, X | XS], ZS) :- compress([X | XS], ZS).
compress([X, Y | XS], [X | R]) :-
    X \= Y,
    compress([Y | XS], R).

% P09 (**) Pack consecutive duplicates of list elements into sublists.
% If a list contains repeated elements they should be placed in separate sublists.
% Example:
% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([],[]).
pack([X|Xs],[Z|Zs]) :- transfer(X,Xs,Ys,Z), pack(Ys,Zs).

% transfer(X,Xs,Ys,Z) Ys is the list that remains from the list Xs
%    when all leading copies of X are removed and transfered to Z

transfer(X,[],[],[X]).
transfer(X,[Y|Ys],[Y|Ys],[X]) :- X \= Y.
transfer(X,[X|Xs],Ys,[X|Zs]) :- transfer(X,Xs,Ys,Zs).

% P10 (*) Run-length encoding of a list.
% Use the result of problem P09 to implement the so-called run-length encoding data compression method.
% Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
% Example:
% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]

% P11 (*) Modified run-length encoding.
% Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
% Only elements with duplicates are transferred as [N,E] terms.
% Example:
% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% P12 (**) Decode a run-length encoded list.
% Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

% P13 (**) Run-length encoding of a list (direct solution).
% Implement the so-called run-length encoding data compression method directly.
% I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them.
% As in problem P11, simplify the result list by replacing the singleton terms [1,X] by X.
% Example:
% ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
% X = [[4,a],b,[2,c],[2,a],d,[4,e]]

% P14 (*) Duplicate the elements of a list.
% Example:
% ?- dupli([a,b,c,c,d],X).
% X = [a,a,b,b,c,c,c,c,d,d]

% P15 (**) Duplicate the elements of a list a given number of times.
% Example:
% ?- dupli([a,b,c],3,X).
% X = [a,a,a,b,b,b,c,c,c]
% What are the results of the goal:
% ?- dupli(X,3,Y).

% P16 (**) Drop every N'th element from a list.
% Example:
% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
% X = [a,b,d,e,g,h,k]

% P17 (*) Split a list into two parts; the length of the first part is given.
% Do not use any predefined predicates.
% Example:
% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
% L1 = [a,b,c]
% L2 = [d,e,f,g,h,i,k]

% P18 (**) Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
% Example:
% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
% X = [c,d,e,f,g]

% P19 (**) Rotate a list N places to the left.
% Examples:
% ?- rotate([a,b,c,d,e,f,g,h],3,X).
% X = [d,e,f,g,h,a,b,c]
% ?- rotate([a,b,c,d,e,f,g,h],-2,X).
% X = [g,h,a,b,c,d,e,f]
% Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17.

% P20 (*) Remove the K'th element from a list.
% Example:
% ?- remove_at(X,[a,b,c,d],2,R).
% X = b
% R = [a,c,d]

% P21 (*) Insert an element at a given position into a list.
% Example:
% ?- insert_at(alfa,[a,b,c,d],2,L).
% L = [a,alfa,b,c,d]

% P22 (*) Create a list containing all integers within a given range.
% Example:
% ?- range(4,9,L).
% L = [4,5,6,7,8,9]

% P23 (**) Extract a given number of randomly selected elements from a list.
% The selected items shall be put into a result list.
% Example:
% ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
% L = [e,d,a]
% Hint: Use the built-in random number generator random/2 and the result of problem P20.

% P24 (*) Lotto: Draw N different random numbers from the set 1..M.
% The selected numbers shall be put into a result list.
% Example:
% ?- rnd_select(6,49,L).
% L = [23,1,17,33,21,37]
% Hint: Combine the solutions of problems P22 and P23.