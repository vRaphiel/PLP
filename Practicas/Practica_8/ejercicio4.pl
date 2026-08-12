%- juntar(?Lista1, ?Lista2, ?Lista3).
%- juntar([], [], []).
%- juntar([X|Ls], [], [X|Ls]).
%- juntar([], [Y|Ls], [Y|Ls]).
%- juntar([X|Ls1], Ls2, [X|Ls3]) :- juntar(Ls1, Ls2, Ls3). 

% - No esta mal, pero hice mas predicados
% - Lo correcto seria:
juntar([], L2, L2).
juntar([X|L1], L2, [X|L3]) :- juntar(L1, L2, L3).
