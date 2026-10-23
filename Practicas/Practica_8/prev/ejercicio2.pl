vecino(X, Y, [X|[Y|Ls]]).
vecino(X, Y, [W|Ls]) :- vecino(X,Y,Ls).

/*
?- vecino(5, Y, [5,6,5,3]).
├─ ?- vecino(5, 6, [5|[6|[5,3]]]).          {X := 5, Y := 6, Ls := [5,3]}
│  └─ true.                                 {Y := 6}
└─ ?- vecino(5, Y, [6,5,3]).                {X := 5, W := 5, Ls := [6,5,3]}
   └─ ?- vecino(5, Y, [5,3]).               {X := 5, W := 6, Ls := [5,3]}
      ├─ ?- vecino(5, 3, [5|[3|[]]]).       {X := 5, Y := 3, Ls := []}
      │  └─ true.                           {Y := 3}
      └─ ?- vecino(5, Y, [3]).              {X := 5, W := 5, Ls := [3]}
         └─ ?- vecino(5, Y, []).            {X := 5, W := 3, Ls := []}
            └─ false.
*/

/*
Si pero en orden inverso. Porque primero va a hacer los casos recursivos y luego va a hacer cada caso donde encuentre los vecinos.
*/

vecino2(X, Y, [W|Ls]) :- vecino(X,Y,Ls).
vecino2(X, Y, [X|[Y|Ls]]).
