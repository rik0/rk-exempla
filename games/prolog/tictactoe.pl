board([_, _, _,
       _, _, _,
       _, _, _]).


index(X_Pos, Y_Pos, Index) :-
    var(Index), nonvar(X_Pos), nonvar(Y_Pos), !,
    Index is X_Pos + 3 * Y_Pos.
index(X_Pos, Y_Pos, Index) :-
    nonvar(Index), var(X_Pos), var(Y_Pos), !,
    X_Pos is Index mod 3,
    Y_Pos is Index // 3.


place(Symbol, X_Pos, Y_Pos, Board) :-
    index(X_Pos, Y_Pos, Index),
    nth0(Index, Board, Symbol).


triples([A, B, C, D, E, F, G, H, I],
        [[A, B, C], [D, E, F], [G, H, I],
         [A, D, G], [B, E, H], [C, F, I],
         [A, E, I], [C, E, G]]).

perfect_triple([X, X, X]) :-
    nonvar(X).

missing_one([X, X, Y]) :-
    nonvar(X), nonvar(Y).
missing_one([X, Y, X]) :-
    nonvar(X), nonvar(Y).
missing_one([Y, X, X]) :-
    nonvar(X), nonvar(Y).


wins(Board) :-
    triples(Board, Triples), !,
    member(X, Triple),
    perfect_triple(X).



