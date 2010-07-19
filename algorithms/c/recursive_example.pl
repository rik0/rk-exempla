solve(0, [], []).
solve(0, [_V|Vs], [0|Ms]) :-
    solve(0, Vs, Ms).
solve(N, [], []) :-
    N \== 0,
    !, fail.
solve(Tot, [V|Vs], [M|Ms]) :-
    (M=0, solve(Tot, Vs, Ms))
    ;
    (M=1, Tot1 is Tot - V, solve(Tot1, Vs, Ms)).



tests([
       [5, 7, 3, 1],
       [5, 7, 3, 1],
       [5, 7, 1, 3],
       [8, 2, 2, 1],
       [13, 0, 0, 0],
       [0, 10, 0, 3],
       [15, -5, 3, 4],
       [15, -5, 4, 3],
       [1, 2, 3, 4]
      ]).

test(X) :-
    (solve(13, X, R) ->
        write(X), write(' '), write(R), write(' ok.'), nl
    ;
        write(X), write(' '), write(fail), nl
    ).

test_all([X|Xs]) :-
    test(X), test_all(Xs).
test_all([]).

test_all :-
    tests(Ts), test_all(Ts).
