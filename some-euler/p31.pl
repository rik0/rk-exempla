:- use_module(library(lists)).
:- use_module(library(clpfd)).

% 1, 2, 5, 10, 20, 50, 100, 200

% solution(How_Many) :-
%     length(How_Many, 8),
%     domain(How_Many, 0, 200),
%     Coins = [200, 100, 50, 20, 10, 5, 2, 1],
%     scalar_product(Coins, How_Many, #=, 200), !,
%     labeling([bisect], How_Many).

% add_domains([Coin|Coins], [Cf|Cfs]) :-
%     Max is 200 // Coin,
%     Cf in 0..Max,
%     add_domains(Coins, Cfs).
% add_domains([],[]).


range(Start, Step, Stop, Range) :-
    range(Start, Step, Stop, [], Range).
range(Start, Step, Stop, Acc, Range) :-
    Start =< Stop, !,
    Next is Start + Step,
    range(Next, Step, Stop, [Start|Acc], Range).
range(_Start, _Step, _Stop, Range, Range).
    

add_domains([Coin|Coins], [Cf|Cfs]) :-
    range(0, Coin, 200, R),
    reverse(R, RR),
    list_to_fdset(RR, S),
    Cf in_set S,
    add_domains(Coins, Cfs).
add_domains([],[]).


solution(Weighted) :-
    Coins = [200, 100, 50, 20, 10, 5, 2, 1],
    add_domains(Coins, Weighted),
    sum(Weighted, #=, 200), !,
    labeling([enum], Weighted).

go :-
    statistics(walltime, _),
    bagof(S, solution(S), Ss),
    statistics(walltime, [_, Elapsed]),
    length(Ss, Ln),
    format("Found ~d solutions in ~3d secs.~n", [Ln, Elapsed]).