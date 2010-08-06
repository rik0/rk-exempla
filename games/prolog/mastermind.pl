:- use_module(library(lists)).
:- dynamic query/3.

mastermind(Code) :-
	cleanup, guess(Code), check(Code), announce.

guess(Code) :-
	Code = [_X1, _X2, _X3, _X4],
	selects(Code, [1,2,3,4,5,6,7,8,9]).

check(Guess) :-
	\+ inconsistent(Guess),
	ask(Guess).

inconsistent(Guess) :-
	query(OldGuess, Bulls, Cows),
	\+ bulls_and_cows_match(OldGuess, Guess, Bulls, Cows).

bulls_and_cows_match(OldGuess, Guess, Bulls, Cows):-
	exact_matches(OldGuess, Guess, N1),
	N1 =:= Bulls,
	common_members(OldGuess, Guess, N2),
	Cows =:= N2-Bulls.

ask(Guess) :-
	repeat,
	format('How many bulls and cows in ~p?~n', [Guess]),
	read((Bulls, Cows)),
	sensible(Bulls, Cows), !,
	assert(query(Guess, Bulls, Cows)),
	Bulls =:= 4.

sensible(Bulls, Cows) :-
	integer(Bulls),
	integer(Cows),
	Bulls + Cows =< 4.


%%	 Helpers
exact_matches(Xs, Ys, N) :-
	exact_matches(Xs, Ys, 0, N).
exact_matches([X|Xs], [Y|Ys], K, N) :-
	(   X = Y ->
	K1 is K + 1
	;   
	K1 is K),
	exact_matches(Xs, Ys, K1, N).
exact_matches([], [], N, N).

common_members(Xs, Ys, N) :-
	common_members(Xs, Ys, 0, N).
common_members([X|Xs], Ys, K, N) :-
	(   member(X, Ys) ->
	K1 is K+1
	;   
	K1 is K),
	common_members(Xs, Ys, K1, N).
common_members([], _Ys, N, N).

cleanup :-
	retractall(query(_,_,_)), !.
cleanup.

announce :-
	size_of(X, query(X, _A, _B), N),
	format('Found the answer after ~d queries.~n', [N]).
%%	 Utilities
selects([X|Xs], Ys) :-
	select(X, Ys, Ys1),
	selects(Xs, Ys1).
selects([], _Ys).

size_of(X, G, N) :-
	findall(X, G, Xs),
	length(Xs, N).
