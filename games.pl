max_fun(TotalGames, Cap, Refill, List, Result) :- 
	sort(List,L1),
	reverse(L1,M,[]),
	get_first_element(M, First),
	play_game(M, Cap, Refill, 0, X),
	play_max_fun_game(Refill, First, 0, X1),
	Result is X+X1.

get_first_element([H|_], X) :- X is H.
	
play_max_fun_game(Tokens,Fun,Value, X) :- 
	(Tokens =:= 0 -> X is Value;
	T1 is Tokens-1,
	V1 is Value+Fun,
	play_max_fun_game(T1, Fun, V1, X)).
	
play_game([], _, _, V, V).
play_game([H|T],Tokens, Refill, V, X) :- 
	play_one_game(H, Tokens, Refill, 0, Value),
	V1 is V+Value,
	play_game(T, Refill, Refill, V1, X).

play_one_game(_,0,_,V,V).
play_one_game(Value, Tokens, Refill, V, X) :-
	V1 is Value + V,
	T1 is Tokens-1,
	play_one_game(Value, T1, Refill, V1, X).
	
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).