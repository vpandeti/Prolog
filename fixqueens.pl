board_size(4).
pos(1,2). 
pos(2,4).
pos(4,3).
pos(3,1). 

max_in_list([L], L).
max_in_list([H|Ts], R):- 
	max_in_list(Ts, T), 
	(H > T -> R = H ; R = T).

back_track(K, N) :- 
	N > 0,
	N1 is N-1;
	pos(P1,P2),
	K is P1,
	backtrack(N1).
do(P1, P2, K) :- 
	P1 >= P2 ->
	K is P1;
	K is P2.
moves(N) :- 
	board_size(N).


