graph_size(6).
start(1).
dest(6).
edge(1,2,4).
edge(1,3,2).
edge(2,3,5).
edge(2,4,10).
edge(3,5,3).
edge(4,5,4).
edge(4,6,11).

max_in_list([L], L).
max_in_list([H|Ts], R):- 
	max_in_list(Ts, T), 
	(H > T -> R = H ; R = T).

recursion(K) :- 
	graph_size(N),
	edge(E1,E2,V),
	traverse(E1,E2,V,K).
traverse(N, E1, E2,K) :- 
	edge(E1,E2,K).
min_cost(K) :- 
	recursion(K),
	write(K), 
	write('.').