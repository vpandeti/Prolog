append([],L,L).
append([H|T], L, [H|Ts]) :- 
	append(T, L, Ts).

reverse(L,R) :-
	rev(L,[],R).

rev([],L,L).
rev([H|T],L,X) :- rev(T,[H|L],X).

del([H|T],H,T).
del([H|T], V, [H|Ts]) :-
	del(T,V,Ts).
	
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- K > 1, K1 is K - 1, element_at(X,L,K1).

per(L,X) :- findall(Z,permute(L,Z),X).

permute([],[]).
permute([H|T],X) :-
	permute(T,Z),
	del(X,H,Z).
	
toarray([],[]).
toarray([H|T], [[H]|Ts]) :-
	toarray(T,Ts).
	
elim1([],[]).
elim1([H|T],[H|T1]) :-
	prefix1(H,T,T2),
	elim1(T2,T1).
	
prefix1(H,[H|T],R) :-
	!,
	prefix1(H,T,R).
prefix1(_H,L,L).

flatten([],[]).
flatten([H|T],L) :-
	!,
	flatten(H,L1),
	flatten(T,L2),
	append(L1,L2,L).
flatten(X,[X]).