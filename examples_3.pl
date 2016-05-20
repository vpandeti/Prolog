append([],L,L).
append([X|M],N,[X|Y]) :- append(M,N,Y).

length([],0).
length([X|T],N) :- length(T,M), N is M+1.

fact(N, X) :- N > 0, X1 is N * X, N1 is N-1, fact(N1, X1).

fact1(0, 1).
fact1(N, X) :- N > 0, N1 is N-1, fact1(N1, X1), X is N * X1.

flatten([],[]).
flatten([H|T],L) :-
	!,
	flatten(H,L1),
	flatten(T,L2),
	append(L1,L2,L).
flatten(X,[X]).

del([H|T], H, T).
del([H|T], X, [H|M]) :- del(T,X,M).

elim1([],[]).
elim1([H|T],[H|T1]) :-
	prefix1(H,T,T2),
	elim(T2,T1).
	
prefix1(H,[H|T],R) :-
	!,
	prefix1(H,T,R).
prefix1(_H,L,L).

p(1).
p(2).
q(1,3).
q(1,4).
q(2,5).

test(X,Y):-
  p(X),
  !,
  q(X,Y).
  
rev([],[]).
rev([H|T],X) :- rev(T,R1),append(R1,[H],X).

findK([H|_],1,H).
findK([H|T],K,X) :- K1 is K - 1, findK(T,K1,X).

last([X],X).
last([H|T],X) :- last(T,X).

elim([],[]).
elim([H|T],[H|T1]) :-
	prefix(H,T,T2),
	elim(T2,T1).
	
prefix(H,[H|T],R) :-
	!,
	prefix(H,T,R).
prefix(_H,L,L).

range(X,X,[X]).
range(X,Y,[X|T]) :-
	X < Y,
	X1 is X+1,
	range(X1,Y,T).
	
on_route(rome).
on_route(Place):-
	move(Place,Method,NewPlace),
	on_route(NewPlace).

move(home,taxi,halifax).
move(halifax,train,gatwick).
move(gatwick,plane,rome).

group([],[]).
group([H|T],[Hs|Ts]) :-
	prefix(H,[H|T],Hs,T1),
	group(T1,Ts).
	
prefix(H,[H|T],[H|T2],T3) :-
	!,
	prefix(H,T,T2,T3).
prefix(_H,L,[],L).

permute([],[]).
permute([X|Xs],Ys) :-
	permute(Xs,Zs),
	del(Ys,X,Zs).

toArray([],[]).
toArray([H|T],[[H]|Y]) :- 
	toArray(T,Y).