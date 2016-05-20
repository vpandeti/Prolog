member([H|T],X) :- H = X; member(T,X).

append([],L,L).
append([H|T], L, [H|Ts]) :-
	append(T,L,Ts).
	
p(X) :- q(X), not(r(X)).
r(X) :- w(X), not(s(X)).
q(a).
q(b).
q(c).
s(a) :- p(a).
s(c).
w(a).
w(b).

reach(X,Y) :- 
	edge(X,Y).

reach(X,Y) :- 
	reach(X,Z),
	edge(Z,Y).

edge(a,a).
edge(a,b).
edge(b,c).
