parent(john, paul).
parent(john, peter).
parent(peter, steven).
paretnt(peter, mia).

male(john).
male(paul).
male(peter).
male(steven).

female(X) :- 
	(parent(X,_); parent(_,X)),
	\+ male(X).
	
grandparent(X,Y) :-
	parent(X,Z),
	parent(Z,Y).
	
grandmother(X,Y) :-
	grandparent(X,Y),
	female(X).
	
grandchildren(X,Y) :-
	findall(Z, grandparent(X,Z), Y).