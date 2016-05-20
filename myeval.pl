maxlist([X],X).
maxlist([X|Xs],X):- maxlist(Xs,Y), X >=Y.
maxlist([X|Xs],N):- maxlist(Xs,N), N > X.

eval_exp(X,A,B,M) :- X == mult -> M is A*B; X == plus -> M is A+B; M is A-B.

eval1(tree(tree(A,B),tree(C,D)), [X,Y,Z], M) :- eval_exp(X,A,B,X1), eval_exp(Y,C,D,X2),	eval_exp(Z,X1,X2,M).

maxval(tree(tree(A,B),tree(C,D)), [X, Y, Z], Max) :-
	eval1(tree(tree(A,B),tree(C,D)), [X, Y, Z], M1),
	eval1(tree(tree(A,B),tree(C,D)), [X, Z, Y], M2),
	eval1(tree(tree(A,B),tree(C,D)), [Y, X, Z], M3),
	eval1(tree(tree(A,B),tree(C,D)), [Z, X, Y], M4),
	eval1(tree(tree(A,B),tree(C,D)), [Y, Z, X], M5),
	eval1(tree(tree(A,B),tree(C,D)), [Z, Y, X], M6),
	maxlist([M1,M2,M3,M4,M5,M6],Max).