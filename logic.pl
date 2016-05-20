and(A,B) :- A, B.

or(A,_) :- A.
or(_,B) :- B.

equ(A,B) :- or(and(A,B), and(not(A),not(B))).

xor(A,B) :- not(equ(A,B)).

nor(A,B) :- not(or(A,B)).

nand(A,B) :- not(and(A,B)).

impl(A,B) :- or(not(A),B).

% bind(X) :- instantiate X to be true and false successively

bind(true).
bind(fail).	

eval(neg(X),X,1).

myClause(A,Expr) :- bind(A), do(A,Expr), fail.

do(A,_) :- write(A), write('  '), fail.
do(_,Expr) :- Expr, !, write(true), nl.
do(_,_) :- write(fail), nl.