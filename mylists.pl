member(X, [X|_]).
member(X, [_|T]) :- 
				member(X,T).
				
append([], L, L).
append([H|T], L, [H|R]) :-
append(T, L, R).

mylast(X,[X]).
mylast(X,[_,X2|T]) :-
		mylast(	X,[X2|T]).

mlast(X,[X|[]]).
mlast(X,[_|T]) :-
		mlast(X, T).
		
insert(X,L,[X|L]).
insert(X,[H|T], [H|R]):-
 insert(X,T,R).
		
% kth(N, L, X).
kth(1, [X|_], X).
kth(N, [_|T], X) :-
number(N),
N>1,
N2 is N-1,
kth(N2, T, X).

% reverse(L, RL).
reverse([],[]).
reverse([H|T], R):-
reverse(T, RT),
append(RT, [H], R).

even(X) :- X mod 2 =:= 0.
 
%star(C, X) :- C < X, count(0, C), C1 is C+1, star(C1, X).
%star(C, X) :- C >= X.

count(X, Y) :- X =< Y, write('*'), X1 is X+1, count(X1,Y).
count(X, Y) :- X > Y, nl.
 
 lace(N) :-
 N mod 2 =:= 0 -> writeln('*').
 
 len([],0).
 len([_|Xs], M) :- 
	len(Xs, N),
	M is N+1.

 for(I,I,_).
 for(I,X,N):-
	 X<N,
	 X1 is X+1,
	 for(I, X1, N).
	 
 for2(1,1,_).
 for2(I,1,N) :-
	for2(I1, 1, N),
	I1 < N,
	I is I1+1.
	
delete([X|Ys], X, Ys).
delete([Y|Ys], X, [Y|Zs]) :-
delete(Ys, X, Zs).

preorder(Root, [Root]) :- leaf(Root).
preorder(Root, [Root|L]) :-
node(Root, Child1, Child2),
preorder(Child1, L1),
preorder(Child2, L2),
append(L1, L2, L).

inorder(Root, [Root]) :- leaf(Root).
inorder(Root, L) :-
node(Root, Child1, Child2),
inorder(Child1, L1),
inorder(Child2, L2),
append(L1, [Root|L2], L).

postorder(Root, [Root]) :- leaf(Root).
postorder(Root, [Root|L]) :-
node(Root, Child1, Child2),
postorder(Child1, L1),
postorder(Child2, L2),
append(L1, L2, L).


%homework(N) :- (N mod 2 =:= 0 -> printeven(N, N); printodd(N, N)).
%printodd(N, 0) :- write('***').
%printodd(N, X) :- L is X/2, L =:= X, countspace(N, X).
%printodd(N, X) :- N >= X, X > 0, A is 2* (N - 1), A1 is A+1, count(0, A1), X1 is X-1, printodd(N, X1).
%printeven(N, 0) :- write('***').
%printeven(N, X) :- L is X/2, L =:= X, countspace(N, X).
%printeven(N, X) :- N >= X, X > 0, A is 2*N - 1, A1 is A+1, count(0, A1), X1 is X-1, printodd(N, X1).


star(C, X) :- C < X, count(C, 5).
star(C, X) :- C >= X.

count(X, Y) :- X =< Y, write('*'), X1 is X+1, count(X1,Y).
count(X, Y) :- X > Y, nl.

count1(X, Y) :- N1 is Y/2, X =:= N1, write(' ').
count1(X, Y) :- X =< Y, write('*'), X1 is X+1, count(X1,Y).
count1(X, Y) :- X > Y, nl.

%stars(N, I) :- I =< N -> write('*'), I1 is I+1, stars(I1, N); nl.

%countspace(X, Y) :- Z is X/2, (Y =:= Z -> write(' '); write('*')), X1 is X+1, countspace(X1,Y).
%countspace(X, Y) :- X > Y, nl.


stars(Y, X) :- X =< Y, write('*'), X1 is X+1, stars(X1,Y).
stars(Y, X) :- X > Y, nl.
stars_middle(N, I) :- N1 is N/2, I =:= N1 -> write(' '), I2 is I+1, stars_middle(N,I2); I =< N -> write('*'), I1 is I+1, stars(I1, N); nl.

%tartan(N) :-  (N mod 2 =:= 0 -> printeven(N); printodd(N)).
lace(N) :- N =:= 1 -> write('*'); printodd(N).
printodd(N) :- X1 is N-1, N1 is (2*N), N2 is N1-1, printodd_upper(X1, X1, N2), printodd_middle(N2), printodd_lower(X1, X1, N2).
printodd_upper(N, X, Y) :- X > 0, star(1, Y), X1 is X-1, printodd_upper(N, X1, Y).
printodd_upper(N, X, Y) :- X =< 0, nl.
printodd_middle(N) :- count1(1, N).
printodd_lower(N, X, Y) :- X > 0 -> stars(1, Y), X1 is X-1, printodd_lower(N, X1, Y); nl.
printodd_lower(N, X, Y) :- X =< 0, nl.