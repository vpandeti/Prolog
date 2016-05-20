r(2).
s(2).
l(0).
t(0).

r_tetramine(X) :-
	r(X).
s_tetramine(X) :-
	s(X).
l_tetramine(X) :-
	l(X).
t_tetramine(X) :-
	t(X).

check(X) :- 
	r_tetramine(Q1),
	s_tetramine(Q2), 
	t_tetramine(Q3),
	l_tetramine(Q4),
	R1 is Q1 + Q2,
	R2 is R1 / Q3,
	R3 is R2+ + Q4,
	R3 > R1 -> write('Yes') , nl, nl; write('No').