rev(L, X) :- rev(L, [], X).

rev([], L, L).
rev([H|T], T1, X) :- rev(T,[H|T1], X).

len([],0).
len([H|T], A) :- len(T,L), A is 1+L.

last([H], H).
last([_|T], L) :- last(T,L).

lastbutone([H,_], H).
lastbutone([H|T], X) :- lastbutone(T,X).

append([],L,L).
append([H|T],L,[H|Ts]) :- append(T, L, Ts).

min([H|T],V) :- minHelper(T, H, V).

minHelper([], V, V).
minHelper([H|T], C, V) :- 
	C > H -> minHelper(T, H, V);minHelper(T, C, V).
	
min1([H|T],V,X) :- minHelper1(T, H, V, X1, X, 0).

minHelper1([], V, V, X, X, _).
minHelper1([H|T], C, V, X1, X, I) :- I1 is I + 1,
	(C > H -> minHelper1(T, H, V, I1, X, I1);minHelper1(T, C, V, X1, X, I1)).
	
raining(ny).
wet(X) :- raining(X).

del([], K, I, []).
del([H|T], K, I, [H1|T1]) :- I1 is I + 1, (K = I -> del(T, K, I1, [H1|T1]); H1 is H, del(T, K, I1, T1)).

power(N,L,X) :- K = 1, power(N, L, K, K, X).

power(N, L, L, X, X).
power(N, L, K, S, X) :- 
	N1 is S * N,
	K1 is K + 1,
	power(N, L, K1, N1, X).

findkth(L, K, V) :- findkthHelper(L, K, 0, V).

findkthHelper([H|_], K, K, H).
findkthHelper([H|T], K, I, V) :- I1 is I+1, findkthHelper(T, K, I1, V).

selectsort([], []).
selectsort(L, [H|T]) :- min1(L, H, I), del(L, I, 0, L1), selectsort(L1, T).

selectionsort([],[]).
selectionsort([First|Rest], [Smallest|SortedList]) :-
	smallest(Rest, First, Smallest),
	remove([First|Rest], Smallest, NewList),
	selectionsort(NewList, SortedList).

/* looks for the smallest element in the list
   atom A is the current smallest 
*/
smallest([], Smallest,Smallest).
smallest([First|Rest], CurrSmallest, Smallest) :- 
	First < CurrSmallest, smallest(Rest, First, Smallest).
smallest([_|Rest], CurrSmallest, Smallest) :-
	smallest(Rest, CurrSmallest, Smallest).

/* remove the first occurance of atom A from L */
remove([], _, []).
remove([First|Rest], First, Rest).
remove([First|Rest], Element, [First|NewList]) :- 
		remove(Rest, Element, NewList).

reverse(L, X) :-
	reverse(L, [], X).
	
reverse([],L,L).
reverse([X|Xs], Ys, Zs) :-
 reverse(Xs, [X|Ys], Zs).
 
palindrome(X) :- reverse(X, R), X = R.

flatten(L, X) :- flattenHelper(L, X).

flattenHelper(X,[X]) :- \+ is_list(X).
flattenHelper([],[]).
flattenHelper([H|T], X) :- flattenHelper(H, Y), flattenHelper(T, Z), append(Y, Z, X).

elimcondups([],[]).
elimcondups([H|T],[H|X]) :- elimcondupsHelper(H, T, Z), elimcondups(Z,X).

elimcondupsHelper(H,[H|T],X) :-
	!,
	elimcondupsHelper(H, T, X).
elimcondupsHelper(_H,T,T).

pack([],[]).
pack([H|T], [Hs|Ts]) :- packHelper(H, [H|T], Hs, L), pack(L, Ts).

packHelper(H,[H|T], [H|X], Z) :-
	!,
	packHelper(H, T, X, Z).
packHelper(_H, T, [], T).

encode([],[]).
encode([H|T], [Hs|Ts]) :- encodeHelper(H, [H|T], 0, L, Hs), encode(L, Ts).

encodeHelper(H,[H|T], I, Z, X) :-
	!,
	I1 is I+1,
	encodeHelper(H, T, I1, Z, X).
encodeHelper(H, T, I, T, [I,H]).

decode([],[]).
decode([H|T], [Hs|Ts]) :- decodeHelper(H, 0, Hs), decode(T, Ts).

decodeHelper([H1,H2], I, [H1|Hs]) :-
	!,
	I < H2,
	I1 is I + 1,
	decodeHelper([H1,H2], I1, Hs). 
	
slice(L, A, B, X) :- sliceHelper(L, A, Xs), D1 is B-A, D is D1+1, sliceHelper1(Xs, D, X).

sliceHelper(L, 0, L).
sliceHelper([_|T], I, X) :-
	I1 is I - 1,
	sliceHelper(T, I1, X).
	
sliceHelper1(L, 0, []).
sliceHelper1([H|T], I, [H|X]) :-
	I1 is I - 1,
	sliceHelper1(T, I1, X).
	
range(A,A,[A]).
range(A,B,[A|X]) :- I is A + 1, range(I, B, X).

powerset([], []).
powerset([H|T], P) :- powerset(T,P).
powerset([H|T], [H|P]) :- powerset(T,P).

istree(nil).
istree(t(_,L,R)) :- istree(L), istree(R).

elementAt([H|_], K, K, H).
elementAt([H|T], K, I, X) :- I1 is I+1, elementAt(T, K, I1, X).

writeN(N,S) :- 
	N > 0 -> N1 is N - 1, write(S), writeN(N1, S); !.

triangle(N) :- 
	S = 1,
	triangle(N, S).
	
triangle(S, C) :-
	writeN(S, ' '),
	writeN(C, '*'),
	nl,
	S1 is S-1,
	S > 0,
	C1 is C+2,
	triangle(S1, C1).
	
triangle1(N, S) :-
	S > 0,
	S1 is S-1,
	C is N - S1,
	writeN(S1, ' '),
	writeN(C, '*'),
	nl,
	triangle1(N, S1).
	
delete([H|T],H,T).
delete([H|T],K,[H|Ts]) :- delete(T,K,Ts).

per(L,X) :- findall(Z,permute(L,Z),X).

permute([],[]).
permute([H|T],X) :-
	permute(T,Z),
	del(X,H,Z).
	
tree1(t(6, t(4, t(2, nil, nil), t(5, nil, nil)), t(9, t(7, nil, nil), nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))).


inorder(t(K,L,R), List):-
	inorder(L,LL), 
	inorder(R, LR),
	append(LL, [K|LR],List).
	
inorder(nil, []).

preorder(t(K,L,R), List) :-
	preorder(L, NL),
	preorder(R, NR),
	append([K|NL], NR, List).
preorder(nil, []).

edge(1,2,1).
edge(1,4,3.5).
edge(1,3,2.5).
edge(2,3,1).
edge(2,5,2.5).
edge(3,4,1).
edge(3,5,2.2).
edge(4,5,1).

connected(X,Y,L) :- edge(X,Y,L) ; edge(Y,X,L).