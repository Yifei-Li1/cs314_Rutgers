/* YOUR CODE HERE (Prolem 1, delete the following line) */
range(X,Q,X).
range(P,X,X).
range(P,Q,X):- X>=P,X=<Q.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Prolem 2, delete the following line) */
reverseL([],[]).
reverseL([H|T],R):-
	reverseL(T,Temp),append(Temp,[H],R).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Prolem 3, delete the following line) */
memberL(X,[H|T]):-
	X = H; memberL(X,T).


?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Prolem 4, delete the following line) */
zip([], [], []).
zip([], [_|_], []).
zip([_|_], [], []).
zip([X|Xs], [Y|Ys], [X-Y|XYs]) :-
   zip(Xs, Ys, XYs).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Prolem 5, delete the following line) */
insert(X, [], [X]). 
insert(X, [Y | Rest], [X,Y | Rest]) :- X @< Y, !.
insert(X, [Y | Rest1], [Y | Rest2]) :- insert(X, Rest1, Rest2).
?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Prolem 6, delete the following line) */
shanchu(_,[],[]).
shanchu(Y,[Y],[]).
shanchu(X,[X|List1],List1).
shanchu(X,[Y|L],[Y|List1]):-
	shanchu(X,L,List1),!.
connect(A,T,T):- member(A,T),!.
connect(A,Tail,[A|Tail]).
remove_duplicates([], []).
remove_duplicates([X],[X]).
remove_duplicates([X1,X2|T],Z):-
	shanchu(X1,[X2|T],Y2),
	remove_duplicates(Y2,Z1),
	connect(X1,Z1,Z).

	

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Prolem 7, delete the following line) */
intersectionL([], _, []).
intersectionL([H|L1tail], L2, L3) :-
        memberchk(H, L2),
        !,
        L3 = [H|L3tail],
        intersectionL(L1tail, L2, L3tail).
intersectionL([_|L1tail], L2, L3) :-
        intersectionL(L1tail, L2, L3).
?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

/* YOUR CODE HERE (Prolem 8, delete the following line) */
prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).
partition（[X]，[X]，[]).
partition(L, P, S):-length(L, N),PL is div(N,2),length(P, PL),append(P, S, L).


?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Prolem 9, delete the following line) */
merge([],Bs,Bs):-!.
merge(As,[],As).
merge([A|As],[B|Bs],[A,B|Rs]):-
	!,merge(As,Bs,Rs).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Prolem 10, delete the following line) */

mergesort([],[]).
mergesort([A],[A]).
mergesort([A,B|R],S):-
	cut([A,B|R],L1,L2),
	mergesort(L1,S1),
	mergesort(L2,S2),
	merges(S1,S2,S).
cut([],[],[]).
cut([A],[A],[]).
cut([A,B|R],[A|Ra],[B|Rb]):-
	cut(R,Ra,Rb).
merges(A,[],A).
merges([],B,B).
merges([A|Ra],[B|Rb],[A|M]):-
	A=<B, merges(Ra,[B|Rb],M).
merges([A|Ra],[B|Rb],[B|M]):-
	A>B,merges([A|Ra],Rb,M).


?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
