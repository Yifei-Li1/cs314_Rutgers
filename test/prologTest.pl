sum([H|T],X) :- sum(T,M), X is H+M.
sum([],0).

lengthhh([],0).
lengthhh([H|T],X):- lengthhh(T,M),X is M+1.
appendd([],Q,Q).
appendd([H | P], Q, [H | R]) :- appendd(P, Q, R).
babab(X,Y,Z).
basdfa(123,sum([],0),Y).