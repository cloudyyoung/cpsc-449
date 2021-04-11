strange(S,T):- suffix(M,T),prefix(S,M).
strange([],_).

suffix(S,S).
suffix(S,[_|T]):-suffix(S,T).

prefix([H|S],[H|T]):- prefix(S,T).
prefix([H],[H|_]).


mystery(X,P,L):- laydown(X,P),!,collectup(L).

laydown(X,P):- P, assert(found(X)), false.
laydown(_,_).

collectup([H|L]):-retract(found(H)),!,collectup(L).
collectup([]).