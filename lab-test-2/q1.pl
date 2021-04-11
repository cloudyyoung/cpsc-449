frev(X,Y):- 
    shunt(X,[],Y).

shunt([],X,X).
shunt([H|T],S,Z):-
    shunt(T,[H|S],Z).

frev2([], []).
frev2([X|Xs], Zs) :- frev2(Xs, Ys), append(Ys, [X], Zs).