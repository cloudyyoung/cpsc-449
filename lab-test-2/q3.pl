
leaves(X, [X]) :- singleton(X).
leaves(X, R) :- arguments(X, Y), foreach(Y, R).

singleton(X) :- X =.. [X].
arguments(X, Ys) :- X =.. [_|Ys].
foreach([], []).
foreach([X|Xs], R) :- leaves(X, Y), foreach(Xs, Z), append(Y, Z, R).
