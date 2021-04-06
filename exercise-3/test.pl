
my_append([], Y, Y).
my_append([X|Xs], Ys, [X|Zs]) :- my_append(Xs, Ys, Zs).

remove_all(_, [], []).
remove_all(X, [X|Ys], Zs) :- remove_all(X, Ys, Zs).
remove_all(X, [Y|Ys], [Y|Zs]) :- remove_all(X, Ys, Zs).

nth_element(1, [Y|_], Y).
nth_element(X, [_|Ys], R) :- Z is X - 1, nth_element(Z, Ys, R).
% OR
% nth_element(X, [Y|Ys], R) :- (X == 1 -> R is Y ; Z is X - 1, nth_element(Z, Ys, R)).

my_length([], 0).
my_length([X|Xs], R) :- my_length(Xs, Y), R is Y + 1.

my_reverse([], []).
my_reverse([X|Xs], Zs) :- my_reverse(Xs, Ys), my_append(Ys, [X], Zs).

my_member(X, [X|Ys]).
my_member(X, [_|Ys]) :- my_member(X, Ys).

