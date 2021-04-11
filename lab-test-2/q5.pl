
myappend([], Y, Y).
myappend([X|Xs], Ys, [X|Zs]) :- myappend(Xs, Ys, Zs).

myrepeat(_, 0, []).
myrepeat(X, Y, [X|Zs]) :- Y2 is Y - 1, myrepeat(X, Y2, Zs).

% grow([1,2,3], X).
grow(Xs, Rs) :- grow_(Xs, 1, Rs), !.
grow_([], _, []) :- !.
grow_([X|Xs], C, Rs) :- myrepeat(X, C, Ys), C2 is C + 1, grow_(Xs, C2, Zs), myappend(Ys, Zs, Rs), !.



