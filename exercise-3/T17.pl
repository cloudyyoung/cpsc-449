% get to console using
%   swipl Example.pl
% once in console can reload using 
%   make.
% do not forget the period


edge(a, b).
edge(b, c).
edge(c, d).
edge(c, e).
edge(c, l1).
edge(l1, l2).
edge(l2, l1).

connected(Y, Y).
connected(X, Y) :- edge(X, Z), connected(Z, Y).

% note these are equivalent
member1(X, [Head | Rest]) :- X = Head ; member(X, Rest).

member2(X, [X | _]).
member2(X, [_ | Rest]) :- member(X, Rest).

connected2(X, X, _).
connected2(X, Y, Visited) :- 
        edge(X, Z),
        not(member(Z, Visited)), 
        Visited2 = [Z | Visited], 
        connected2(Z, Y, Visited2).

% How terms should be thought about
% 15 + 2 = add(15, 2)

% Does the following unify?
% 15 + 2 = 17
% This does not unify, as add(15, 2) != 17

addTwo(X, Output) :- Output is X + 2.
addTwoPrime(X, Output) :- Output = X + 2.

equalArithmetic(X, Y) :- X is Y.
equalUnify(X, Y) :- X = Y.
equalSyntactic(X, Y) :- X == Y.

% one more type of equality in prolog
%   ==

% a(X, b) = a(b, X)  -> will return true, as [b/X] 
% a(X, b) == a(b, X)  -> will return false
