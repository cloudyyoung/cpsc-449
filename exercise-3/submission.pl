
% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
:- module(submission, []).
:- use_module(library(lists), []).
% Begin Assignment Code

%%%%%%% Q1 %%%%%%%

% X concatenated with Y results in Z
myappend([], Y, Y).
myappend([X|Xs], Ys, [X|Zs]) :- myappend(Xs, Ys, Zs).

% Y is the reverse of X
myreverse([], []).
myreverse([X|Xs], Y) :- 
    myreverse(Xs, T), 
    myappend(T, [X], Y).


% Y is the flattening of X
islist([]).
islist([_|Xs]) :- islist(Xs).

myflatten([], []).
myflatten([X|Xs], Ys) :- islist(X), myflatten(Xs, Zs), myappend(X, Zs, Ys), !.
myflatten([X|Xs], [X|Ys]) :- myflatten(Xs, Ys).


% X is a member of Y
mymember(X,[X|_]).
mymember(X,[_|Ys]) :- mymember(X,Ys).

% Z is list obtained from remove X from Y
myremove(_, [], []).
myremove(X, [X|Ys], Zs) :- myremove(X, Ys, Zs), !.
myremove(X, [Y|Ys], [Y|Zs]) :- myremove(X, Ys, Zs).

%%%%%%% Q2 %%%%%%%

% X occurs precisely two times in L
% mymember2(X, L).


%%%%%%% Q3 %%%%%%%

% X is a contiguous sublist of Y
% substring(X, Y).

%%%%%%% Q4 %%%%%%%

% O contains all the sublists of L
% sublists(L, O).

%%%%%%% Q5 %%%%%%%

% X and Y are permutations of each other.
% mypermutation(X, Y).


%%%%%%% Q6 %%%%%%%

% Note, the daughter and son predicates are assumed to be in this form.
%
% daughter(Mother, Father, Daughter)
% son(Mother, Father, Son)
%


% is Grandfather a grandfather of Child
% grandfather(Grandfather, Grandchild).

% is Grandmother a grandmother of Child
% grandmother(Grandmother, Grandchild).


% is Brother a brother of Child
% brother(Brother, Child).

% is Sister a sister of Child
% sister(Sister, Child).

% Sibling a sibling of child
% sibling(Sibling, Child).

% Is A a cousin of B?
% I.e. (Is A a child of a sibling of B).
% cousin(A, B).



%%%%%%% Q7 %%%%%%%

% Note, an edge is defined as 
%   edge(X, Y).
% which is a unidirectional edge from X to Y

% Does there exists a path between X and Y
% path(X, Y).

% What is the length L of the shortest path from X to Y
% shortpath(X, Y, L).


%%%%%%% Q8 %%%%%%%

% use the following terms when solving the problem, so the autograder can roperly check if the answer is correct or not.
blue.
green.
red.
white.
yellow.

englishman.
frenchman.
irishman.
scotsman.
spaniard.

dog.
hamster.
horse.
snake.
tiger.

baseball.
squash.
rugger.
soccer.
tennis.

beer.
gin.
orange_juice.
whiskey.
wine.

% Implement and define the following two predicates, where O is the output.
% hamster_owner(O).
% orange_juice_drinker(O).

%%%%%%% Q9 %%%%%%%

% NumberofSoldiers: the total number of soldiers including Josephus and his accomplice (>2)
% N: The selected number to count down
% J: Output position for Josephus (<NumberSoldiers)
% A: Output position for the accomplice (<NumberSoldiers)

% josephus(NumberSoldiers, N, J, A).

