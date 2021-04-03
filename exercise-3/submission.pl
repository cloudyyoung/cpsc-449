
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
mymember2(X, L) :- count(X, L, Z), Z is 2.
count(_, [], 0).
count(X, [X|Ls], Y) :- count(X, Ls, Z), Y is Z + 1, !.
count(X, [_|Ls], Y) :- count(X, Ls, Y).


%%%%%%% Q3 %%%%%%%

% X is a contiguous sublist of Y
substring([], _) :- !.
substring(X,Y) :- myappend(X,_,T), myappend(_,T,Y).


%%%%%%% Q4 %%%%%%%

% O contains all the sublists of L
% This function is reversable.
sublists([], [[]]).
sublists([H|T], L) :-
    sublists(T, L2),
    addlists(H, L2, L3),
    myappend(L2, L3, L), !.

addlists(_, [], []).
addlists(X, [H|T], [[X|H]|S]) :- addlists(X,T,S).


%%%%%%% Q5 %%%%%%%
% X and Y are permutations of each other.
% mypermutation(X, Y).
mypermutation([], []).
mypermutation([T|H], X) :- mypermutation(H, H1), myappend(L1, L2, H1), myappend(L1, [T], X1), myappend(X1, L2, X).


%%%%%%% Q6 %%%%%%%

% Note, the daughter and son predicates are assumed to be in this form.
%
% daughter(Mother, Father, Daughter)
% son(Mother, Father, Son)
%
daughter("Doreen","Leonard","Nina").
daughter("Doreen","Leonard","Rene").
daughter("Melba","Leonard","Doreen").
daughter("Cecilia","Timothy","Ruth").
son("Kathleen","Marvin","Hogan").
son("Doreen","Leonard","Pam").
son("Melba","Leonard","Charles").
son("Cecilia","Timothy","Marvin").
son("Cecilia","Timothy","Leonard").

% is Grandfather a grandfather of Child
% grandfather(Grandfather, Grandchild).
grandfather(X, Y) :-
    (son(_, X, P); daughter(_, X, P)),
    (son(P, _, Y); daughter(P, _, Y); son(_, P, Y); daughter(_, P, Y)).

% is Grandmother a grandmother of Child
% grandmother(Grandmother, Grandchild).
grandmother(X, Y) :-
    (son(X, _, P);daughter(X, _, P)),
    (son(P, _, Y);daughter(P, _, Y);son(_, P, Y);daughter(_, P, Y)).

% is Brother a brother of Child
brother(X, Y) :-
    (son(_, M, X), (son(_, M, Y); daughter(_, M, Y)); son(F, _, X), (son(F, _, Y); daughter(F, _, Y))),
    X \= Y.

% is Sister a sister of Child
sister(X, Y) :-
    (daughter(_, M, X), (son(_, M, Y); daughter(_, M, Y)); daughter(F, _, X), (son(F, _, Y); daughter(F, _, Y))),
    X \= Y.

% Sibling a sibling of child
sibling(X, Y) :-
    (son(D, M, X); daughter(D, M, X)),
    (son(D, M, Y); daughter(D, M, Y)),
    X \= Y.

% Is A a cousin of B?
% I.e. (Is A a child of a sibling of B).
cousin(X, Y) :-
    grandfather(F, X), 
    grandfather(F, Y),
    grandmother(M, X), 
    grandmother(M, Y),
    father(F1, X),
    father(F2, Y),
    mother(M1, X),
    mother(M2, Y),
    F1 \= F2,
    M1 \= M2.

father(X, Y) :- son(_, X, Y); daughter(_, X, Y).
mother(X, Y) :- son(X, _, Y); daughter(X, _, Y).


%%%%%%% Q7 %%%%%%%

% Note, an edge is defined as 
%   edge(X, Y).
% which is a unidirectional edge from X to Y
edge(a,b).
edge(a,c).
edge(a,d).
edge(e,a).
edge(c,a).

% Does there exists a path between X and Y
path(X, X).
path(X, Y) :- edge(X,Y); edge(X, Z), edge(Z, Y).

% What is the length L of the shortest path from X to Y
% shortpath(X, Y, L)


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

structure(houses,([house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_)])).

nationality((Nationality, _, _, _, _), Nationality).
color((_, Color, _, _, _), Color).
sport((_, _, Sport, _, _), Sport).
pet((_,_, _, Pet, _), Pet).
drink((_, _, _, _, Drink), Drink).

nextto(A, B, [A, B, _, _, _]).
nextto(A, B, [_, A, B, _, _]).
nextto(A, B, [_, _, A, B, _]).
nextto(A, B, [_, _, _, A, B]).
nextto(A, B, H) :- rightof(A, B, H).

rightof(A, B, [B, A, _, _, _]).
rightof(A, B, [_, B, A, _, _]).
rightof(A, B, [_, _, B, A, _]).
rightof(A, B, [_, _, _, B, A]).

anywhere(X, H) :- mymember(X, H).

% The Irishman lives in the first house on the left
hint1([X, _, _, _, _]) :- nationality(X, irishman).

% The man who plays baseball lives in the house next to the man who keeps a tiger
hint2(H) :- sport(X, baseball), nextto(X, Y, H), pet(Y, tiger).

% The occupant of the house, next to the house where the owner rides a horse, plays soccer.
hint3(H) :- sport(X, soccer), pet(Y, horse), nextto(X, Y, H).

% The squash player drinks gin
hint4(H) :- sport(X, squash), drink(X, gin), anywhere(X, H).

% The Frenchman plays rugger
hint5(H) :- nationality(X, frenchman), sport(X, rugger), anywhere(X, H).

% The Irishman lives next to the blue house
hint6(H) :- nationality(X, irishman), nextto(X, Y, H), color(Y, blue).

% The Englishman lives in the red house
hint7(H) :- nationality(X, englishman), color(X, red), anywhere(X, H).

% The Spaniard is often seen taking his dog for a walk
hint8(H) :- nationality(spaniard), pet(X, dog), anywhere(X, H).

% Beer is brewed (and drunk in large quantities) in the green house
hint9(H) :- drink(X, beer), color(X, green), anywhere(X, H).

% The Scotsman drinks whiskey and is often tipsey
hint10(H) :- nationality(X, scotsman), drink(X, whiskey), anywhere(X, H).

% The green house is immediately to the rightof of the white house
hint11(H) :- color(X, green), color(Y, white), rightof(X, Y, H).

% The tennis player owns snakes
hint12(H) :- sport(X, tennis), pet(X, snake), anywhere(X, H).

% Soccer is played in the yellow house
hint13(H) :- sport(X, soccer), color(X, yellow), anywhere(X, H).

% A lot of wine get consumed in the middle house
hint14([_, _, X, _, _]) :- drink(X, wine).

hints(H) :-
    hint1(H), 
    hint2(H), 
    hint3(H), 
    hint4(H), 
    hint5(H), 
    hint6(H), 
    hint7(H), 
    hint8(H), 
    hint9(H), 
    hint10(H), 
    hint11(H), 
    hint12(H), 
    hint13(H),
    hint14(H).

hamster_owner(O) :- hints(houses), pet(X, hamster), nationality(X, O).
orange_juice_drinker(O) :- hints(houses), drink(X, orange_juice), nationality(X, O).

%%%%%%% Q9 %%%%%%%

% NumberofSoldiers: the total number of soldiers including Josephus and his accomplice (>2)
% N: The selected number to count down
% J: Output position for Josephus (<NumberSoldiers)
% A: Output position for the accomplice (<NumberSoldiers)

% josephus(NumberSoldiers, N, J, A).


