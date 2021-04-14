
% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
:- module(submission, []).
:- use_module(library(lists), [member/2, once/1]).
% Begin Assignment Code

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

solver(Name,Solution):- get_puzzle(Name,P),solve_puzzle(P,Solution).

solve_puzzle(puzzle(Clues,Queries,Solution),Solution):-
     solve(Clues),
     solve(Queries).

solve([Clue|Clues]):- Clue, solve(Clues).
solve([]).

get_puzzle(Name,puzzle(Clues,Queries,Solution)):-
    structure(Name,Structure),
    clues(Name,Structure,Clues),
    queries(Name,Structure,Queries,Solution).

structure(houses,([house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_),house(_,_,_,_,_)])).


clues(houses,Houses,
    [
      (nationality(A, irishman), left(A, Houses)),
      (sport(B, baseball), nextto(B, C, Houses), pet(C, tiger)),
      (sport(D, soccer), pet(E, horse), nextto(D, E, Houses)),
      (drink(F, gin), sport(F, squash), anywhere(F, Houses)),
      (nationality(G, frenchman), sport(G, rugger), anywhere(G, Houses)),
      (nationality(H, irishman), color(I, blue), nextto(H, I, Houses)),
      (nationality(J, englishman), color(J, red), anywhere(J, Houses)),
      (nationality(K, spaniard), pet(K, dog), anywhere(K, Houses)),
      (drink(L, beer), color(L, green), anywhere(L, Houses)),
      (nationality(M, scotsman), drink(M, whiskey), anywhere(M, Houses)),
      (color(N, green), color(O, white), rightof(N, O, Houses)),
      (pet(P, snake), sport(P, tennis), anywhere(P, Houses)),
      (color(Q, yellow), sport(Q, soccer), anywhere(Q, Houses)),
      (drink(R, wine), middle(R, Houses))
    ]
).

nationality(house(Nationality, _, _, _, _), Nationality).
color(house(_, Color, _, _, _), Color).
sport(house(_, _, Sport, _, _), Sport).
pet(house(_,_, _, Pet, _), Pet).
drink(house(_, _, _, _, Drink), Drink).

nextto(A, B, [A, B, _, _, _]).
nextto(A, B, [_, A, B, _, _]).
nextto(A, B, [_, _, A, B, _]).
nextto(A, B, [_, _, _, A, B]).
nextto(A, B, H) :- rightof(A, B, H).

rightof(A, B, [B, A, _, _, _]).
rightof(A, B, [_, B, A, _, _]).
rightof(A, B, [_, _, B, A, _]).
rightof(A, B, [_, _, _, B, A]).

anywhere(X, H) :- member(X, H).

left(X, [X, _, _, _, _]).
middle(X, [_, _, X, _, _]).

queries(houses, Houses,
    [
        member(A, Houses),
        nationality(A, X),
        pet(A, hamster),

        member(B, Houses),
        nationality(B, Y),
        drink(B, orangejuice)
    ],
    [X, Y]
).

hamster_owner(X) :- solver(houses, [X, _]).
orange_juice_drinker(Y) :- solver(houses, [_, Y]).

