
% REQUIRED CODE FOR AUTOGRADER
% DO NOT CHANGE
% :- module(submission, []).
% :- use_module(library(lists), [member/2, append/3]).
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
        nationality(A, X),
        pet(A, hamster),
        anywhere(A, Houses),

        nationality(B, Y),
        drink(B, orangejuice),
        anywhere(B, Houses)
    ],
    [X, Y]
).

hamster_owner(X) :- solver(houses, [X, _]).
orange_juice_drinker(Y) :- solver(houses, [_, Y]).



josephus(Soldiers, N, J, A) :- makelist(Soldiers, 0, List) , loop(List, N, N, J, A).

makelist(Soldiers, N, []) :- Soldiers = N.
makelist(Soldiers, N, List) :- Soldiers > N, N2 is N + 1, append([N2], List2, List), makelist(Soldiers, N2, List2).

loop([F,S|T], 0, N, J, A) :- T == [] -> J is F, A is S;
                                        loop([S|T], N, N, J, A).

loop([H|T], C, N, J, A) :- C > 0, C2 is C - 1, append(T, [H], L), loop(L, C2, N, J, A).


% -*- Mode:Prolog -*-
%  working_directory(_,'/Users/robin/ucalgary/class/449/prolog/').

/*  

The purpose of these notes is to give a very brief introduction to 
definite clause grammars as supported in most prolog systems.
These have been used quite successfully to develop natural langage 
processing applications.

A typical application is to parse English language sentences.  Here 
is a very simple example to give the idea:

*/

sentence --> pronoun(subject), verb_phrase.
sentence --> proper_noun(subject),verb_phrase.

verb_phrase --> verb, pronoun(object).
verb_phrase --> verb, noun_phrase(object).

noun_phrase(object) --> adjective,noun(object).
noun_phrase(object) --> noun(object).

proper_noun(subject) --> [mary].
proper_noun(subject) --> [john].

noun(object) --> [cats].
noun(object) --> [dogs].

adjective --> [black].
adjective --> [tan].

pronoun(subject) --> [he].
pronoun(subject) --> [she].
pronoun(object) --> [him].
pronoun(object) --> [her].

verb --> [likes].
verb --> [hates].
verb --> [does,not,mind].