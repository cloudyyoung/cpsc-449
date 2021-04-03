/* Hand evaluation
    Do not forget to label each of your cases/options as label each of the body options.
    Do not forget to generate new type variables during each step, this will prevent mistakes from being made.
    Remember that prolog evaluates:
        cases top down,
        body arguments left to right.
 */


% is X a member of list Y
myMember(X, [X | _]).                           % (1)
myMember(X, [_ | Rest]) :- myMember(X, Rest).   % (2) :- (2.1)

/*
goal: myMember(2, [1,2]).
    unify (1) myMember(X1, [X1 | _])                        does not unify! cannot unify (1/2), backtrack
    unify (2) myMember(X2, [_ | Rest2])                     success { 2/X2, [2]/Rest }
        body (2.1) member(X2, Rest2) { 2/X2, [2] / Rest }
            = member(2, [2]).
                unify (1) member(X3, [X3 | _])              success { 2/X3 }
                    body empty, true                        ; (force backtracking)
                unify (2) member(X4, [_ | Rest4)            success { 2/X4, []/Rest4 }
                    body (2.1) member(X4, Rest4)  { 2/X4, []/Rest4 }
                        = member(2, [])
                            unify (1) member(X5, [X5 | _]) ~~ member(2, []) does not unify! (fails []/[X5 | _])
                            unify (2) member(X6, [_ | Rest6])  does not unify! 
                            false.
*/

/*
 { myMember(X, [1])/myMember(X1, [X1 | _]) }. decompose
 { X/X1, [1]/[ X1 | _Tmp ] } decompose
 { X/X1, 1/X1 []/_Tmp } eliminate X1
 { X/1, 1/X1 []/_Tmp } swap X/1
 { 1/X, 1/X1 []/_Tmp } 
*/


/*
 Remember substitutions do no carry over between cases.


goal: myMember(X, [1])
    unify (1) myMember(X1, [X1 | _])                        success! { 1/X, 1/X1 }
        body empty: X = 1                                   ; (force backtracking)
    unify (2) myMember(X2, [_ | Rest2]) ~~ myMember(X, [1]) success! { X/X2 , []/Rest2 }
        body (2.1) myMember(X2, Rest2) { X/X2, []/Rest2}
            = myMember(X2, []).
                .
                . (same as above case basically)
                .
                false.
*/


myNot(X) :- X, !, false.    % (3) :- (3.1), (3.2), (3.3)
myNot(_).                   % (4)

/*
goal: myNot(member(X, [1]))
    unify (3) myNot(X1)  success { myMember(X, [1])/X1 }
        body (3.1) X1 { member(X, [1]) / X1} 
            = myMember(X, [1])
                unify (1) myMember(X2, [X2 | _])   success! { 1/X, 1/X1 }
                    body empty: X = 1 (true)
        body (3.2) ! 
            true.
        body (3.3).
            (can't backtrack back past the !)
            false.
*/


% =../2
% write()
% nl
 
% a(b, c) =.. [a, b, c]
% edge(1,2) =.. [edge, 1, 2]
% X =.. [edge, 1, 2] { X = edge(1,2) }

addOne(X, O) :- O is X + 1.

% assumign Pred/2
maplist(_   , [], []).
maplist(Pred, [X | Rest], O) :-
    Pred =.. Args,                      % (4.1)
    append(Args, [X, E], NewArgs),      % (4.2)
    CallPred =.. NewArgs,               % (4.3)
    CallPred,
    maplist(Pred, Rest, O1),
    O = [ E | O1 ].

% addOne =.. [addOne] (4.1)
% append([addOne], [X, E], [addOne, X, E]) (4.2)
% CallPred =.. [addOne, X, E]
%       CallPred = addOne(X, E)
% CallPred = addOne(X, E) 
%   X = 1, E = 2
%   map the rest of the list
% O = [ 2 | Rest ]

example_write() :-
    write("Hello, World!"),
    false,
    write(a(b,c)),
    nl.                     % basically the same as write("\n").
example_write() :-
    write("lol").
    

