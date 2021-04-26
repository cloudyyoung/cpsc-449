met(john,28,mary).
met(john,3,mary).
met(peter,21,john).
met(mary,23,mark).
met(anne,19,peter).
met(anne,29,john).
met(anne,24,mary).
met(mark,17,paul).
met(hey, 10, hi).
met(hi, 7, good).
met(good, 3, oh).
met(oh, 2, my).
met(my, 1, gosh).

trace(infect(StartPerson,StartTime),infect(GoalPerson,GoalTime), [infect(StartPerson,StartTime), infect(GoalPerson,X)]) :- 
    met(StartPerson, X, GoalPerson), 
    StartTime >= X,
    X >= GoalTime,
    7 >= (StartTime - X).

trace(infect(StartPerson,StartTime),infect(GoalPerson,GoalTime), InfectionPathway) :-
    StartTimeWithin = StartTime - 7,
    GoalTimeWithin = GoalTime + 7,
    trace(infect(StartPerson, StartTime), infect(MiddlePerson, StartTimeWithin), A),
    trace(infect(MiddlePerson, GoalTimeWithin), infect(GoalPerson, GoalTime), [infect(MiddlePerson, GoalTimeWithin)|B]),
    append(A, B, InfectionPathway).


% trace(infect(hey, 11), infect(hi, 10), X).
% trace(infect(hey, 11), infect(oh, 2), X).
% trace(infect(hey, 11), infect(gosh, 1), X).
% trace(infect(john, 30), infect(gosh, 1), X).