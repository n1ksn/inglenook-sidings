%%%------------------------------------------------------------------------
%%% inglenook-sidings-full-dev.prolog
%%%
%%% Find solutions to a classic shunting (switching) puzzle using Prolog.
%%% This set of predicates has been tested using SWI Prolog and gprolog on
%%% Linux and using SWI Prolog on Windows 10.
%%%
%%% Andrew Palm
%%% 2019-11-06
%%%
%%%
%%%------------------------------------------------------------------------
%%%
%%% Predicates to find solutions
%%% ----------------------------
%% Allowed moves
%% The variable E in the move predicate arguments represents the engine.
% Pull or drop three cars
move([[E],[A,B,C|T1],T2,T3], [[E,A,B,C],T1,T2,T3]) :- length(T1,N), N<3.
move([[E],T1,[A,B,C],T3], [[E,A,B,C],T1,[],T3]).
move([[E],T1,T2,[A,B,C]], [[E,A,B,C],T1,T2,[]]).
move([[E,A,B,C],T1,T2,T3], [[E],[A,B,C|T1],T2,T3]) :- length(T1,N), N<3.
move([[E,A,B,C],T1,[],T3], [[E],T1,[A,B,C],T3]).
move([[E,A,B,C],T1,T2,[]], [[E],T1,T2,[A,B,C]]).

% Pull or drop two cars
move([[E],[A,B|T1],T2,T3], [[E,A,B],T1,T2,T3]) :- length(T1,N), N<4.
move([[E],T1,[A,B|T2],T3], [[E,A,B],T1,T2,T3]) :- length(T2,N), N<2.
move([[E],T1,T2,[A,B|T3]], [[E,A,B],T1,T2,T3]) :- length(T3,N), N<2.
move([[E,A,B],T1,T2,T3], [[E],[A,B|T1],T2,T3]) :- length(T1,N), N<4.
move([[E,A,B],T1,T2,T3], [[E],T1,[A,B|T2],T3]) :- length(T2,N), N<2.
move([[E,A,B],T1,T2,T3], [[E],T1,T2,[A,B|T3]]) :- length(T3,N), N<2.

move([[E,A],[B,C|T1],T2,T3], [[E,A,B,C],T1,T2,T3]) :- length(T1,N), N<4.
move([[E,A],T1,[B,C|T2],T3], [[E,A,B,C],T1,T2,T3]) :- length(T2,N), N<2.
move([[E,A],T1,T2,[B,C|T3]], [[E,A,B,C],T1,T2,T3]) :- length(T3,N), N<2.
move([[E,A,B,C],T1,T2,T3], [[E,A],[B,C|T1],T2,T3]) :- length(T1,N), N<4.
move([[E,A,B,C],T1,T2,T3], [[E,A],T1,[B,C|T2],T3]) :- length(T2,N), N<2.
move([[E,A,B,C],T1,T2,T3], [[E,A],T1,T2,[B,C|T3]]) :- length(T3,N), N<2.

% Pull or drop one car
move([[E],[A|T1],T2,T3], [[E,A],T1,T2,T3]) :- length(T1,N), N<5.
move([[E],T1,[A|T2],T3], [[E,A],T1,T2,T3]) :- length(T2,N), N<3.
move([[E],T1,T2,[A|T3]], [[E,A],T1,T2,T3]) :- length(T3,N), N<3.
move([[E,A],T1,T2,T3], [[E],[A|T1],T2,T3]) :- length(T1,N), N<5.
move([[E,A],T1,T2,T3], [[E],T1,[A|T2],T3]) :- length(T2,N), N<3.
move([[E,A],T1,T2,T3], [[E],T1,T2,[A|T3]]) :- length(T3,N), N<3.

move([[E,A],[B|T1],T2,T3], [[E,A,B],T1,T2,T3]) :- length(T1,N), N<5.
move([[E,A],T1,[B|T2],T3], [[E,A,B],T1,T2,T3]) :- length(T2,N), N<3.
move([[E,A],T1,T2,[B|T3]], [[E,A,B],T1,T2,T3]) :- length(T3,N), N<3.
move([[E,A,B],T1,T2,T3], [[E,A],[B|T1],T2,T3]) :- length(T1,N), N<5.
move([[E,A,B],T1,T2,T3], [[E,A],T1,[B|T2],T3]) :- length(T2,N), N<3.
move([[E,A,B],T1,T2,T3], [[E,A],T1,T2,[B|T3]]) :- length(T3,N), N<3.

move([[E,A,B],[C|T1],T2,T3], [[E,A,B,C],T1,T2,T3]) :- length(T1,N), N<5.
move([[E,A,B],T1,[C|T2],T3], [[E,A,B,C],T1,T2,T3]) :- length(T2,N), N<3.
move([[E,A,B],T1,T2,[C|T3]], [[E,A,B,C],T1,T2,T3]) :- length(T3,N), N<3.
move([[E,A,B,C],T1,T2,T3], [[E,A,B],[C|T1],T2,T3]) :- length(T1,N), N<5.
move([[E,A,B,C],T1,T2,T3], [[E,A,B],T1,[C|T2],T3]) :- length(T2,N), N<3.
move([[E,A,B,C],T1,T2,T3], [[E,A,B],T1,T2,[C|T3]]) :- length(T3,N), N<3.

%% path(StartState, LastState, Path) succeeds if Path is a list of
%% states from StartState to LastState (in reverse order) using legal
%% moves
path(State, State, [State]).
path(StartState, LastState, [LastState|Path]) :-
  path(StartState, OneButLast, Path),
  move(OneButLast, LastState),
  \+ member(LastState, Path).

%%% solve(StartState, EndState) succeeds if there is a set of moves
%%% (a path) from StartState to EndState
solve(StartState, EndState) :-
  % If there are only five cars or less (not counting the engine), find a
  % minimal length solution without using an intermediate node
  flatten(StartState, FlatStartState),
  length(FlatStartState, N),
  N < 7,
  path(StartState, EndState, Path), !,
  write_solution(Path).
solve(StartState, EndState) :-
  % If the last three cars on track 1 are the same in the start and
  % end states, find a minimal length solution without using an
  % intermediate node
  [_, StartTrk1, _, _] = StartState,
  [_, EndTrk1, _, _] = EndState,
  last_n(3, StartTrk1, StartTrk1Last3, _),
  last_n(3, EndTrk1, EndTrk1Last3, _),
  EndTrk1Last3 = StartTrk1Last3,
  path(StartState, EndState, Path), !,
  write_solution(Path).
solve(StartState, EndState) :-
  % Find both forward and reverse solutions using an intermediate
  % node and set Path as the shorter of the two (or the forward
  % solution if solution lengths are equal)
  solve_fwd(StartState, EndState, FwdPath), !,
  length(FwdPath, FwdM),
  % Handle case where track(s) in EndState are originally indefinate
  FwdPath = [DefEndState|_],
  solve_rev(StartState, DefEndState, RevPath), !,
  length(RevPath, RevM),
  ( FwdM > RevM -> Path = RevPath; Path = FwdPath ),
  write_solution(Path).

%% solve_fwd(StartState, EndState, Path) succeeds if there is a path of
%% moves from StartState to EndState by Path and which goes through an
%% intermediate state [_,EndTrk1Last,_,_], where EndTrk1Last is the last 3
%% (or fewer, if necessary) cars on track 1 in the StartState.  The
%% solution steps in Path are in reverse order.
solve_fwd(StartState, EndState, Path) :-
  % Construct intermediate goal state, a partial solution for track 1 only
  [_, EndTrk1, _, _] = EndState,
  (last_n(3, EndTrk1, EndTrk1Last, _); EndTrk1Last = EndTrk1),
  IntState = [_, EndTrk1Last, _, _],
  path(StartState, IntState, Path1), !,    % Get path to intermediate state
  [_|Path1Tail] = Path1,          % Strip intermediate goal state
  path(IntState, EndState, Path2), !,      % Get path from intermediate goal state
  append(Path2, Path1Tail, Path). % to end state and join paths

%% Find reverse solution from StartState to EndState
solve_rev(StartState, EndState, Path) :-
  solve_fwd(EndState, StartState, RevPath),
  reverse(RevPath, Path), !.

%% Find an optimally minimal length solution using depth-first iterative
%% deepening.  Note that run times are unacceptably long for paths of
%% length more than about 12 moves.
solve_pure(StartState, EndState) :-
  path(StartState, EndState, Path), !,
  write_solution(Path).

write_solution(Path) :-
  length(Path, N),
  Nsteps is N-1,
  %write(Nsteps), nl.   % This line is for writing number of moves only
  write('    '), write('Moves: '), write(Nsteps), nl.
  %write('Solution: (read from top down)'),
  %reverse(Path, RevPath),
  %nl, write_states(RevPath).

%% Write the solution path with track 0 re-reversed for output
write_states([]) :- nl.
write_states([H|T]) :-
  write(H), nl, write_states(T).


%%% Utilities
%%% ---------
%% first_n(N, L1, L2, L3) succeeds if L2 is a list of the first N
%% elements of L1 and L3 is what remains from L1
first_n(N, L1, L2, L3) :-
  append(L2, L3, L1), length(L2, N).

%% last_n(N, L1, L2, L3) succeeds if L2 is a list of the last N
%% elements of L1 and L3 is what remains from L1
last_n(N, L1, L2, L3) :-
  append(L3, L2, L1), length(L2, N).

%% Generate all problem starting conditions and solve them.
generate_all_problems :-
	append('solutions-full-all.txt'), !,
  permutation([1, 2, 3, 4, 5, 6, 7, 8], StartList),
	first_n(5, StartList, StartTrk1, StartTrk2),
 	StartState = [[e], StartTrk1, StartTrk2, []],
 	EndState = [[e], [1, 2, 3, 4, 5], [6, 7, 8], []],
  write('Start state: '), write(StartState), nl,
 	solve(StartState, EndState),
 	fail.

generate_all_permutations :-
  append('all-permutations.txt'), !,
  permutation([1, 2, 3, 4, 5, 6, 7, 8], L),
  write(L), write('.'), nl,
  fail.

read_permutations(FileName) :-
  see(FileName),
  repeat,
    read(Term),
    (  Term == end_of_file
    -> !
    ;  process(Term),
       fail
    ).

process(StartList) :-
	first_n(5, StartList, StartTrk1, StartTrk2),
 	StartState = [[e], StartTrk1, StartTrk2, []],
 	EndState = [[e], [1, 2, 3, 4, 5], [6, 7, 8], []],
  write('Start state: '), write(StartState),
 	solve(StartState, EndState).
