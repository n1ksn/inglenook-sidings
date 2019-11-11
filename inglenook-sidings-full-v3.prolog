%%%------------------------------------------------------------------------
%%% inglenook-sidings-full-v3.prolog
%%%
%%% Find solutions to a classic shunting (switching) puzzle using Prolog.
%%% This set of predicates has been tested using SWI Prolog, gprolog, and
%%% yap on Linux and using SWI Prolog on Windows 10.
%%%
%%% Andrew Palm
%%% 2019-11-09
%%%
%%% Quick start
%%% -----------
%%% The Inglenook Sidings shunting puzzle uses the following track
%%% configuration with fixed track capacities. Note how the switching
%%% lead track (head shunt) is limited to the engine plus three cars.
%%%
%%%                                   /------------- 3
%%%                                  /    3 cars
%%%    West                         /
%%%                                /----------------  2     East
%%%        Switching Lead         /       3 cars
%%%        (Head Shunt)          /
%%%   0  -----------------------/---------------------  1
%%%        Engine + 3 cars                5 cars
%%%
%%% In this program the tracks are represented by Prolog lists containing
%%% the occupying cars in the order from west to east.
%%%
%%% The engine and car names can be any Prolog atoms (all lower case
%%% alphanumeric or the underscore character, or any string in single
%%% quotes), but cars are named by positive integers in the predicates
%%% that generate problems.
%%%
%%% The engine is assumed to be on the west end of all movements.  The
%%% engine must be on the west end of track 0 in the start and end
%%% conditions (states).
%%%
%%% On starting your version of Prolog you will see a prompt "?-".  You can
%%% use this "query" to load the code file:
%%%
%%% ?- ['inglenook-sidings-full-v3.prolog'].
%%%
%%% (assuming you are in the folder/directory where the code file resides).
%%% All entries require a period at the end.  If there are no errors you will
%%% get another prompt.  Run a test by entering the query
%%%
%%% ?- generate_problem(1).
%%%
%%% You should get a printout showing
%%%
%%%     1.  The "start state" at the beginning of the puzzle with the engine
%%%         "e" on the switching lead (track 0), five cars on track 1,
%%%         three cars on track 2, and no cars on track 3.  The cars are
%%%         in a random order.
%%%
%%%     2.  The number of engine moves (pulls or drops) to the "end state".
%%%
%%%     3.  A list of the intermediate states of the "solution path" which
%%%         shows the car positions between the engine moves.  The cars in
%%%         the end state are in numerical order, five on track 1 and three
%%%         on track 2.
%%%
%%% To exit SWI Prolog, type the query
%%%
%%% ?- halt().
%%%
%%% For gprolog and yap, omit the parentheses.
%%%
%%% Standard problems
%%% -----------------
%%% At the start there are five cars on track 1 representing an arriving
%%% train with the engine placed on the switching lead (head shunt),
%%% track 0.  There are also three cars on siding track 2, for a total
%%% of eight cars.  We assign the numbers 1 to 8 randomly as labels of
%%% the cars in their starting positions.  At the end the eight cars
%%% are distributed so that track 1 contains cars 1, 2, 3, 4, and 5 in
%%% that order from west to east, and similarly cars 6, 7, and 8 are on
%%% track 2 in order from west to east.
%%%
%%%   Start:  Track 0: engine
%%%           Track 1: 3, 2, 7, 5, 1
%%%           Track 2: 4, 8, 6
%%%           Track 3: empty
%%%   State:  [[e], [3, 2, 7, 5, 1], [4, 8, 6], []]
%%%
%%%   End:    Track 0: engine
%%%           Track 1: 1, 2, 3, 4, 5
%%%           Track 2: 6, 7, 8
%%%           Track 3: empty
%%%   State:  [[e], [1, 2, 3, 4, 5], [6, 7, 8],[]]
%%%
%%% The following query using the predicate "solve" finds and displays a
%%% solution to this problem (do not enter the Prolog prompt "?-"):
%%%
%%% ?- solve([[e],[3,2,7,5,1],[4,8,6],[]], [[e],[1,2,3,4,5],[6,7,8],[]]).
%%%
%%% The output is:
%%%
%%% Moves: 14
%%% Solution: (read from top down)
%%% [[e],[3,2,7,5,1],[4,8,6],[]]
%%% [[e,3,2,7],[5,1],[4,8,6],[]]
%%% [[e,3],[5,1],[4,8,6],[2,7]]
%%% [[e,3,5,1],[],[4,8,6],[2,7]]
%%% [[e,3,5],[],[4,8,6],[1,2,7]]
%%% [[e,3],[5],[4,8,6],[1,2,7]]
%%% [[e,3,4],[5],[8,6],[1,2,7]]
%%% [[e],[3,4,5],[8,6],[1,2,7]]
%%% [[e,1,2],[3,4,5],[8,6],[7]]
%%% [[e],[1,2,3,4,5],[8,6],[7]]
%%% [[e,8,6],[1,2,3,4,5],[],[7]]
%%% [[e,8],[1,2,3,4,5],[],[6,7]]
%%% [[e],[1,2,3,4,5],[8],[6,7]]
%%% [[e,6,7],[1,2,3,4,5],[8],[]]
%%% [[e],[1,2,3,4,5],[6,7,8],[]]
%%%
%%% Another form of this puzzle is the same as that described above, except
%%% that the order of the three cars on track 2 in the end state is not
%%% specified.  In this case the list for track 2 in the end state is
%%% replaced by an underscore, as follows:
%%%
%%% ?- solve([[e],[3,2,7,5,1],[4,8,6],[]], [[e],[1,2,3,4,5],_,[]]).
%%%
%%% Moves: 10
%%% Solution: (read from top down)
%%% [[e],[3,2,7,5,1],[4,8,6],[]]
%%% [[e,3,2,7],[5,1],[4,8,6],[]]
%%% [[e,3],[5,1],[4,8,6],[2,7]]
%%% [[e,3,5,1],[],[4,8,6],[2,7]]
%%% [[e,3,5],[],[4,8,6],[1,2,7]]
%%% [[e,3],[5],[4,8,6],[1,2,7]]
%%% [[e,3,4],[5],[8,6],[1,2,7]]
%%% [[e],[3,4,5],[8,6],[1,2,7]]
%%% [[e,1,2,7],[3,4,5],[8,6],[]]
%%% [[e,1,2],[3,4,5],[7,8,6],[]]
%%% [[e],[1,2,3,4,5],[7,8,6],[]]
%%%
%%% For the convenience of the user there are four predicates which
%%% generate one or more standard problems and find the solutions.
%%%
%%%   generate_problem(N).  Generates N standard problems and finds a
%%%   solution for each using the predicate solve.
%%%
%%%   generate_problem_orig(N).  Same as that above, but does not
%%%   specify a particular order of the three cars on track 2 in the
%%%   solution.  (This is the original variant of the puzzle.)
%%%
%%%   generate_problem_file(N).  Like generate_problem, but  writes the
%%%   solution(s) to a file.
%%%
%%%   generate_problem_orig_file(N).  Like generate_problem_orig, but
%%%   writes the solution(s) to a file.
%%%
%%% Here is an example of generating a single standard problem:
%%%
%%% ?- generate_problem(1).
%%%
%%% Start state: [[e],[2,5,8,4,3],[7,1,6],[]]
%%% Moves: 16
%%% Solution: (read from top down)
%%% [[e],[2,5,8,4,3],[7,1,6],[]]
%%% [[e,2,5,8],[4,3],[7,1,6],[]]
%%% [[e,2,5],[4,3],[7,1,6],[8]]
%%% [[e,2,5,4],[3],[7,1,6],[8]]
%%% [[e,2],[3],[7,1,6],[5,4,8]]
%%% [[e,2,3],[],[7,1,6],[5,4,8]]
%%% [[e,2,3,5],[],[7,1,6],[4,8]]
%%% [[e,2,3],[5],[7,1,6],[4,8]]
%%% [[e,2,3,4],[5],[7,1,6],[8]]
%%% [[e,2],[3,4,5],[7,1,6],[8]]
%%% [[e],[2,3,4,5],[7,1,6],[8]]
%%% [[e,7,1],[2,3,4,5],[6],[8]]
%%% [[e,7],[1,2,3,4,5],[6],[8]]
%%% [[e],[1,2,3,4,5],[6],[7,8]]
%%% [[e,6],[1,2,3,4,5],[],[7,8]]
%%% [[e,6,7,8],[1,2,3,4,5],[],[]]
%%% [[e],[1,2,3,4,5],[6,7,8],[]]
%%%
%%% Non-standard problems
%%% ---------------------
%%% Non-standard start or end states can be used with the predicate
%%% "solve" as long as the engine is on the west (bumper) end of track 0.
%%% This may be useful if the user models specific industries on the
%%% sidings.  A maximum of eight cars still applies.
%%%
%%% Care must be taken to ensure that the number of cars and their labels
%%% are identical in the start and end states and that there are no
%%% duplicate car labels.  The predicate "check_setup" can be used to
%%% check for such errors, plus the engine's position at the start and end.
%%%
%%% A non-standard problem with six cars plus a transfer caboose "tc" and
%%% an engine named "sw1":
%%%
%%% First we check the setup for possible errors.  The last two arguments
%%% of "check_setup" are the number of cars and the name of the engine.
%%% (Note that the transfer caboose is counted as a car.):
%%%
%%% ?- check_setup([[sw1,tc],[3,2,7,5],[4,6],[]], [[sw1],[6,4,tc],[2,3],[5,7]], 7, sw1).
%%% true.
%%%
%%% Note that this predicate does not work for problems with indeterminant
%%% tracks specified with an underscore.  Since the setup is OK, we make
%%% a solution query:
%%%
%%% ?- solve([[sw1,tc],[3,2,7,5],[4,6],[]], [[sw1],[6,4,tc],[2,3],[5,7]]).
%%%
%%% Moves: 14
%%% Solution: (read from top down)
%%% [[sw1,tc],[3,2,7,5],[4,6],[]]
%%% [[sw1,tc,3],[2,7,5],[4,6],[]]
%%% [[sw1],[2,7,5],[4,6],[tc,3]]
%%% [[sw1,4,6],[2,7,5],[],[tc,3]]
%%% [[sw1,4],[6,2,7,5],[],[tc,3]]
%%% [[sw1,4,tc,3],[6,2,7,5],[],[]]
%%% [[sw1],[6,2,7,5],[4,tc,3],[]]
%%% [[sw1,6,2,7],[5],[4,tc,3],[]]
%%% [[sw1,6,2],[5],[4,tc,3],[7]]
%%% [[sw1,6,2,5],[],[4,tc,3],[7]]
%%% [[sw1,6],[],[4,tc,3],[2,5,7]]
%%% [[sw1,6,4,tc],[],[3],[2,5,7]]
%%% [[sw1],[6,4,tc],[3],[2,5,7]]
%%% [[sw1,2],[6,4,tc],[3],[5,7]]
%%% [[sw1],[6,4,tc],[2,3],[5,7]]
%%%
%%% Notes on solutions
%%% ------------------
%%% Most of the time, the solutions found by the predicate "solve" are not
%%% the shortest possible.  This is because finding a minimal length
%%% solution with the base approach used here in most cases would take an
%%% unacceptably long time (possibly many hours!).  For this reason the
%%% "solve" predicate uses two "tricks."  First, it breaks the solution
%%% into two pieces connected by an intermediate state.  This intermediate
%%% state is chosen so the combined solution is of reasonable length and
%%% takes at worst a few minutes to calculate.  Second, after finding a
%%% solution from the start to the end (a "forward" solution), it looks for
%%% a solution starting at the end and going to the start (again using an
%%% intermediate state).  If this "reverse" solution is shorter than the
%%% forward solution, it is used instead since all moves are reversible.
%%%
%%% An interested user can try to find an optimal minimal length solution
%%% by using the "solve_pure" predicate, preferably on a problem
%%% with a known solution of 12 or fewer moves.  Here is an example:
%%%
%%% ?- solve_pure([[e],[6,7,1,5,2],[3,4,8],[]],[[e],[1,2,3,4,5],[6,7,8],[]]).
%%%
%%% Moves: 11
%%% Solution: (read from top down)
%%% [[e],[6,7,1,5,2],[3,4,8],[]]
%%% [[e,6,7],[1,5,2],[3,4,8],[]]
%%% [[e],[1,5,2],[3,4,8],[6,7]]
%%% [[e,1,5,2],[],[3,4,8],[6,7]]
%%% [[e,1,5],[],[3,4,8],[2,6,7]]
%%% [[e,1],[5],[3,4,8],[2,6,7]]
%%% [[e,1,3,4],[5],[8],[2,6,7]]
%%% [[e,1],[3,4,5],[8],[2,6,7]]
%%% [[e,1,2],[3,4,5],[8],[6,7]]
%%% [[e],[1,2,3,4,5],[8],[6,7]]
%%% [[e,6,7],[1,2,3,4,5],[8],[]]
%%% [[e],[1,2,3,4,5],[6,7,8],[]]
%%%
%%% No other solution to this problem has fewer moves than this solution.
%%% If we use the solve predicate on this problem we get a solution which
%%% is just one move longer:
%%%
%%% ?- solve([[e],[6,7,1,5,2],[3,4,8],[]],[[e],[1,2,3,4,5],[6,7,8],[]]).
%%%
%%% Moves: 12
%%% Solution: (read from top down)
%%% [[e],[6,7,1,5,2],[3,4,8],[]]
%%% [[e,6,7,1],[5,2],[3,4,8],[]]
%%% [[e,6],[5,2],[3,4,8],[7,1]]
%%% [[e,6,5,2],[],[3,4,8],[7,1]]
%%% [[e,6,5],[],[3,4,8],[2,7,1]]
%%% [[e,6],[5],[3,4,8],[2,7,1]]
%%% [[e,6,3,4],[5],[8],[2,7,1]]
%%% [[e,6],[3,4,5],[8],[2,7,1]]
%%% [[e,6,2],[3,4,5],[8],[7,1]]
%%% [[e,6],[2,3,4,5],[8],[7,1]]
%%% [[e,6,7,1],[2,3,4,5],[8],[]]
%%% [[e,6,7],[1,2,3,4,5],[8],[]]
%%% [[e],[1,2,3,4,5],[6,7,8],[]]
%%%
%%% Development Notes
%%% -----------------
%%% In this version, the solve predicate has been changed.  Now if the
%%% last three or more cars on track 1 are the same in the start and end
%%% states, then the heuristic intermediate node is not used.  The same
%%% is done if there are fewer than five cars total.  This gives better
%%% solutions when only a few moves are needed.
%%%
%%% Originally the intermediate goal for track 1 was set for [_,2,3,4,5],
%%% and this resulted in a run time of about 1.5 hours for typical examples.
%%% Changing this to [_,_,3,4,5] resulted in a run time of about a half
%%% hour, with small changes in the number of steps needed.  Changing the
%%% intermediate goal for track 1 to [3,4,5] resulted in shorter solutions
%%% (on average about 1 step) and much shorter run times.  Changing to
%%% [4,5] did not make a difference in the distribution of solution lengths.
%%% The same was true when track 1 was specified to have 3, 4, and 5 as its
%%% last three occupants.
%%%
%%% As per Bratko, "Prolog Programming for Artificial Intelligence, 2nd. ed.,
%%% Section 11.2, the solve_pure predicate is a depth-first iterative deepening
%%% search.  For this puzzle, this method appears to be the best one among
%%% heuristic-free searches.  The intermediate node approach used by the solve
%%% predicate is a "non-admissible heuristic" search method.  The use of an
%%% explicit (pattern-matching by unification) move predicate, rather than
%%% a calculated one, noticably speeds up the finding of solutions.
%%%
%%% Below is a test predicate for running as a single purpose executable
%%% using swipl compilation with the command:
%%% $ swipl --stand_alone=true -O -o inglenook -g one_problem -c inglenook-test.prolog
%%%
%%% one_problem :-
%%%   generate_problem(1), halt().
%%%
%%% Doing this under gprolog requires the use of an initialization predicate,
%%% but probably one won't use this kind of executable as it is so inflexible.
%%% To compile under gprolog with an interactive shell, use
%%% $ gplc --no-debugger --min-pl-bips --strip --fixed-sizes inglenook-test.prolog
%%%
%%%------------------------------------------------------------------------
%%% These are for use in yap.  They can be commented out when using
%%% swi prolog or gprolog
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(random)).
append(File) :-
  open(File, 'append', Stream),
  set_output(Stream).

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
  write('Moves: '), write(Nsteps),
  nl, write('Solution: (read from top down)'),
  reverse(Path, RevPath),
  nl, write_states(RevPath).

%% Write the solution path with track 0 re-reversed for output
write_states([]) :- nl.
write_states([H|T]) :-
  write(H), nl, write_states(T).

%% generate_problem(N) succeeds when N random start states are
%% generated and their solutions found.  The end condition is the
%% same for each problem.  The three cars not on track 1 are put
%% in a specified order on track 2 in the end condition.
generate_problem(0) :-
  write('Done!'), nl, !.
generate_problem(N) :-
  N > 0,
  rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], StartList),
  first_n(5, StartList, Trk1List, Trk2List),
  StartState = [[e], Trk1List, Trk2List, []],
  EndState = [[e], [1, 2, 3, 4, 5], [6, 7, 8], []],
  write('Start state: '), write(StartState), nl,
  solve(StartState, EndState), !,
  N1 is N-1,
  generate_problem(N1).

%% generate_problem_orig(N) is the same as generate_problem(N) except
%% that the order of the three cars on track 2 at the end is not
%% specified.  This is the original version of the puzzle.  Generally
%% this reduces the number of solution moves by 1 or 2.
generate_problem_orig(0) :-
  write('Done!'), nl, !.
generate_problem_orig(N) :-
  N > 0,
  rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], StartList),
  first_n(5, StartList, Trk1List, Trk2List),
  StartState = [[e], Trk1List, Trk2List, []],
  EndState = [[e], [1, 2, 3, 4, 5], _, []],
  write('Start state: '), write(StartState), nl,
  solve(StartState, EndState), !,
  N1 is N-1,
  generate_problem_orig(N1).

%% The next three predicates are identical to the three above, except that
%% the solutions are appended to the file "solutions.txt".  If the file
%% does not exist, it will be created.
generate_problem_file(0) :-
  % Close current output stream and switch to terminal
  current_output(Stream),
  close(Stream),
  tell(user),
  write('Done!'), nl, !.
generate_problem_file(N) :-
  N > 0,
  rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], StartList),
  first_n(5, StartList, Trk1List, Trk2List),
  StartState = [[e], Trk1List, Trk2List, []],
  EndState = [[e], [1, 2, 3, 4, 5], [6, 7, 8], []],
  append('solutions.txt'),
  write('Start state: '), write(StartState), nl,
  solve(StartState, EndState), !,
  N1 is N-1,
  generate_problem_file(N1).

generate_problem_orig_file(0) :-
  % Close current output stream and switch to terminal
  current_output(Stream),
  close(Stream),
  tell(user),
  write('Done!'), nl, !.
generate_problem_orig_file(N) :-
  N > 0,
  rnd_permu([1, 2, 3, 4, 5, 6, 7, 8], StartList),
  first_n(5, StartList, Trk1List, Trk2List),
  StartState = [[e], Trk1List, Trk2List, []],
  EndState = [[e], [1, 2, 3, 4, 5], _, []],
  append('solutions-orig.txt'),
  write('Start state: '), write(StartState), nl,
  solve(StartState, EndState), !,
  N1 is N-1,
  generate_problem_orig_file(N1).

%%% Predicates to check that a proposed problem has no errors
%%% ---------------------------------------------------------
%% check_setup(StartState, EndState, Ncars, EngName) succeeds if StartState
%% and EndState have the proper number of tracks, have no duplicate cars,
%% and do not exceed track capacities.  Also, both states must have the
%% same set of cars, less than or equal to eight, and have the engine
%% on the west end of track 0.
%% This predicate does not work for the states with indeterminate positions.
check_setup(StartState, EndState, Ncars, EngName) :-
  length(StartState, 4),                  % Correct number of tracks
  length(EndState, 4),
  flatten(StartState, FlatStartState),
  flatten(EndState, FlatEndState),
  N is Ncars + 1,
  length(FlatStartState, N),              % Correct number of cars w/engine
  length(FlatStartState, N),
  sort(FlatStartState, SortedStart),
  sort(FlatEndState, SortedEnd),
  length(SortedStart, N),                 % No duplicates
  length(SortedEnd,N),
  SortedStart = SortedEnd,                % Same cars at start & end
  nth1(1, StartState, StartTrk0),         % Engine in correct position at
  nth1(1, StartTrk0, EngName),            % west end of track 0
  nth1(1, EndState, EndTrk0),
  nth1(1, EndTrk0, EngName),
  no_trk_cap_error(StartState),
  no_trk_cap_error(EndState).

chk_trk_cap(MaxNumCars, TrkList) :-
  length(TrkList, NumCars),
  NumCars =< MaxNumCars.

no_trk_cap_error(State) :-
  maplist(chk_trk_cap, [4, 5, 3, 3], State).

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

%%% The following three predicates were taken from a webpage with solutions
%%% to "99 Problems in Prolog."  They are used for compatibility with ISO
%%% prolog and replace the SWI Prolog predicate random_permutation/3.

%% remove_at(X, L, K, R) :- X is the K'th element of the list L; R is the
%% list that remains when the K'th element is removed from L.
remove_at(X, [X|Xs], 1, Xs).
remove_at(X, [Y|Xs], K, [Y|Ys]) :-
  K > 1,
  K1 is K - 1,
  remove_at(X, Xs, K1, Ys).

%% rnd_select(L, N, R) :- the list R contains N randomly selected
%% items taken from the list L.
rnd_select(_, 0, []).
rnd_select(Xs, N, [X|Zs]) :- N > 0,
    length(Xs, L),
    random(0, L, I),
    I1 is I + 1,
    remove_at(X, Xs, I1, Ys),
    N1 is N - 1,
    rnd_select(Ys, N1, Zs).

%% rnd_permu(L1, L2) :- the list L2 is a random permutation of the
%% elements of the list L1.
rnd_permu(L1, L2) :- length(L1, N), rnd_select(L1, N, L2).
