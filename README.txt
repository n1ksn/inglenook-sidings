------------------------------------------------------------------------
inglenook-sidings-full-v2.prolog
--------------------------------
Find solutions to a classic switching (shunting) puzzle using Prolog.

inglenook-sidings-small-v2.prolog
---------------------------------
The small version of the puzzle.

The information in this README file applies to the full size version.
For similar information for the small version see the header comments
in that file.  The small version is very similar except for the track
capacities.

All predicates (procedures) have been tested using SWI Prolog and gprolog
on Linux and using SWI Prolog on Windows 10.

Andrew Palm
2019-10-28

Quick start
-----------
The Inglenook Sidings shunting puzzle uses the following track
configuration with fixed track capacities. Note how the switching
lead track (head shunt) is limited to the engine plus three cars.

                                   /------------- 3
                                  /    3 cars
    West                         /
                                /----------------  2     East
        Switching Lead         /       3 cars
        (Head Shunt)          /
   0  -----------------------/---------------------  1
        Engine + 3 cars                5 cars

In this program the tracks are represented by Prolog lists containing
the occupying cars in the order from west to east.

The engine and car names can be any Prolog atoms (all lower case
alphanumeric or the underscore character, or any string in single
quotes), but cars are named by positive integers in the predicates
that generate problems.

The engine is assumed to be on the west end of all movements.  The
engine must be on the west end of track 0 in the start and end
conditions (states).  Its name can be any Prolog atom, but in the
generate problem predicates it is named "e".

On starting your version of Prolog you will see a prompt "?-".  You can
use this "query" to load the code file:

?- ['inglenook-sidings-full-v2.prolog'].

(assuming you are in the folder/directory where the code file resides).
All entries require a period at the end.  If there are no errors you will
get another prompt.  Run a test by entering the query

?- generate_problem(1).

You should get a printout showing

    1.  The "start state" at the beginning of the puzzle with the engine
        "e" on the switching lead (track 0), five cars on track 1,
        three cars on track 2, and no cars on track 3.  The cars are
        in a random order.

    2.  The number of engine moves (pulls or drops) to the "end state".

    3.  A list of the intermediate states of the "solution path" which
        shows the car positions between the engine moves.  The cars in
        the end state are in numerical order, five on track 1 and three
        on track 2.

To exit SWI Prolog, type the query

?- halt().

For gprolog, omit the parentheses.

Standard problems
-----------------
At the start there are five cars on track 1 representing an arriving
train with the engine placed on the switching lead (head shunt),
track 0.  There are also three cars on siding track 2, for a total
of eight cars.  We assign the numbers 1 to 8 randomly as labels of
the cars in their starting positions.  At the end the eight cars
are distributed so that track 1 contains cars 1, 2, 3, 4, and 5 in
that order from west to east, and similarly cars 6, 7, and 8 are on
track 2 in order from west to east.

   Start:  Track 0: engine
           Track 1: 3, 2, 7, 5, 1
           Track 2: 4, 8, 6
           Track 3: empty
   State:  [[e], [3, 2, 7, 5, 1], [4, 8, 6], []]

   End:    Track 0: engine
           Track 1: 1, 2, 3, 4, 5
           Track 2: 6, 7, 8
           Track 3: empty
   State:  [[e], [1, 2, 3, 4, 5], [6, 7, 8],[]]

The following query using the predicate "solve" finds and displays a
solution to this problem (do not enter the Prolog prompt "?-"):

?- solve([[e],[3,2,7,5,1],[4,8,6],[]], [[e],[1,2,3,4,5],[6,7,8],[]]).

The output is:

Moves: 14
Solution: (read from top down)
[[e],[3,2,7,5,1],[4,8,6],[]]
[[e,3,2,7],[5,1],[4,8,6],[]]
[[e,3],[5,1],[4,8,6],[2,7]]
[[e,3,5,1],[],[4,8,6],[2,7]]
[[e,3,5],[],[4,8,6],[1,2,7]]
[[e,3],[5],[4,8,6],[1,2,7]]
[[e,3,4],[5],[8,6],[1,2,7]]
[[e],[3,4,5],[8,6],[1,2,7]]
[[e,1,2],[3,4,5],[8,6],[7]]
[[e],[1,2,3,4,5],[8,6],[7]]
[[e,8,6],[1,2,3,4,5],[],[7]]
[[e,8],[1,2,3,4,5],[],[6,7]]
[[e],[1,2,3,4,5],[8],[6,7]]
[[e,6,7],[1,2,3,4,5],[8],[]]
[[e],[1,2,3,4,5],[6,7,8],[]]

Another form of this puzzle is the same as that described above, except
that the order of the three cars on track 2 in the end state is not
specified.  In this case the list for track 2 in the end state is
replaced by an underscore, as follows:

?- solve([[e],[3,2,7,5,1],[4,8,6],[]], [[e],[1,2,3,4,5],_,[]]).

Moves: 10
Solution: (read from top down)
[[e],[3,2,7,5,1],[4,8,6],[]]
[[e,3,2,7],[5,1],[4,8,6],[]]
[[e,3],[5,1],[4,8,6],[2,7]]
[[e,3,5,1],[],[4,8,6],[2,7]]
[[e,3,5],[],[4,8,6],[1,2,7]]
[[e,3],[5],[4,8,6],[1,2,7]]
[[e,3,4],[5],[8,6],[1,2,7]]
[[e],[3,4,5],[8,6],[1,2,7]]
[[e,1,2,7],[3,4,5],[8,6],[]]
[[e,1,2],[3,4,5],[7,8,6],[]]
[[e],[1,2,3,4,5],[7,8,6],[]]

For the convenience of the user there are four predicates which
generate one or more standard problems and find the solutions.

  generate_problem(N).  Generates N standard problems and finds a
  solution for each using the predicate solve.

  generate_problem_orig(N).  Same as that above, but does not
  specify a particular order of the three cars on track 2 in the
  solution.  (This is the original variant of the puzzle.)

  generate_problem_file(N).  Like generate_problem, but writes the
  solution(s) to a file.

  generate_problem_orig_file(N).  Like generate_problem_orig, but
  writes the solution(s) to a file.

Here is an example of generating a single standard problem:

?- generate_problem(1).

Start state: [[e],[2,5,8,4,3],[7,1,6],[]]
Moves: 16
Solution: (read from top down)
[[e],[2,5,8,4,3],[7,1,6],[]]
[[e,2,5,8],[4,3],[7,1,6],[]]
[[e,2,5],[4,3],[7,1,6],[8]]
[[e,2,5,4],[3],[7,1,6],[8]]
[[e,2],[3],[7,1,6],[5,4,8]]
[[e,2,3],[],[7,1,6],[5,4,8]]
[[e,2,3,5],[],[7,1,6],[4,8]]
[[e,2,3],[5],[7,1,6],[4,8]]
[[e,2,3,4],[5],[7,1,6],[8]]
[[e,2],[3,4,5],[7,1,6],[8]]
[[e],[2,3,4,5],[7,1,6],[8]]
[[e,7,1],[2,3,4,5],[6],[8]]
[[e,7],[1,2,3,4,5],[6],[8]]
[[e],[1,2,3,4,5],[6],[7,8]]
[[e,6],[1,2,3,4,5],[],[7,8]]
[[e,6,7,8],[1,2,3,4,5],[],[]]
[[e],[1,2,3,4,5],[6,7,8],[]]

Non-standard problems
---------------------
Non-standard start or end states can be used with the predicate
"solve" as long as the engine is on the west (bumper) end of track 0.
This may be useful if the user models specific industries on the
sidings.  A maximum of eight cars still applies.

Care must be taken to ensure that the number of cars and their labels
are identical in the start and end states and that there are no
duplicate car labels.  The predicate "check_setup" can be used to
check for such errors, plus the engine's position at the start and end.

A non-standard problem with six cars plus a transfer caboose "tc" and
an engine named "sw1":

First we check the setup for possible errors.  The last two arguments
of "check_setup" are the number of cars and the name of the engine.
(Note that the transfer caboose is counted as a car.):

?- check_setup([[sw1,tc],[3,2,7,5],[4,6],[]], [[sw1],[6,4,tc],[2,3],[5,7]], 7, sw1).
true.

Note that this predicate does not work for problems with indeterminant
tracks specified with an underscore.  Since the setup is OK, we make
a solution query:

?- solve([[sw1,tc],[3,2,7,5],[4,6],[]], [[sw1],[6,4,tc],[2,3],[5,7]]).

Moves: 14
Solution: (read from top down)
[[sw1,tc],[3,2,7,5],[4,6],[]]
[[sw1],[tc,3,2,7,5],[4,6],[]]
[[sw1,4,6],[tc,3,2,7,5],[],[]]
[[sw1,4],[tc,3,2,7,5],[],[6]]
[[sw1,4,tc,3],[2,7,5],[],[6]]
[[sw1],[2,7,5],[4,tc,3],[6]]
[[sw1,6],[2,7,5],[4,tc,3],[]]
[[sw1,6,2,7],[5],[4,tc,3],[]]
[[sw1,6,2],[5],[4,tc,3],[7]]
[[sw1,6,2,5],[],[4,tc,3],[7]]
[[sw1,6],[],[4,tc,3],[2,5,7]]
[[sw1,6,4,tc],[],[3],[2,5,7]]
[[sw1],[6,4,tc],[3],[2,5,7]]
[[sw1,2],[6,4,tc],[3],[5,7]]
[[sw1],[6,4,tc],[2,3],[5,7]]

Notes on solutions
------------------
Most of the time, the solutions found by the predicate "solve" are not
the shortest possible.  This is because finding a minimal length
solution with the base approach used here in most cases would take an
unacceptably long time (possibly many hours!).  For this reason the
"solve" predicate uses two "tricks."  First, it breaks the solution
into two pieces connected by an intermediate state.  This intermediate
state is chosen so the combined solution is of reasonable length and
takes at worst a few minutes to calculate.  Second, after finding a
solution from the start to the end (a "forward" solution), it looks for
a solution starting at the end and going to the start (again using an
intermediate state).  If this "reverse" solution is shorter than the
forward solution, it is used instead since all moves are reversible.

An interested user can try to find an optimal minimal length solution
by using the "solve_pure" predicate, preferably on a problem
with a known solution of 12 or fewer moves.  Here is an example:

?- solve_pure([[e],[6,7,1,5,2],[3,4,8],[]],[[e],[1,2,3,4,5],[6,7,8],[]]).

Moves: 11
Solution: (read from top down)
[[e],[6,7,1,5,2],[3,4,8],[]]
[[e,6,7],[1,5,2],[3,4,8],[]]
[[e],[1,5,2],[3,4,8],[6,7]]
[[e,1,5,2],[],[3,4,8],[6,7]]
[[e,1,5],[],[3,4,8],[2,6,7]]
[[e,1],[5],[3,4,8],[2,6,7]]
[[e,1,3,4],[5],[8],[2,6,7]]
[[e,1],[3,4,5],[8],[2,6,7]]
[[e,1,2],[3,4,5],[8],[6,7]]
[[e],[1,2,3,4,5],[8],[6,7]]
[[e,6,7],[1,2,3,4,5],[8],[]]
[[e],[1,2,3,4,5],[6,7,8],[]]

No other solution to this problem has fewer moves than this solution.
If we use the solve predicate on this problem we get a solution which
is just one move longer:

?- solve([[e],[6,7,1,5,2],[3,4,8],[]],[[e],[1,2,3,4,5],[6,7,8],[]]).

Moves: 12
Solution: (read from top down)
[[e],[6,7,1,5,2],[3,4,8],[]]
[[e,6,7,1],[5,2],[3,4,8],[]]
[[e,6],[5,2],[3,4,8],[7,1]]
[[e,6,5,2],[],[3,4,8],[7,1]]
[[e,6,5],[],[3,4,8],[2,7,1]]
[[e,6],[5],[3,4,8],[2,7,1]]
[[e,6,3,4],[5],[8],[2,7,1]]
[[e,6],[3,4,5],[8],[2,7,1]]
[[e,6,2],[3,4,5],[8],[7,1]]
[[e,6],[2,3,4,5],[8],[7,1]]
[[e,6,7,1],[2,3,4,5],[8],[]]
[[e,6,7],[1,2,3,4,5],[8],[]]
[[e],[1,2,3,4,5],[6,7,8],[]]

Development Notes
-----------------
Originally the intermediate goal for track 1 was set for [_,2,3,4,5],
and this resulted in a run time of about 1.5 hours for typical examples.
Changing this to [_,_,3,4,5] resulted in a run time of about a half
hour, with small changes in the number of steps needed.  Changing the
intermediate goal for track 1 to [3,4,5] resulted in shorter solutions
(on average about 1 step) and much shorter run times.  Changing to
[4,5] did not make a difference in the distribution of solution lengths.
The same was true when track 1 was specified to have 3, 4, and 5 as its
last three occupants.

As per Bratko, "Prolog Programming for Artificial Intelligence, 2nd. ed.,
Section 11.2, the solve_pure predicate is a depth-first iterative deepening
search.  For this puzzle, this method appears to be the best one among
heuristic-free searches.  The intermediate node approach used by the solve
predicate is a "non-admissible heuristic" search method.  The use of an
explicit (pattern-matching by unification) move predicate, rather than
a calculated one, noticably speeds up the finding of solutions.

------------------------------------------------------------------------

