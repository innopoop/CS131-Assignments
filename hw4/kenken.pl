%Get Ith row, Jth Element
ijthElement(T, [I|J], El):-
	nth(I, T, Row),
	nth(J, Row, El).

%Transpose code from github code help "sudoku_cell.pl": https://github.com/CS131-TA-team/UCLA_CS131_CodeHelp/blob/master/Prolog/sudoku_cell.pl
transpose([], []).
transpose([F|Fs], Ts):-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]):-
	lists_firsts_rests(Ms, Ts, Ms1),
	transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]):-
	lists_firsts_rests(Rest, Fs, Oss).

%Check operations
sum(_, [], 0).
sum(List, [H | T], Sum):-
	ijthElement(List, H, El),
	sum(List, T, NewSum),
	NewSum #= Sum - El.

product(_, [], 1).
product(List, [H | T], Product):-
	ijthElement(List, H, El),
	product(List, T, NewProduct),
	NewProduct #= Product / El.

subtract(List, Diff, J, K):-
	ijthElement(List, J, El1),
	ijthElement(List, K, El2),
	(Diff #= El1 - El2; Diff #= El2 - El1).

divide(List, Quotient, J, K):-
	ijthElement(List, J, El1),
	ijthElement(List, K, El2),
	(Quotient #= El1 / El2; Quotient #= El2 / El1).

%Constrain matrix based off of provided operations
fd_constraint(List, []).
fd_constraint(List, [H | T]):-
	fd_constraint(List, H),
	fd_constraint(List, T).

fd_constraint(List, +(S,L)):-
	sum(List, L, S).

fd_constraint(List, *(P,L)):-
	product(List, L, P).

fd_constraint(List, /(Q, J, K)):-
	divide(List, Q, J, K).

fd_constraint(List, -(D, J, K)):-
	subtract(List, D, J, K).

%Check whether something is in the domain
in_domain(_, []).
in_domain(N, [H | T]):-
	fd_domain(H, 1, N),
	in_domain(N, T).

%Create a list of length N
createList(N, List):-
	length(List, N).

%Create a N by N matrix
n_by_n(N, T):-
	createList(N, T),
	maplist(createList(N), T).

/*
	Create a NxN Matrix
	Constrain elements to be different
	Have elements be in the domain
	Transpose the matrix
	Ensure elements are different after transposition
	Apply every constraint in C on the transposed matrix
*/
kenken(N, C, T):-
	n_by_n(N, Transposed),
	maplist(fd_all_different, Transposed),
	in_domain(N, Transposed),
	transpose(Transposed, T),
	maplist(fd_all_different, T),
	maplist(fd_constraint(T), C),
	maplist(fd_labeling,T).

%Check whether operations are valid plainly
plain_sum(_, [], FinalSum, FinalSum).
plain_sum(List, [H | T], CurSum, FinalSum):-
	ijthElement(List, H, El),
	NewSum is CurSum + El,
	plain_sum(List, T, NewSum, FinalSum).

plain_product(_, [], 1).
plain_product(List, [H | T], Product):-
	ijthElement(List, H, El),
	0 is (Product mod El),
	NewProduct is Product // El,
	plain_product(List, T, NewProduct).

plain_subtract(List, Diff, J, K):-
	ijthElement(List, J, El1),
	ijthElement(List, K, El2),
	Diff1 is El1 - El2,
	Diff2 is El2 - El1,
	(Diff = Diff1 ; Diff = Diff2).

plain_divide(List, Quotient, J, K):-
	ijthElement(List, J, El1),
	ijthElement(List, K, El2),
	Q1 is Quotient * El1,
	Q2 is Quotient * El2,
	(El1 =:= Q2; El2 = Q1).

%Constrain matrix plainly
plain_constraint(List, []).
plain_constraint(List, [H | T]):-
	plain_constraint(List, H),
	plain_constraint(List, T).

plain_constraint(List, +(S, L)):-
	plain_sum(List, L, 0, S).

plain_constraint(List, *(P, L)):-
	plain_product(List, L, P).

plain_constraint(List, -(D, J, K)):-
	plain_subtract(List, D, J, K).

plain_constraint(List, /(Q, J, K)):-
	plain_divide(List, Q, J, K).

plain_all_different(Domain, Row):-
	permutation(Domain, Row).

%Create a domain by which to limit the matrix to
createDomain(N, Domain):-
	findall(El, between(1, N, El), Domain).

/*
	Create a NxN Matrix
	Create a domain for the predicate to constrain domain with permutation
	Transpose the matrix
	Check whether elements are different again
	Apply every constraint in C on the transposed matrix
*/
plain_kenken(N, C,T):-
	n_by_n(N,Transposed),
	createDomain(N, Domain),
	maplist(plain_all_different(Domain), Transposed),
	transpose(Transposed, T),
	maplist(plain_all_different(Domain), T),
	maplist(plain_constraint(T), C).

/*No-Op Kenken API

N: Dimension of N by N Matrix

C: List of (Result (Number), List of Squares ([I|J]) Related)

T: Solved No-Op Puzzle

Operations: Variable passed in to capture the result

noop_kenken_testcase(
	5,
	[
		(10, [[1|1], [1|2]]),
		(10, [[1|3], [1|4], [1|5], [2|5]]),
		(10, [[2|1], [3|1], [3|2]]),
		(10, [[2|2], [2|3], [2|4]]),
		(10, [[3|3],[3|4],[3|5]]),
		(10, [[4|1], [4|2], [5|1]]),
		(10, [[4|3], [5|2], [5|3], [5|4]]),
		(10, [[4|4], [4|5], [5|5]])
	]
).

This predicate checks for whether an operation is valid for a certain 

getOperations(_, FinalOperations, FinalOperations).
getOperations((Result, Squares), CurOps, FinalOperations):-
	checkAddition(Result, Squares, CurOps, FinalOperations);
	checkMultiplication(Result, Squares, CurOps, FinalOperations);
	checkSubtraction(Result, Squares, CurOps, FinalOperations);
	checkDivision(Result, Squares, CurOps, FinalOperations);
	getOperations(0, FinalOperations, FinalOperations).

The following set of predicates check whether it is possible to format squares using the respective operations and accepts it as true if so
checkAddition(Result, Squares, Tmp, Final)

checkMultiplication(Result, Squares, Tmp, Final)

checkSubtraction(Result, Squares, Tmp, Final)

checkDivision(Result, Squares, Tmp, Final)

testOperations(N, Operations, T, Operations).
testOperations(N, [OpHd | OpTl], T, Operations):-
	kenken(N, OpHd, T),
	testOperations(N, OpHd, T, OpHd).
testOperations(N, [OpHd | OpTl], T, Operations):-
	testOperations(N, OpTl, T, Operations).

noop_kenken(N, C, T, Operations)
- Goes through C and maps each C with possible operations that can be performed given the number of squares and the Result
	- If Result > N, division and subtraction are not options as PossibleOperations
	- If Result == 1, addition and multiplication are not options as operations
	- Stores the possible operations as solutions in PossibleOperations [+, *, *, + , ... ] of length length(C).
- maplist(makeOperations(LinkedOperations, C), PossibleOperations) that creates a list of prefixed operations to their squares as the form of C in kenken
	- LinkedOperations is the set of solutions based off of the multiple results of Possible Operations
- Apply LinkedOperations until a successful matrix has been made
	- testOperations(N, LinkedOperations, T, Operations).
*/

%STATISTICS OF KENKEN VS. PLAIN_KENKEN
/*

	Here are the results of the kenken vs plain_kenken cases for a 4x4 matrix. The difference
	is clearly seen by the runtime of the two- kenken finished in a single ms whereas plain_kenken took
	1727ms to do the same operations.

	kenken(
		4,
		[
			+(6, [[1|1], [1|2], [2|1]]),
			*(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
			-(1, [3|1], [3|2]),
			-(1, [4|1], [4|2]),
			+(8, [[3|3], [4|3], [4|4]]),
			*(2, [[3|4]])
		],
		T
	), write(T), nl, fail.

	(1ms)

	Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            2 Kb        32765 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1813 atoms     30955 atoms

	Times              since start      since last

	user   time     401.936 sec       0.000 sec
	system time      15.254 sec       0.011 sec
	cpu    time     417.190 sec       0.011 sec
	real   time    5980.655 sec      20.277 sec


	plain_kenken(
		4,
		[
			+(6, [[1|1], [1|2], [2|1]]),
			*(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
			-(1, [3|1], [3|2]),
			-(1, [4|1], [4|2]),
			+(8, [[3|3], [4|3], [4|4]]),
			*(2, [[3|4]])
		],
		T
	), write(T), nl, fail.

	(1727ms)

	Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            2 Kb        32765 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1815 atoms     30953 atoms

	Times              since start      since last

	user   time     403.668 sec       1.732 sec
	system time      15.273 sec       0.019 sec
	cpu    time     418.941 sec       1.751 sec
	real   time    6184.729 sec     204.074 sec

*/