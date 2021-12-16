/* Lab 3  : Deadline 13th November 2021 (Sat) 11pm

   Generalizing Sudoku Puzzle in Prolog
   
   Below is a solution to Sudoku Puzzle
   can be found in SWI-Prolog web site. It is fun
   to see how it works for a 9 x 9 sudoku puzzle
   with 3 x 3 mini-blocks. If you have not played
   Sudoku before, please try mini-sudoku first:
      https://www.mathinenglish.com/puzzlessudoku.php

   There are variations of the sudoku puzzle
   with different main grid sizes and mini-block sizes.

   For example, junior-sudoku is based on
       4 x 4 grid with 2 x 2 mini-blocks

   Another example is mini-sudoku that is based on
       6 x 6 grid with 3 x 2 mini-blocks

   Task 1 (85%)
   ======
   Generalize your sudoku solution generator using
   a new predicate gen_sudoku below which supports
   different variations of sudoku puzzles, based on
   grid and mini-block sizes.

   gen_sudoku(Rows,N,B_R,B_C)
      N - size of entire block of N x N
      B_R - mini-block row size
      B_C - mini-block column size

   We can add the following constraints:
         N #>3, B_R >1, B_C>1, N #= B_R * B_C
   To restrict ourselves to regular-shaped sudokus that
   that are easier for humans to follow.
   
   The output for gen_sudoku will be made using maplist(portray_clause, Rows)
   in the query predicate.

Below are some test cases for mini and junior 
sudoku test cases.

mini_sudoku(1,[[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

junior_sudoku(1,[[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

mini_sudoku 1: 

No result. 

junior_sudoku 1 :

[2, 4, 3, 1].
[3, 1, 4, 2].
[1, 3, 2, 4].
[4, 2, 1, 3].

   
*/

:- use_module(library(clpfd)).

/*
  Doc on maplist (similar to Haskell's map)
   https://www.swi-prolog.org/pldoc/man?predicate=maplist/2
  Doc on transpose/2 (for list of lists of same length)
   https://www.swi-prolog.org/pldoc/doc_for?object=clpfd%3Atranspose/2
  Doc on append/2 (to concatenate a list of lists)
   https://www.swi-prolog.org/pldoc/doc_for?object=append/2
  Doc on same_length/2 (to concatenate a list of lists)
   https://www.swi-prolog.org/pldoc/doc_for?object=same_length/2
  
*/

/*
   append(Rows,Vs) concats them into Vs. 
*/
sudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
        blocks(As, Bs, Cs),
        blocks(Ds, Es, Fs),
        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).

problem(2, [[3,_,_,8,_,1,_,_,2],
            [2,_,1,_,3,_,6,_,4],
            [_,_,_,2,_,4,_,_,_],
            [8,_,9,_,_,_,1,_,6],
            [_,6,_,_,_,_,_,5,_],
            [7,_,2,_,_,_,4,_,9],
            [_,_,_,5,_,9,_,_,_],
            [9,_,4,_,8,_,7,_,5],
            [6,_,_,1,_,7,_,_,3]]).

mini_sudoku(1,[[_,_,6,_,4,_],
               [_,_,_,_,6,_],
               [_,_,_,5,_,3],
               [3,_,_,_,_,_],
               [_,1,_,_,_,_],
               [_,5,_,_,4,_]]).

junior_sudoku(1,[[_,4,_,1],
                 [3,_,4,_],
                 [1,_,_,4],
                 [_,2,1,_]]).

query(Prob,Rows) :- problem(Prob, Rows), sudoku(Rows), maplist(portray_clause, Rows).

first_n(0, [], Xs).
first_n(N, [X|Ys], [X|Xs]) :- first_n(M, Ys, Xs), M #= N-1.
remove_n(0, Xs, Xs).
remove_n(N, Ys, [X|Xs]) :- remove_n(M, Ys, Xs), M #= N-1.
split_N(N, Xs, Ys, Zs) :- first_n(N, Ys, Xs), remove_n(N, Zs, Xs).


gen_sudoku(Rows,N,B_R,B_C):- N #>1, B_R >1, B_C>1, N #= B_R * B_C,
        length(Rows, N), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..N,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        check_block_constraints_full(Rows,N,B_R,B_C).

check_block_constraints_full([], N, B_R, B_C).
check_block_constraints_full(Rows,N,B_R,B_C):-
   split_N(B_R, Rows, FirstFewRows, RestOfRows),
   check_block_constraints_row_block(FirstFewRows, N, B_R, B_C),
   check_block_constraints_full(RestOfRows, N, B_R, B_C).

check_block_constraints_row_block(Rows,N,B_R,B_C):-
   transpose(Rows,Columns),
   check_block_constraints_columns(Columns,N,B_R,B_C).

check_block_constraints_columns([], N, B_R, B_C).
check_block_constraints_columns(Columns,N,B_R,B_C):-
   split_N(B_C, Columns, FirstFewColumns, RestOfColumns),
   check_block_constraints_block(FirstFewColumns,N),
   check_block_constraints_columns(RestOfColumns, N, B_R, B_C).

check_block_constraints_block(Columns,N):-
   append(Columns,Vs),
   all_distinct(Vs),
   Vs ins 1..N.

mini_sudoku(Rows) :- gen_sudoku(Rows,6,2,3).
junior_sudoku(Rows) :- gen_sudoku(Rows,4,2,2).
new_sudoku(Rows) :- gen_sudoku(Rows,9,3,3).

new_query(Prob,Rows) :- problem(Prob, Rows), new_sudoku(Rows), maplist(portray_clause, Rows).



/*

   Task 2 (15%)
   ======
   Design a problem sudoku generator, called
           find_puzzle_sudoku(Rows,S,N,M,B_R,B_C)
   that would generate a random sudoku puzzle of grid size S x S,
   mini-blocks B_R x B_C and which has from N to M known number of values. 

   Your solution may make use of random generator predicate
      random(+L:int, +U:int, -R:int)

   It should start with a random puzzle (whose numbers
   are well-spaced out) with N known numbers.
   If this did not return a unique solution, you could
   add one more (random) number to this incomplete puzzle,
   You can progressively do that until it hits M known numbers.

   If no unique puzzle is found with M known numbers, you can
   exit with a false outcome.

   PS You may use a predicate  aggregate_all(count, pred(X), Count). to count number of solutions.
   See https://stackoverflow.com/questions/6060268/prolog-count-the-number-of-times-a-predicate-is-true

   Due to the use of randomization, kindly note that the solution you get
   from this predicate is non-deterministic. You should try to
   think of some solutions that would give you the best possible outcome
   with smallest number of random values used,
   but without sacrificing too much on the time taken for your puzzle
   generator to terminate. If appropriate, you may repeat some of the 
   randomization processes but bearing in mind a trade-off between a 
   better solution versus time-out.
   
   We shall have a mini-competition to see who has the best find_puzzle_sudoku code. 
   For this mini-competition, the winner is one who can use the smallest number of
   known values used, followed by time taken. The competition is 
   just for fun and to encourage you to try your best.
   
*/






empty_list(0,[]).
empty_list(N,[_|Xs]) :- M is N-1, empty_list(M, Xs).

empty_grid(0,[[]]).
empty_grid(N,Xs) :- empty_grid_helper(N,N,Xs).

empty_grid_helper(N,0,[]).
empty_grid_helper(N,Remaining,[X|Xs]):-
    empty_list(N,X),M is Remaining-1, empty_grid_helper(N,M,Xs).

can_replace_in_list(1, [X|Xs]) :- not(integer(X)).
can_replace_in_list(N, [X|Xs]) :-
    M is N-1, can_replace_in_list(M, Xs).

replace_in_list(1, [X|Xs], NewVal, [Y|Ys]) :- 
    Y=NewVal, Xs=Ys.
replace_in_list(N, [X|Xs], NewVal, [X|Ys]) :-
    M is N-1, replace_in_list(M, Xs, NewVal, Ys).

can_replace_in_grid(1, N2, [X|Xs]) :- can_replace_in_list(N2, X). 
can_replace_in_grid(N1, N2, [X|Xs]) :-
    M is N1-1, can_replace_in_grid(M, N2, Xs).

replace_in_grid(1, N2, [X|Xs], NewVal, [Y|Ys]) :-
    replace_in_list(N2, X, NewVal, Y), Xs=Ys.
replace_in_grid(N1, N2, [X|Xs], NewVal, [X|Ys]) :-
    M is N1-1, replace_in_grid(M, N2, Xs, NewVal, Ys). 
    
count_solns(Rows, SolnCount, S, B_R, B_C):-
		aggregate_all(count, gen_sudoku(Rows, S, B_R, B_C), SolnCount).

find_puzzle_sudoku(Rows,S,N,M,B_R,B_C):-
    N #>1, B_R >1, B_C>1, N #= B_R * B_C, N #< M, S #= B_R * B_C,
    empty_grid(S, EmptyGrid),
    fill_nums(EmptyGrid, S,N,M,B_R,B_C, Rows).

fill_nums(SoFar, S, 0, 0, B_R, B_C, Rows) :-
    count_solns(SoFar, 1, S, B_R, B_C),
    Rows = SoFar.

fill_nums(SoFar, S, 0, M, B_R, B_C, Rows) :-
    (   
    	count_solns(SoFar, SolnCount, S, B_R, B_C),
    	SolnCount = 1,
        Rows = SoFar
    );
    ( 
    	fill_nums(SoFar, S, 1, M, B_R, B_C, Rows)
    ).

fill_nums(SoFar, S, N, M, B_R, B_C, Rows) :- 
	UpperBound is S+1,
    random(1, UpperBound, Rand1),
    random(1, UpperBound, Rand2), 
    random(1, UpperBound, Rand3),
    (
    	can_replace_in_grid(Rand1, Rand2, SoFar),
        replace_in_grid(Rand1, Rand2, SoFar, Rand3, NewSoFar),
        count_solns(NewSoFar, SolnCount, S, B_R, B_C),
        SolnCount #> 0,
        NewN is N-1,
        NewM is M-1,
        fill_nums(NewSoFar, S, NewN, NewM, B_R, B_C, Rows)
    );
    (
    	fill_nums(SoFar, S, N, M, B_R, B_C, Rows)
    ).
        
  
