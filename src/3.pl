:- [prelude].

input_file("inputs/3.txt").

tinput("..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#").

test1(Data) :- part1(Data, Res), print1(Res), nl.
test1_from_string(Input) :- input_from_string(Input, Data), test1(Data).
test1 :- tinput(Data), test1_from_string(Data).

test2(Data) :- part2(Data, Res), print2(Res), nl.
test2_from_string(Input) :- input_from_string(Input, Data), test2(Data).
test2 :- tinput(Data), test2_from_string(Data).

test(Data) :- test1(Data), test2(Data).
test_from_string(Input) :- input_from_string(Input, Data), test(Data).
test :- tinput(Input), test_from_string(Input).

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Lines) --> list_of(raw_line(line), Lines), blanks.
line([S|Ss]) --> square(S), !, list_of(square, Ss).
square(X) --> free_square(X); tree_square(X).
free_square(false) --> ".".
tree_square(true) --> "#".

advance(IncX, IncY, X0, Y0, X, Y) :-
  X is X0+IncX,
  Y is Y0+IncY.

count_trees(Matrix, Width, Advance, X0,Y0, C0, Count) :-
  nth0(Y0, Matrix, Line),
  nth0(X0, Line, Square), !,
  (
      Square == true
  ->  C1 is C0+1
  ;   C1 is C0
  ),
  call(Advance, X0, Y0, X1, Y),
  X is X1 mod Width,
  count_trees(Matrix, Width, Advance, X,Y, C1, Count).
count_trees(_, _, _, _, _, Count, Count).

count_trees(Matrix, Width, Advance, X0,Y0, Count) :-
  count_trees(Matrix, Width, Advance, X0,Y0, 0, Count).

part1(Data, Res) :-
  nth0(0, Data, Line0),
  length(Line0, Width),
  count_trees(Data, Width, advance(3, 1), 0,0, Res).

part2(Data, Res) :-
  nth0(0, Data, Line0),
  length(Line0, Width),
  maplist(
    {Data, Width}/[(X,Y), Out]>>(
      count_trees(Data, Width, advance(X,Y), 0,0, Out)),
    [(1,1), (3,1), (5,1), (7,1), (1,2)],
    Trees),
  mul_list(Trees, Res).
