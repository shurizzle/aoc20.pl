:- [prelude].

input_file("inputs/5.txt").

% tests {{{
tinput("FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL").

test1(Data) :- part1(Data, Res), print1(Res), nl.
test1_from_string(Input) :- input_from_string(Input, Data), test1(Data).
test1 :- tinput(Data), test1_from_string(Data).

test2(Data) :- part2(Data, Res), print2(Res), nl.
test2_from_string(Input) :- input_from_string(Input, Data), test2(Data).
test2 :- tinput(Data), test2_from_string(Data).

test(Data) :- test1(Data), test2(Data).
test_from_string(Input) :- input_from_string(Input, Data), test(Data).
test :- tinput(Input), test_from_string(Input).
% }}}

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Seats) --> ordset_of(Seat, trim_line(row(Seat)), Seats).

row(Res) -->
  row(Res, (0,127), 7).

row(Res, (R,_), 0) --> !,
  col(Res, R, (0,7), 3).
row(Res, Range0, Count0) -->
  [C],
  { row(C, Mapper),
    call(Mapper, Range0, Range),
    succ(Count, Count0) },
  row(Res, Range, Count).

col(Res, R, (C,_), 0, Rest, Rest) :- !,
  Res is R*8+C.
col(Res, R, Range0, Count0) -->
  [C],
  { col(C, Mapper),
    call(Mapper, Range0, Range),
    succ(Count, Count0) },
  col(Res, R, Range, Count).

row(0'F, lower_half).
row(0'B, upper_half).
col(0'L, lower_half).
col(0'R, upper_half).

lower_half((Min, Max), (Min, Max0)) :-
  Max0 is ((Max-Min) div 2)+Min.
upper_half((Min, Max), (Min1, Max)) :-
  lower_half((Min,Max), (_, Min0)),
  Min1 is Min0+1.

missing([Prev, Next|_], Res) :-
  succ(Prev, Res),
  succ(Res, Next), !.
missing([_|T], Res) :- missing(T, Res).

part1(Data, Res) :- aggregate(max(N), member(N, Data), Res).
part2(Data, Res) :- missing(Data, Res).
