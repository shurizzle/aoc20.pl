:- [prelude].

input_file("inputs/5.txt").

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

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Seats) --> ordset_of(Seat, trim_line(row(Seat)), Seats).

row(Res) -->
  row(Res, (0,127), 7).

row(Res, (R,_), 0) --> !,
  col(Res, R, (0,7), 3).
row(Res, Range0, Count0) -->
  [C],
  { atom_char(AC, C),
    row(AC, Mapper),
    call(Mapper, Range0, Range),
    succ(Count, Count0) },
  row(Res, Range, Count).

col(Res, R, (C,_), 0, Rest, Rest) :- !,
  Res is R*8+C.
col(Res, R, Range0, Count0) -->
  [C],
  { atom_char(AC, C),
    col(AC, Mapper),
    call(Mapper, Range0, Range),
    succ(Count, Count0) },
  col(Res, R, Range, Count).

row('F', lower_half).
row('B', upper_half).
col('L', lower_half).
col('R', upper_half).

lower_half((Min, Max), Res) :-
  Max0 is ((Max-Min) div 2)+Min,
  Res = (Min, Max0).
upper_half((Min, Max), Res) :-
  lower_half((Min,Max), (_, Min0)),
  Min1 is Min0+1,
  Res = (Min1, Max).

missing([Prev, Next|_], Res) :-
  succ(Prev, Res),
  succ(Res, Next), !.
missing([_|T], Res) :- missing(T, Res).

part1(Data, Res) :- aggregate(max(N), member(N, Data), Res).
part2(Data, Res) :- missing(Data, Res).
