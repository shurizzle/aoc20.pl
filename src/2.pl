:- [prelude].

:- use_module(library(yall)).

input_file("inputs/2.txt").

tinput("1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc").

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

parse(Lines) --> list_of(trim_line(line), Lines).
line(Res) -->
  integer(Start), "-", integer(Stop),
  white, whites,
  letter(C), ":",
  white, whites,
  letters(P),
  { Res = (Start, Stop, C, P) }.

filtercount(_, [], 0) :- !.
filtercount(Filter, [X|Xs], Count) :-
  (
     call(Filter, X)
  -> filtercount(Filter, Xs, C0),
     Count is C0+1
  ;  filtercount(Filter, Xs, Count)
  ).

part1(Data, Res) :-
  filtercount([(R1, R2, C, S)]>>(
    string_chars(S, Cs),
    filtercount(=(C), Cs, Count),
    Count >= R1,
    Count =< R2
  ), Data, Res).

part2(Data, Res) :-
  filtercount([(R10, R20, C, S)]>>(
    R1 is R10-1,
    R2 is R20-1,
    string_chars(S, Cs),
    nth0(R1, Cs, C1),
    nth0(R2, Cs, C2),
    C1 \= C2,
    (C1 = C; C2 = C)
  ), Data, Res).
