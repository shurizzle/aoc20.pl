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

filtercount(_, [], Count, Count) :- !.
filtercount(Filter, [X|Xs], C0, Count) :-
  (
      call(Filter, X)
  ->  C1 is C0+1
  ;   C1 is C0
  ),
  filtercount(Filter, Xs, C1, Count).

filtercount(Filter, List, Count) :-
  filtercount(Filter, List, 0, Count).

part1(Data, Res) :-
  filtercount([(R1, R2, C, S)]>>(
    string_chars(S, Cs),
    filtercount(=(C), Cs, Count),
    Count >= R1,
    Count =< R2
  ), Data, Res).

part2(Data, Res) :-
  filtercount([(R1, R2, C, S)]>>(
    string_chars(S, Cs),
    nth1(R1, Cs, C1),
    nth1(R2, Cs, C2),
    C1 \= C2,
    (C1 = C; C2 = C)
  ), Data, Res).
