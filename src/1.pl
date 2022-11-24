:- [prelude].

input_file("inputs/1.txt").

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Is) --> list_of(I, trim_line(integer(I)), Is).

% tests {{{
tinput("
1721
979
366
299
675
1456").

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

solve0(Data, N, Res) :-
  combinations(Data, N, List),
  sum_list(List, 2020), !,
  mul_list(List, Res).

part1(Data, Res) :- solve0(Data, 2, Res).
part2(Data, Res) :- solve0(Data, 3, Res).
