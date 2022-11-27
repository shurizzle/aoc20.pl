:- [prelude].
:- use_module(library(clpfd)).

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

% non unique values solution
solve0(Data, N, Res) :-
  length(Is, N),
  length(Data, L),
  L >= N,
  Is ins 1..L,
  all_distinct(Is),
  maplist({Data}/[I,Out]>>(element(I, Data, Out)), Is, Xs),
  sum(Xs, #=, 2020),
  label(Xs), !,
  mul_list(Xs, Res).

% unique values solution
solve1(Data, N, Res) :-
  length(Data, L),
  L >= N,
  maplist([X,X..X]>>true, Data, Domains),
  foldl1([A,B,A\/B]>>true, Domains, Domain),
  length(Xs, N),
  Xs ins Domain,
  all_distinct(Xs),
  sum(Xs, #=, 2020),
  label(Xs), !,
  mul_list(Xs, Res).

part1(Data, Res) :- solve1(Data, 2, Res).
part2(Data, Res) :- solve1(Data, 3, Res).
