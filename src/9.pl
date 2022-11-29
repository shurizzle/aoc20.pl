:- [prelude].

input_file("inputs/9.txt").

% tests {{{
tinput("").

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

parse(Is) --> list_of(I, trim_line(integer(I)), Is).

valid(Preamble, CombLen, X) :-
  length(CombIs, CombLen),
  length(Preamble, L),
  once((
    maplist(between(1, L), CombIs),
    is_set(CombIs),
    maplist({Preamble}/[I,O]>>nth1(I, Preamble, O), CombIs, CombXs),
    sum_list(CombXs, X)
  )).

find_invalids(Data, PreambleLen, CombLen, Res) :-
  length(Preamble, PreambleLen),
  append(Preamble, _, Data),
  nth0(PreambleLen, Data, X),
  \+ valid(Preamble, CombLen, X),
   Res is X.
find_invalids([_|T], PreambleLen, CombLen, Res) :-
  find_invalids(T, PreambleLen, CombLen, Res).

find_paths([N|Tail], Acc, Sum, Res) :-
  sum_list(Acc, S),
  (
      S is Sum
  ->  Res = Acc
  ;   S > Sum
  ->  fail
  ;   append(Acc, [N], Acc0),
      find_paths(Tail, Acc0, Sum, Res)
  ).

find_paths(Data, Sum, Res) :-
  find_paths(Data, [], Sum, Res).
find_paths([_|Data], Sum, Res) :-
  find_paths(Data, Sum, Res).

part1(Data, Res) :- once(find_invalids(Data, 25, 2, Res)).
part2(Data, Res) :-
  part1(Data, Err),
  once(find_paths(Data, Err, Seq)),
  aggregate_all((min(X), max(X)), member(X, Seq), (Min, Max)),
  Res is Min+Max.

?- retractall(solution_from_data(_)).
solution_from_data(Data) :-
  part1(Data, One),
  format("Solution1: "), print1(One), nl,
  once(find_paths(Data, One, Seq)),
  aggregate_all((min(X), max(X)), member(X, Seq), (Min, Max)),
  Two is Min+Max,
  format("Solution2: "), print2(Two), nl.
