:- [prelude].

input_file("inputs/6.txt").

% tests {{{
tinput("abc

a
b
c

ab
ac

a
a
a
a

b").

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

parse(R) --> parse(R, []), blanks.
parse(R, Gs0) -->
  group(G), !,
  { append(Gs0, [G], Gs) },
  parse0(R, Gs).

parse0(R, Gs0) -->
  blanknonls, newline, blanknonls,
  blanknonls, newline, blanknonls,
  group(G), !,
  { append(Gs0, [G], Gs) },
  parse0(R, Gs).
parse0(R, R) --> [].

blanknonl --> [L], { \+ code_type(L, newline), code_type(L, space) }.
blanknonls --> blanknonl, blanknonls.
blanknonls --> [].
newline --> [L], { code_type(L, newline) }.

char(C) -->
  [C0],
  { code_type(C0, alpha),
    char_code(C1, C0),
    downcase_atom(C1, C) }.

person(Cs) --> person(Cs, []).
person(Res, Cs0) -->
  blanknonls, char(C), !,
  { append(Cs0, [C], Cs) },
  person0(Res, Cs).
person0(Res, Cs0) --> person(Res, Cs0), !.
person0(Cs, Cs) --> [].

group(Res) -->
  person(P), group0(Res, [P]).

group0(Res, Ps0) -->
  blanknonls, newline, blanknonls,
  person(P), !,
  { append(Ps0, [P], Ps) },
  group0(Res, Ps).
group0(Ps, Ps) --> [].

part1(Data, Res) :-
  aggregate_all(sum(X),
    (
      member(Xs0, Data),
      flatten(Xs0, Xs1),
      list_to_ord_set(Xs1, Xs2),
      length(Xs2, X)
    ), Res).
part2(Data, Res) :-
  aggregate_all(sum(X),
    (
      member(Xs0, Data),
      maplist(list_to_ord_set, Xs0, Xs1),
      foldl1(intersection, Xs1, Xs2),
      length(Xs2, X)
    ), Res).
