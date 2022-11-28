:- [prelude].

input_file("inputs/7.txt").

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

parse(Bags) -->
  list_of(Bag, raw_line(line(Bag)), Bags0), !,
  blanks,
  { dict_create(Bags, #, Bags0) }.
line(Name:Content) --> string_ends_with(Name, " bags contain "), content(Content).

content(#{}) --> "no other bags.", !.
content(Res) -->
  content0(KVs, []),
  bag(KV), ".",
  { dict_create(Res, #, [KV|KVs]) }.

content0(Res, Acc) -->
  bag(KV), ", ", !, content0(Res, [KV|Acc]).
content0(Res, Res) --> [].

bag(Name:1) -->
  "1 ", string_ends_with(Name, " bag"), !.
bag(Name:N) -->
  integer(N), " ", string_ends_with(Name, " bags"),
  { N > 1 }.

string_ends_with(Str, End0, Codes, Rest) :-
  string(End0),
  string_codes(End0, End), !,
  string_ends_with(Str, End, Codes, Rest).
string_ends_with(Str, End) -->
  { length(End, L), L > 0 },
  string_ends_with(Codes, [], End),
  { atom_codes(Str, Codes) }.

string_ends_with(Res, Res, End, Codes, Rest) :-
  append(End, Rest, Codes), !.
string_ends_with(_, _, _, [], _) :- !, fail.
string_ends_with(Res, Acc0, End, [C|Codes], Rest) :-
  append(Acc0, [C], Acc),
  string_ends_with(Res, Acc, End, Codes, Rest).

part1(Data, Res) :- nonvar(Data), var(Res).
part2(Data, Res) :- nonvar(Data), var(Res).
