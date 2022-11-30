:- [prelude].

input_file("inputs/7.txt").

% tests {{{
tinput("shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.").

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
  list_of(KV, bag(KV), ", ", KVs), ".",
  { length(KVs, L), L > 0,
    dict_create(Res, #, KVs) }.

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

contains0(_, Res, Res).
contains0(Data, Search, Res) :-
  get_dict(K, Data, Vs),
  get_dict(Search, Vs, _),
  contains0(Data, K, Res).
contains(Data, Search, Res) :-
  aggregate_all(set(R), (
    get_dict(K, Data, Vs),
    get_dict(Search, Vs, _),
    contains0(Data, K, R)
  ), Res0),
  length(Res0, Res).

count_bags(Data, Search, Res) :-
  get_dict(Search, Data, Bags), !,
  dict_pairs(Bags, _, Pairs),
  aggregate_all(sum(N), (
    member(K-V, Pairs),
    count_bags(Data, K, Sum),
    N is Sum*V+V
  ), Res).
count_bags(_, _, 0).

part1(Data, Res) :- contains(Data, 'shiny gold', Res).
part2(Data, Res) :- count_bags(Data, 'shiny gold', Res).
