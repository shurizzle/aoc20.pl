:- [prelude].

input_file("inputs/4.txt").

tinput("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in").

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

parse(Groups) --> blanks, groups(Groups), blanks, eos.

blanknonl --> [L], { \+ code_type(L, newline), code_type(L, space) }.
blanknonls --> blanknonl, blanknonls.
blanknonls --> [].
newline --> [L], { code_type(L, newline) }.
breaker --> blanknonls, newline, blanknonls, newline, blanknonls.
spacer --> blanknonls, newline, !, blanknonls.
spacer --> blanknonls.

groups([G|Gs]) --> group(G), !, groups0(Gs).
groups([]) --> [].

groups0([G|Gs]) --> breaker, group(G), !, groups0(Gs).
groups0([]) --> [].

group(G) --> group0(Es), { dict_create(G, #, Es) }.

group0([E|Es]) --> element(E), !, spacer, group1(Es).
group0([]) --> [].

group1([E|Es]) --> spacer, element(E), !, group1(Es).
group1([]) --> [].

element(K:V) --> key(K), ":", value(V).

key(Name, Codes, Rest) :-
  key(Name),
  atom_codes(Name, Cs),
  append(Cs, Rest, Codes).

value(V) --> nonblanks(V0), { string_chars(V, V0) }.

key(cid).
key(Name) :- minkey(Name).

minkey(byr).
minkey(iyr).
minkey(eyr).
minkey(hgt).
minkey(hcl).
minkey(ecl).
minkey(pid).

int(C) :-
  between(0'0, 0'9, C).

hex(C) :-
  int(C);
  between(0'a, 0'z, C);
  between(0'A, 0'Z, C).

eye_color(amb).
eye_color(blu).
eye_color(brn).
eye_color(gry).
eye_color(grn).
eye_color(hzl).
eye_color(oth).

valid_field(byr, Value) :-
  string_length(Value, 4),
  number_string(N, Value),
  between(1920, 2002, N).
valid_field(iyr, Value) :-
  string_length(Value, 4),
  number_string(N, Value),
  between(2010, 2020, N).
valid_field(eyr, Value) :-
  string_length(Value, 4),
  number_string(N, Value),
  between(2020, 2030, N).
valid_field(hgt, Value) :-
  (
    string_concat(V0, "cm", Value),
    number_string(N, V0),
    between(150, 193, N)
  ; string_concat(V0, "in", Value),
    number_string(N, V0),
    between(59, 76, N)
  ).
valid_field(hcl, Value) :-
  string_length(Value, 7),
  string_concat("#", HS, Value),
  string_codes(HS, Hs),
  maplist(hex, Hs).
valid_field(ecl, Value) :-
  atom_string(V, Value),
  eye_color(V).
valid_field(pid, Value) :-
  string_length(Value, 9),
  string_codes(Value, Xs),
  maplist(int, Xs).

valid1(Passport) :- forall(minkey(Name), get_dict(Name, Passport, _)).
valid2(Passport) :-
  forall(
    minkey(Name),
    (
      get_dict(Name, Passport, Value)
    , valid_field(Name, Value)
    )).

part1(Data, Res) :- filtercount(valid1, Data, Res).
part2(Data, Res) :- filtercount(valid2, Data, Res).
