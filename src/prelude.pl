% boilerplate {{{

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

:- dynamic default_input/1.
:- dynamic parse/3.
input_from_file(File, Input) :- phrase_from_file(parse(Input), File).
input_from_string(Codes, Input) :-
  is_list(Codes), !,
  phrase(parse(Input), Codes).
input_from_string(Str, Input) :-
  string(Str), !,
  string_codes(Str, Codes),
  input_from_string(Codes, Input).
input_from_string(Str, Input) :-
  atom(Str), !,
  atom_codes(Str, Codes),
  input_from_string(Codes, Input).
input(Input) :- default_input(Input), !.
input(Input) :-
  input_file(File),
  input_from_file(File, Input),
  asserta(default_input(Input)).

test_parse:- input(Input), print(Input), nl.
test_parse_from_file(File) :- input_from_file(File, Res), print(Res), nl.
test_parse_from_string(Str) :- input_from_string(Str, Res), print(Res), nl.

solve1_from_string(Str, Res) :- input_from_string(Str, Data), part1(Data, Res).
solve1_from_file(File, Res) :- input_from_file(File, Data), part1(Data, Res).
solve1(Res) :- input(Data), part1(Data, Res).

solve2_from_string(Str, Res) :- input_from_string(Str, Data), part2(Data, Res).
solve2_from_file(File, Res) :- input_from_file(File, Data), part2(Data, Res).
solve2(Res) :- input(Data), part2(Data, Res).

solution1_from_string(Str) :- solve1_from_string(Str, Res), print1(Res), nl.
solution1_from_file(File) :- solve1_from_file(File, Res), print1(Res), nl.
solution1 :- solve1(Res), print1(Res), nl.

solution2_from_string(Str) :- solve2_from_string(Str, Res), print2(Res), nl.
solution2_from_file(File) :- solve2_from_file(File, Res), print2(Res), nl.
solution2 :- solve2(Res), print2(Res), nl.

solution_from_string(Str) :-
  input_from_string(Str, Data),
  solution_from_data(Data).
solution_from_file(Str) :-
  input_from_string(Str, Data),
  solution_from_data(Data).
solution_from_data(Data) :-
  part1(Data, One),
  part2(Data, Two),
  format("Solution1: "), print1(One), nl,
  format("Solution2: "), print2(Two), nl.
solution :- input(Data), solution_from_data(Data).
% }}}

% parsing {{{
raw_line(Fn) --> call(Fn), eol.

trim_line(Fn) --> blanks, call(Fn), blanks, eos, !.
trim_line(Fn) --> blanks, call(Fn), blanks_to_nl.

list_of(_, _, []) --> eos, !.
list_of(Templ, Term0, [X|Xs], Codes, Rest) :-
  copy_term_nat(Templ, X),
  term_variables(Term0, Vars0),
  exclude(==(Templ), Vars0, Vars),
  copy_term(Templ^Vars^Term0, X^Vars^Term),

  call_dcg(Term, Codes, C0), !,
  list_of(Templ, Term0, Xs, C0, Rest).
list_of(_, _, []) --> [].

parse_string(IsValid, Cs) -->
  parse_string_character(IsValid, C0), !,
  parse_string(IsValid, Cs0),
  { string_concat(C0, Cs0, Cs) }.
parse_string(_, "") --> [].

parse_string_character(IsValid, C) -->
  [L],
  { call(IsValid, L), !,
    char_code(C, L)
  }.

letter(C) --> parse_string_character(is_alpha, C).
letters(Cs) --> parse_string(is_alpha, Cs).

is_alpha(C) :- code_type(C, alpha).
% }}}

% util {{{
minus(A,B,C) :-
     var(A)
  -> A is B+C
  ;  var(B)
  -> B is A-C
  ;  C is A-B.

times(A,B,C) :-
      var(A)
  ->  A is C/B
  ;   var(B)
  ->  B is C/A
  ;   C is A*B.

foldl1(Goal, [X|Xs], V) :-
  foldl(Goal, Xs, X, V).

mul_list(Xs, Res) :-
  foldl1(times, Xs, Res).

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
% }}}
