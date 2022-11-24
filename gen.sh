#!/usr/bin/env bash

BASE="$(dirname "$(readlink -f "$0")")"
INPUT="$BASE/inputs/$1.txt"
FILE="$BASE/src/$1.pl"

if ! test -f "$INPUT"; then
  touch "$INPUT"
fi

if ! test -f "$FILE"; then
  cat <<EOF > "$FILE"
:- [prelude].

input_file("inputs/$1.txt").

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

print1(Res) :- print(Res).
print2(Res) :- print(Res).

parse(Lines) --> parse_lines(Lines).
line(I) --> var(I), !.

part1(Data, Res) :- nonvar(Data), var(Res).
part2(Data, Res) :- nonvar(Data), var(Res).
EOF
fi
