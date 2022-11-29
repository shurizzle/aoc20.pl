:- [prelude].

input_file("inputs/8.txt").

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

parse(Lines) --> list_of(L, trim_line(line(L)), Lines).
line(Name:I) -->
  inst(Name), whites, integer(I).

inst(acc) --> "acc".
inst(jmp) --> "jmp".
inst(nop) --> "nop".

step0(acc:N, (A0, PC0), (A, PC)) :- A is A0+N, succ(PC0, PC).
step0(jmp:N, (A, PC0), (A, PC)) :- PC is PC0+N.
step0(nop:_, (N, PC0), (N, PC)) :- succ(PC0, PC).

solve1(Program, State0, Ran0, Res) :-
  (PrevAcc, PC) = State0,
  nth0(PC, Program, Inst),
  step0(Inst, State0, State),
  put_dict(PC, Ran0, true, Ran),
  (
      (_, NewPC) = State,
      get_dict(NewPC, Ran, _)
  ->  Res is PrevAcc
  ;   solve1(Program, State, Ran, Res)
  ).
solve1(Program, Res) :-
  solve1(Program, (0, 0), #{}, Res).

step1(acc:N, S0, S, step1) :- step0(acc:N, S0, S).
step1(jmp:N, S0, S, step1) :- step0(jmp:N, S0, S).
step1(jmp:N, S0, S, step2) :- step0(nop:N, S0, S).
step1(nop:N, S0, S, step1) :- step0(nop:N, S0, S).
step1(nop:N, S0, S, step2) :- step0(jmp:N, S0, S).

step2(Inst, S0, S, step2) :- step0(Inst, S0, S).

solve2(Program, (Res, L), _, _, Res) :-
  length(Program, L), !.
solve2(Program, State0, Ran0, Step0, Res) :-
  (_, PC) = State0,
  nth0(PC, Program, Inst),
  call(Step0, Inst, State0, State, Step),
  put_dict(PC, Ran0, true, Ran),
  (_, NewPC) = State,
  \+ get_dict(NewPC, Ran, _),
  solve2(Program, State, Ran, Step, Res).
solve2(Program, Res) :-
  once(solve2(Program, (0, 0), #{}, step1, Res)).

part1(Data, Res) :- solve1(Data, Res).
part2(Data, Res) :- solve2(Data, Res).
