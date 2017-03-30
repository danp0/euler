-module(factorial).

-export(
   [
    digit_seq/2,
    digits/1,
    factorial/1,
    factorial_sum/1,
    find/0,
    is_factorial_sum/2,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

digit_seq(1, Digits) ->
  [[N] || N <- Digits];
digit_seq(N, Digits) ->
  [[E|P] || E <- Digits, P <- digit_seq(N-1, lists:seq(E,9))].

digits(0) ->
  [0];
digits(N) ->
  digits(N, []).

digits(0, Acc) ->
  Acc;
digits(N, Acc) ->
  digits(N div 10, [N rem 10|Acc]).

factorial(0) ->
  1;
factorial(N) ->
  N * factorial(N-1).

factorial_sum(L) ->
  lists:foldl(fun(N, Sum) -> Sum + factorial(N) end, 0, L).

is_factorial_sum(Digits, SumFactorial) ->
  lists:sort(Digits) =:= lists:sort(digits(SumFactorial)).

find(N) ->
  Sums = [{D, factorial_sum(D)} || D <- digit_seq(N, lists:seq(0,9))],
  lists:filter(fun({Digits, Sum}) -> is_factorial_sum(Digits, Sum) end, Sums).

find() ->
  lists:merge(
    [find(N) || N <- lists:seq(2,7)]).

main() ->
  Sums = find(),
  Sum = lists:sum(lists:map(fun({_D, S}) -> S end, Sums)),
  io:format("~w~n~p~n", [Sums, Sum]),
  ok.

%%
%% unit tests
%%
digits_test_() ->
  [
   ?_assertEqual([0], digits(0)),
   ?_assertEqual([1], digits(1)),
   ?_assertEqual([1,2], digits(12)),
   ?_assertEqual([1,2,3], digits(123))
  ].

factorial_test_() ->
  [
   ?_assertEqual(1, factorial(0)),
   ?_assertEqual(1, factorial(1)),
   ?_assertEqual(2, factorial(2)),
   ?_assertEqual(6, factorial(3)),
   ?_assertEqual(24, factorial(4)),
   ?_assertEqual(120, factorial(5))
  ].

factorial_sum_test_() ->
  [
   ?_assertEqual(1, factorial_sum([0])),
   ?_assertEqual(2, factorial_sum([0,0])),
   ?_assertEqual(9, factorial_sum([1,2,3]))
  ].

is_factorial_sum_test_() ->
  [
   ?_assert(is_factorial_sum([1,4,5], factorial_sum([1,4,5])))
  ].
