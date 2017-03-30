-module(fibonacci).

-export(
   [
    gen_has_n_digits/1,
    main/0,
    power_of_10/1,
    until_nth/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

gen_has_n_digits(Digits) ->
  Upper = power_of_10(Digits),
  Lower = Upper div 10,
  fun(Number) ->
      (Number >= Lower andalso Number < Upper) orelse
      (Number =:= 0 andalso Digits =:= 1)
  end.

main() ->
  io:format("fibonacci...~n", []),
  Nth = until_nth(gen_has_n_digits(1000)),
  io:format("~p~n", [Nth]).

power_of_10(0) ->
  1;
power_of_10(N) when N > 0 ->
  10 * power_of_10(N-1).
  
until_nth(Function) ->
  until_nth(Function, 1, 1, 1).

until_nth(Function, Fn1, Fn2, Nth) ->
  case Function(Fn1) of
    true ->
      Nth;
    false ->
      until_nth(Function, Fn2, Fn1+Fn2, Nth+1)
  end.

%%
%% unit tests
%%
gen_has_n_digits_test_() ->
  Has_1_digit  = gen_has_n_digits(1),
  Has_2_digits = gen_has_n_digits(2),
  Has_3_digits = gen_has_n_digits(3),
  [
   ?_assert(Has_1_digit(0)),
   ?_assert(Has_1_digit(1)),
   ?_assert(Has_1_digit(9)),
   ?_assertNot(Has_1_digit(10)),
   ?_assert(Has_2_digits(10)),
   ?_assert(Has_2_digits(99)),
   ?_assertNot(Has_2_digits(100)),
   ?_assert(Has_3_digits(100)),
   ?_assert(Has_3_digits(999)),
   ?_assertNot(Has_3_digits(1000))
  ].

power_of_10_test_() ->
  [
   ?_assertEqual(1, power_of_10(0)),
   ?_assertEqual(10, power_of_10(1)),
   ?_assertEqual(100, power_of_10(2)),
   ?_assertEqual(1000, power_of_10(3)),
   ?_assertEqual(10*power_of_10(999), power_of_10(1000))
  ].

until_nth_test_() ->
  Has_1_digit  = gen_has_n_digits(1),
  Has_2_digits = gen_has_n_digits(2),
  Has_3_digits = gen_has_n_digits(3),
  [
   ?_assertEqual(1, until_nth(Has_1_digit)),
   ?_assertEqual(7, until_nth(Has_2_digits)),
   ?_assertEqual(12, until_nth(Has_3_digits))
  ].

