-module(factorial).

-export(
   [
    digits/1,
    factorial/1,
    factorial_digit_sum/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

digits(N) ->
  digits(N, []).

digits(0, Acc) ->
  Acc;
digits(N, Acc) ->
  digits(N div 10, [N rem 10 | Acc]).

factorial(0) ->
  1;
factorial(1) ->
  1;
factorial(N) when N >= 2 ->
  N * factorial(N-1).

factorial_digit_sum(N) ->
  lists:sum(digits(factorial(N))).

main() ->
  io:format("digit sum: ~p~n", [factorial_digit_sum(100)]).

%%
%% unit tests
%%
digits_test_() ->
  [
   ?_assertEqual([1], digits(1)),
   ?_assertEqual([1,2], digits(12)),
   ?_assertEqual([3,6,2,8,8,0,0], digits(3628800))
  ].

factorial_test_() ->
  [
   ?_assertEqual(1, factorial(0)),
   ?_assertEqual(1, factorial(1)),
   ?_assertEqual(2, factorial(2)),
   ?_assertEqual(6, factorial(3)),
   ?_assertEqual(24, factorial(4))
  ].

