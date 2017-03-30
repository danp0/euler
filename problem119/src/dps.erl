
-module(dps).

-export(
   [
    main/0,
    power/2,
    power_sums/2,
    sum_digits/1
   ]
  ).

main() ->
  PS = power_sums(200, 200),
  io:format("~p~n", [PS]),
  io:format("~p~n", [lists:nth(30, PS)]),
  ok.

power(_A,0) ->
  1;
power(A,1) ->
  A;
power(A,N) when N rem 2 =:= 1 ->
  A * power(A, N-1);
power(A,N) ->
  P2 = power(A, N div 2),
  P2 * P2.

power_sums(AMax, NMax) ->
  lists:sort(
    [
     {Power, A, N} ||
     A <- lists:seq(2, AMax),
     N <- lists:seq(1, NMax),
     Power <- [power(A,N)],
     Sum <- [sum_digits(Power)],
     Power >= 10,
     Sum =:= A
    ]).

sum_digits(N) when is_integer(N) ->
  sum_digits(integer_to_list(N));
sum_digits(N) when is_list(N) ->
  lists:sum(lists:map(fun(D) -> list_to_integer([D]) end, N)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

power_test_() ->
  [
   ?_assertEqual([1,2,4,8,16,32,64,128,256], 
                 [power(2,I) || I <- lists:seq(0,8)])
  ].

power_sums_test_() ->
  PS = power_sums(50, 9),
  [
   ?_assertEqual(81,        element(1, lists:nth(1, PS))),
   ?_assertEqual(512,       element(1, lists:nth(2, PS))),
   ?_assertEqual(2401,      element(1, lists:nth(3, PS))),
   ?_assertEqual(4913,      element(1, lists:nth(4, PS))),
   ?_assertEqual(5832,      element(1, lists:nth(5, PS))),
   ?_assertEqual(17576,     element(1, lists:nth(6, PS))),
   ?_assertEqual(19683,     element(1, lists:nth(7, PS))),
   ?_assertEqual(234256,    element(1, lists:nth(8, PS))),
   ?_assertEqual(390625,    element(1, lists:nth(9, PS))),
   ?_assertEqual(614656,    element(1, lists:nth(10, PS))),
   ?_assertEqual(1679616,   element(1, lists:nth(11, PS))),
   ?_assertEqual(17210368,  element(1, lists:nth(12, PS))),
   ?_assertEqual(34012224,  element(1, lists:nth(13, PS))),
   ?_assertEqual(52521875,  element(1, lists:nth(14, PS))),
   ?_assertEqual(60466176,  element(1, lists:nth(15, PS))),
   ?_assertEqual(205962976, element(1, lists:nth(16, PS))),
   ?_assertEqual(612220032, element(1, lists:nth(17, PS)))
  ].

sum_digits_test_() ->
  [
   ?_assertEqual(
      [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,10,2],
      [sum_digits(I) || I <- lists:seq(0, 20)]),
   ?_assertEqual(45, sum_digits("123456789"))
  ].

-endif.
