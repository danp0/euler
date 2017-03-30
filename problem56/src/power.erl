-module(power).

-export(
   [
    main/0,
    pow/2,
    sum_digits/1
   ]
  ).

main() ->
  Powers = 
  [sum_digits(pow(A,B)) || A <- lists:seq(1,99), B <- lists:seq(1,99)],
  io:format("~p~n", [lists:max(Powers)]),
  ok.

pow(A,B) when B >= 0 ->
  pow(A, B, 1).

pow(_A, 0, Power) ->
  Power;
pow(A, 1, Power) ->
  pow(A, 0, A * Power);
pow(A, B, Power) when B rem 2 =:= 1 ->
  pow(A, B - 1, A * Power);
pow(A, B, Power) ->
  P2 = pow(A, B div 2),
  pow(A, 0, P2 * P2 * Power).

sum_digits(N) ->
  lists:sum([D - $0 || D <- integer_to_list(N)]).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pow_test_() ->
  [
   ?_assertEqual(
      [1,2,4,8,16,32,64,128,256,512,1024], 
      [pow(2,N) || N <- lists:seq(0,10)]),
   ?_assertEqual(
      [1,3,9,27,81,243],
      [pow(3,N) || N <- lists:seq(0,5)]),
   ?_assertEqual(
      [1,5,25,125,625],
      [pow(5,N) || N <- lists:seq(0,4)])
  ].

sum_digits_test_() ->
  [
   ?_assertEqual(1, sum_digits(1)),
   ?_assertEqual(3, sum_digits(12)),
   ?_assertEqual(6, sum_digits(123)),
   ?_assertEqual(10, sum_digits(1234)),
   ?_assertEqual(15, sum_digits(12345))
  ].

-endif.
