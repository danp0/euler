-module(power).

-export(
   [
    find/0,
    find/1,
    main/0,
    n_digit_pow_compare/2,
    pow/2
   ]
  ).

find() ->
  find(1, []).

find(N, Acc) ->
  case find(N) of
    [] ->
      lists:usort(lists:flatten(Acc));
    NPowers ->
      find(N+1, [NPowers|Acc])
  end.

find(N) ->
  find(1, N, []).

find(M, N, Acc) ->
  case n_digit_pow_compare(M, N) of
    -1 ->
      find(M+1, N, Acc);
    0 ->
      find(M+1, N, [{M,N}|Acc]);
    1 ->
      lists:reverse(Acc)
  end.

main() ->
  NPowers = find(),
  io:format("~p~n~p~n", [NPowers, length(NPowers)]),
  ok.

n_digit_pow_compare(M, N) ->
  Power = pow(M, N),
  Length = length(integer_to_list(Power)),
  if
    Length < N ->
      -1;
    Length > N ->
      1;
    true ->
      0
  end.

pow(M, N) when N >= 0 ->
  pow(M, N, 1).

pow(_M, 0, Power) ->
  Power;
pow(M, N, Power) when N rem 2 =:= 1 ->
  pow(M, N-1, M*Power);
pow(M, N, Power) ->
  M2 = pow(M, N div 2, 1),
  pow(M, 0, M2 * M2 * Power).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

n_digit_pow_compare_test_() ->
  [
   ?_assertEqual(
      [0,0,0,0,0,0,0,0,0,1], 
      [n_digit_pow_compare(M,1) || M <- lists:seq(1,10)]),
   ?_assertEqual(
      [-1,0,0,0,0,0,0,1],
      [n_digit_pow_compare(M,2) || M <- lists:seq(3,10)]),
   ?_assertEqual(
      [-1,0,0,0,0,0,1],
      [n_digit_pow_compare(M,3) || M <- lists:seq(4,10)]),
   ?_assertEqual(0, n_digit_pow_compare(7,5)),
   ?_assertEqual(0, n_digit_pow_compare(8,9))
  ].

pow_test_() ->
  [
   ?_assertEqual(1, pow(1,0)),
   ?_assertEqual(1, pow(2,0)),
   ?_assertEqual(2, pow(2,1)),
   ?_assertEqual(4, pow(2,2)),
   ?_assertEqual(8, pow(2,3)),
   ?_assertEqual(16, pow(2,4)),
   ?_assertEqual(32, pow(2,5))
  ].

-endif.

