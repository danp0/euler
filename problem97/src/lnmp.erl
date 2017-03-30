-module(lnmp).

-export(
   [
    main/0,
    pow/2,
    pow2/1
   ]
  ).

main() ->
  Pow10 = pow(10, 10),
  Prime = (28433 * (pow2(7830457) rem Pow10) + 1) rem Pow10,
  io:format("~p~n", [Prime]),
  ok.

pow(N, M) ->
  pow(N, M, 1).

pow(_N, 0, Pow) ->
  Pow;
pow(N, M, Pow) when M rem 2 =:= 1 ->
  pow(N, M-1, N*Pow);
pow(N, M, Pow) ->
  Pow2 = pow(N, M div 2, 1),
  pow(N, 0, Pow2 * Pow2 * Pow).

pow2(M) ->
  1 bsl M.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

pow_test_() ->
  [
   ?_assertEqual(1, pow(2, 0)),
   ?_assertEqual(2, pow(2, 1)),
   ?_assertEqual(4, pow(2, 2)),
   ?_assertEqual(8, pow(2, 3)),
   ?_assertEqual(16, pow(2, 4)),
   ?_assertEqual(32, pow(2, 5)),
   ?_assertEqual(64, pow(2, 6)),
   ?_assertEqual(128, pow(2, 7))
  ].

pow2_test_() ->
  Seq = lists:seq(0, 10),
  [
   ?_assertEqual([pow2(M) || M <- Seq], [pow(2, M) || M <- Seq])
  ].

-endif.
