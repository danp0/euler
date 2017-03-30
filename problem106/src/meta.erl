-module(meta).

-export(
   [
    catalan/1,
    choose/2,
    comparisons/1,
    main/0,
    subset_pairs/1,
    subset_pairs/2
   ]
  ).

catalan(N) ->
  choose(2*N, N) div (N + 1).

choose(_N, 0) ->
  1;
choose(N, N) ->
  1;
choose(N, K) ->
  choose(N-1, K-1) + choose(N-1, K).

comparisons(N) ->
  lists:sum(
    lists:map(
      fun(K) ->
          subset_pairs(N,K) - catalan(K) * choose(N, 2*K)
      end,
      lists:seq(2, N div 2))).

main() ->
  io:format("~p~n", [comparisons(12)]),
  ok.

subset_pairs(N) ->
  (trunc(math:pow(3, N)) - trunc(math:pow(2, N+1)) + 1) div 2.

subset_pairs(N, K) ->
  (choose(N, K) * choose(N - K, K)) div 2.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

catalan_test_() ->
  [
   ?_assertEqual(1, catalan(0)),
   ?_assertEqual(1, catalan(1)),
   ?_assertEqual(2, catalan(2)),
   ?_assertEqual(5, catalan(3)),
   ?_assertEqual(14, catalan(4)),
   ?_assertEqual(42, catalan(5)),
   ?_assertEqual(132, catalan(6))
  ].

choose_test_() ->
  [
   ?_assertEqual(1, choose(1,0)),
   ?_assertEqual(1, choose(2,0)),
   ?_assertEqual(1, choose(1,1)),
   ?_assertEqual(1, choose(2,2)),
   ?_assertEqual(5, choose(5,4)),
   ?_assertEqual(10, choose(5,3)),
   ?_assertEqual(10, choose(5,2))
  ].

comparisons_test_() ->
  [
   ?_assertEqual(1, comparisons(4)),
   ?_assertEqual(5, comparisons(5)),
   ?_assertEqual(20, comparisons(6)),
   ?_assertEqual(70, comparisons(7))
  ].

subset_pairs_test_() ->
  [
   ?_assertEqual(25, subset_pairs(4)),
   ?_assertEqual(966, subset_pairs(7)),
   ?_assertEqual(3, subset_pairs(4,2))
  ].

-endif.

