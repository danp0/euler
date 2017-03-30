-module(powers).

-export(
   [
    main/0,
    n_to_nth/1
   ]
  ).

main() ->
  Sum = lists:sum(
          lists:map(fun(N) -> n_to_nth(N) end,
                    lists:seq(1,1000))),
  io:format("~p~n", [Sum rem n_to_nth(10)]),
  ok.

n_to_nth(N) ->
  n_to_nth(1, N, N).

n_to_nth(N, N, Power) ->
  Power;
n_to_nth(I, N, Power) ->
  n_to_nth(I+1, N, N * Power).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

n_to_nth_test_() ->
  [
   ?_assertEqual(1, n_to_nth(1)),
   ?_assertEqual(2*2, n_to_nth(2)),
   ?_assertEqual(3*3*3, n_to_nth(3))
  ].

-endif.
