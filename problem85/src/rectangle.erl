-module(rectangle).

-export(
   [
    count/2,
    main/0,
    m_of_n/1
   ]
  ).

%%
%% mxn: m(m+1)n(n+1)/4
%%

count(M, N) ->
  M * (M+1) * N * (N+1) div 4.

main() ->
  {Count, M, N} =
  lists:max(
    lists:map(
      fun(N) ->
          M = m_of_n(N),
          {count(M, N), M, N}
      end,
      lists:seq(1, 1999))),
  io:format("~px~p: ~p~n", [M, N, Count]),
  io:format("area: ~p~n", [M*N]),
  ok.

m_of_n(N) ->
  trunc((-1 + math:sqrt(1 + 32000000 / (N * (N+1))))/2).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

count_test_() ->
  [
   ?_assertEqual(
      [1, 3, 9, 6, 18, 36, 10, 30, 60, 100],
      [count(M,N) || N <- lists:seq(1,4), M <- lists:seq(1,N)])
  ].

-endif.
