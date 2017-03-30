-module(probability).

-export(
   [
    find/1,
    main/0
   ]
  ).

%%
%% (B)(B-1)/(N)(N-1) = 1/2
%% 2B(B-1) = N(N-1)
%% 2B^2 - 2B - N^2 + N = 0
%%
%% B(i+1) = 3B(i) + 2N(i) - 2
%% N(i+1) = 4B(i) + 3N(i) - 3
%%
%% B(0) = 3
%% N(0) = 4
%%

find(After) ->
  find(3, 4, After).

find(B, N, After) when N > After ->
  {B, N};
find(B, N, After) ->
  find(3*B + 2*N - 2, 4*B + 3*N - 3, After).

main() ->
  io:format("~p~n", [find(1000000000000)]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_test_() ->
  [
   ?_assertEqual({15,21}, find(5)),
   ?_assertEqual({85,120}, find(21)),
   ?_assertEqual({493,697}, find(120))
  ].

-endif.
