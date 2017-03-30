-module(bouncy).

-export(
   [
    bouncy/1,
    find/1,
    start/0
   ]
  ).

bouncy(N) when is_integer(N) ->
  bouncy(integer_to_list(N));
bouncy(N) when length(N) =< 2 ->
  false;
bouncy([H1, H2 | T]) when H1 =:= H2 ->
  bouncy([H2 | T]);
bouncy([H1, H2 | T]) ->
  case H1 < H2 of
    true ->
      not increasing(H2, T);
    false ->
      not decreasing(H2, T)
  end.

decreasing(_H1, []) ->
  true;
decreasing(H1, [H2 | T]) ->
  case H1 >= H2 of
    true ->
      decreasing(H2, T);
    false ->
      false
  end.

increasing(_H1, []) ->
  true;
increasing(H1, [H2 | T]) ->
  case H1 =< H2 of
    true ->
      increasing(H2, T);
    false ->
      false
  end.

find(Percentage) ->
  find(Percentage, 100, 0, 99).

find(Percentage, N, Count, Total) ->
  {Count1, Total1} =
  case bouncy(N) of
    true ->
      {Count + 1, Total + 1};
    false ->
      {Count, Total + 1}
  end,
  case Count1 / Total1 >= Percentage of
    true ->
      N;
    false ->
      find(Percentage, N+1, Count1, Total1)
  end.

start() ->
  io:format("bouncy...~n", []),
  io:format("~p~n", [find(0.99)]),
  ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

bouncy_test_() ->
  [
   ?_assertNot(bouncy([])),
   ?_assertNot(bouncy(1)),
   ?_assertNot(bouncy(89)),
   ?_assertNot(bouncy(98)),
   ?_assertNot(bouncy(99)),
   ?_assertNot(bouncy(122)),
   ?_assertNot(bouncy(123)),
   ?_assertNot(bouncy(321)),
   ?_assertNot(bouncy(322)),
   ?_assertNot(bouncy(332)),
   ?_assertNot(bouncy(335)),
   ?_assert(bouncy(323)),
   ?_assert(bouncy(121)),
   ?_assert(bouncy(12354))
  ].

find_test_() ->
  [
   ?_assertEqual(538, find(0.5)),
   ?_assertEqual(21780, find(0.9))
  ].

-endif.
