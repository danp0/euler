-module(reciprocal2).

-export(
   [
    find/1,
    find_cycle/1,
    main/0,
    max_cycle/2,
    split/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

find(N) ->
  find(1, N, []).

find(A, _B, Acc) when A =:= 0 ->
  lists:reverse(Acc);
find(A, B, Acc) when A < B ->
  find(10 * A, B, Acc);
find(A, B, Acc) ->
  Div = A div B,
  Rem = A rem B,
  if
    Rem =:= 0 ->
      find(Rem, B, [Div | Acc]);
    Rem =/= 0 ->
      case find_cycle([Div | Acc]) of
        false ->
          find(Rem, B, [Div | Acc]);
        Cycle ->
          {lists:reverse([Div | Acc]), lists:reverse(Cycle)}
      end
  end.

find_cycle(L) when length(L) =< 1 ->
  false;
find_cycle(L) ->
  Sublists = split(L),
  if
    length(Sublists) =< 1 ->
      false;
    true ->
      Cycle =
      lists:dropwhile(
        fun({L1, L2}) -> not lists:prefix(L1, L2) end,
        lists:foldl(
          fun(N, Acc) ->
              {L1, L2} = lists:split(N, Sublists),
              [{lists:flatten(L1), lists:flatten(L2)} | Acc]
          end,
          [],
          lists:seq(1,length(Sublists) - 1))),
      case Cycle of
        [] ->
          false;
        [{L1, _L2} | _T] ->
          L1
      end
  end.

main() ->
  io:format("~p~n", [max_cycle(2, 999)]),
  lists:foreach(
    fun
      ({N, {Q,C} = R}) when length(Q) div 2 > length(C) -> 
        io:format("~p: ~p: ~p~n", [N, R, length(C)]);
      (_) ->
        skip
    end, 
    [{N, find(N)} || N <- lists:seq(2,999)]),
  ok.

max_cycle(From, To) ->
  Reciprocals = [{N, find(N)} || N <- lists:seq(From, To)],
  lists:foldl(
    fun
      ({N, {_, Cycle}}, {NMax, CycleMax}) ->
        case length(Cycle) > length(CycleMax) of
          true ->
            {N, Cycle};
          false ->
            {NMax, CycleMax}
        end;
      (_, Acc) ->
        Acc
    end,
    {0, []},
    Reciprocals).

split([]) ->
  [[]];
split(L) ->
  split(hd(L), L, []).

split(_H, [], Acc) ->
  lists:map(fun(L) -> lists:reverse(L) end, lists:reverse(Acc));
split(H, [H|T], Acc) ->
  split(H, T, [[H] | Acc]);
split(H, [E|T], [L | Acc]) ->
  split(H, T, [[E|L] | Acc]).

%%
%% unit tests
%%
find_test_() ->
  [
   ?_assertEqual([5], find(2)),
   ?_assertEqual({[3,3],[3]}, find(3)),
   ?_assertEqual([2,5], find(4)),
   ?_assertEqual([2], find(5)),
   ?_assertEqual({[1,6,6], [6]}, find(6))
  ].

find_cycle_test_() ->
  [
   ?_assertEqual([3], find_cycle([3,3])),
   ?_assertEqual([1,2,3], find_cycle([1,2,3,1,2,3,1])),
   ?_assertEqual(false, find_cycle([1,2,3,1,2]))
  ].

split_test_() ->
  [
   ?_assertEqual([[]], split([])),
   ?_assertEqual([[1]], split([1])),
   ?_assertEqual([[1,2],[1,2,3], [1,2]], split([1,2,1,2,3,1,2]))
  ].
