-module(collatz).

-export(
   [
    find/1,
    find_parallel/1,
    seq/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

find(List) ->
  lists:foldl(
    fun(N, {Root, Length}) ->
        NewLength = length(seq(N)),
        case NewLength > Length of
          true ->
            {N, NewLength};
          false ->
            {Root, Length}
        end
    end,
    {0,0},
    List).

find_parallel(List) ->
  lists:foreach(
    fun(N) ->
        Pid = spawn(
                fun() -> 
                    receive
                      {Sender, Root} ->
                        Sender ! {Root, length(seq(Root))}
                    end
                end),
        Pid ! {self(), N}
    end,
    List),
  lists:foldl(
    fun(_N, {Root, Length}) ->
        receive
          {NewRoot, NewLength} ->
            case NewLength > Length of
              true ->
                {NewRoot, NewLength};
              false ->
                {Root, Length}
            end
        end
    end,
    {0,0},
    lists:seq(1, length(List))).

main() ->
  {Duration, {Root, Longest}} = timer:tc(fun() -> find(lists:seq(999999, 500001, -2)) end),
  io:format("~p is ~p in ~p microseconds~n", [Root, Longest, Duration]),
  {Duration2, {Root2, Longest2}} = timer:tc(fun() -> find_parallel(lists:seq(999999, 500001, -2)) end),
  io:format("~p is ~p in ~p microseconds~n", [Root2, Longest2, Duration2]).

seq(N) ->
  seq(N, []).

seq(1, Acc) ->
  lists:reverse([1 | Acc]);
seq(N, Acc) when N > 1 ->
  Next =
  case N rem 2 of
    0 ->
      N div 2;
    _ ->
      3 * N + 1
  end,
  seq(Next, [N | Acc]).

%%
%% tests
%%
seq_test_() ->
  [
   ?_assertEqual([13, 40, 20, 10, 5, 16, 8, 4, 2, 1], seq(13))
  ].
