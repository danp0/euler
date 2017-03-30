-module(blocks0).

-export(
   [
    count/1,
    count_blocks/1,
    find_blocks/1,
    get_blocks/1,
    init/0,
    main/0,
    pow/2,
    start/0,
    stop/0,
    to_binary/2
   ]
  ).

-record(
   state,
   {
    counts = gb_trees:empty()
   }).

%%
%% blocks...
%% 1: 1
%% 2: 1
%% 3: 2        1 +    1
%% 4: 4        1 +    2 + 1
%% 5: 7        2 +    4 + 1
%% 6: 11       4 +    7
%% 7: 17       7 +   11 - 1
%% 8: 27      11 +   17 - 1
%% 9: 44      17 +   27
%% 10: 72     27 +   44 + 1
%% 11: 117    44 +   72 + 1
%% 12: 189    72 +  117
%% 13: 305   117 +  189 - 1
%% 14: 493   189 +  305 - 1
%% 15: 798   305 +  493
%% 16: 1292  493 +  798 + 1
%% 17: 2091  798 + 1292 + 1
%% 18: 3383 1292 + 2091
%% 19: 5473 2091 + 3383 - 1
%% 20: 8855 3383 + 5473 - 1
%%
%% C(1) = 1
%% C(2) = 1
%% C(n) = C(n-1) + C(n-2) (-1 when n rem 6 == 1 or 2 or +1 when n rem 6 == 4 or 5)
%%

count(1) ->
  1;
count(2) ->
  1;
count(N) ->
  lookup(N-1) + lookup(N-2) + lists:nth((N rem 6) + 1, [0,-1,-1,0,1,1]).

count_blocks(Length) ->
  count_blocks(0, pow(2, Length) - 1, Length, 0).

count_blocks(From, To, _Length, Total) when From > To ->
  Total;
count_blocks(From, To, Length, Total) ->
  B = get_blocks(to_binary(From, Length)),
  Ok =
  case (B =:= [] orelse length(hd(B)) >= 3) of
    true ->
      1;
    false ->
      0
  end,
  count_blocks(From+1, To, Length, Total + Ok).

find_blocks(Length) ->
  Blocks =
  lists:map(
    fun(B) ->
        get_blocks(B)
    end,
    [to_binary(I, Length) || I <- lists:seq(0, pow(2, Length) - 1)]),
  lists:filter(
    fun(B) ->
        B =:= [] orelse
        length(hd(B)) >= 3
    end,
    Blocks).

get_blocks(Binary) ->
  Blocks =
  lists:sort(re:split(Binary, "1+", [{return, list}, trim])),
  case Blocks of
    [[]|T] ->
      T;
    Blocks ->
      Blocks
  end.

init() ->
  loop(#state{}).

lookup(N) ->
  case send({lookup, N}) of
    none ->
      Count = count(N),
      send({enter, N, Count}),
      Count;
    {value, Value} ->
      Value
  end.

loop(State) ->
  NewState =
  receive
    {From, {enter, N, Count}} ->
      From ! ok,
      State#state{counts = gb_trees:enter(N, Count, State#state.counts)};
    {From, {lookup, N}} ->
      From ! gb_trees:lookup(N, State#state.counts),
      State;
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("50: ~p~n", [count(50)]),
  ok = stop(),
  ok.

pow(_A, 0) ->
  1;
pow(A, 1) ->
  A;
pow(A, B) when B rem 2 =:= 1 ->
  A * pow(A, B-1);
pow(A, B) ->
  Power = pow(A, B div 2),
  Power * Power.

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  send(stop).

to_binary(I, Width) ->
  lists:flatten(io_lib:format("~*.2.0B", [Width, I])).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

count_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(
       [1,1,2,4,7,11,17,27,44,72], [count(I) || I <- lists:seq(1,10)])
   ]
  }.

count_blocks_test_() ->
  [
    ?_assertEqual(
       [1,1,2,4,7,11,17,27,44,72], [count_blocks(I) || I <- lists:seq(1,10)])
  ].
    
pow_test_() ->
  [
   ?_assertEqual(
      [1, 2, 4, 8, 16, 32, 64, 128, 256],
      [pow(2, N) || N <- lists:seq(0,8)])
  ].

to_binary_test_() ->
  [
   ?_assertEqual(
      ["000", "001", "010", "011", "100", "101", "110", "111"],
      [to_binary(I, 3) || I <- lists:seq(0,7)])
  ].

-endif.
