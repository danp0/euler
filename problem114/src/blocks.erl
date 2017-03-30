-module(blocks).

-export(
   [
    count/1,
    init/0,
    main/0,
    start/0,
    stop/0
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

-endif.
