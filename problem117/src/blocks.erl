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
    c = gb_trees:empty()
   }
  ).

count(0) ->
  1;
count(N) when N < 0 ->
  0;
count(N) ->
  lookup(N).

init() ->
  loop(#state{}).

lookup(N) ->
  case send({lookup, N}) of
    none ->
      Count = count(N-1) + count(N-2) + count(N-3) + count(N-4),
      send({enter, N, Count}),
      Count;
    {value, Value} ->
      Value
  end.

loop(State) ->
  NewState =
  receive
    {From, {lookup, Key}} ->
      From ! gb_trees:lookup(Key, State#state.c),
      State;
    {From, {enter, Key, Value}} ->
      C = gb_trees:enter(Key, Value, State#state.c),
      From ! ok,
      State#state{c = C};
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  io:format("blocks...~n", []),
  start(),
  lists:foreach(
    fun(I) ->
      io:format("~p: ~p~n", [I, count(I)])
    end,
    lists:seq(5,50)),
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
   fun(_) ->
       [
        ?_assertEqual(15, count(5))
       ]
   end
  }.

-endif.
