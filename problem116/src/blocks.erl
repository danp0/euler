-module(blocks).

-export(
   [
    c/2,
    init/0,
    main/0,
    start/0,
    stop/0,
    tile/2
   ]
  ).

-record(
   state,
   {
    c = gb_trees:empty()
   }
  ).

c(_N, 0) ->
  1;
c(N, N) ->
  1;
c(N, K) ->
  lookup(N,K).

init() ->
  loop(#state{}).

lookup(N, K) ->
  case send({lookup, {N, K}}) of
    none ->
      C = c(N-1, K-1) + c(N-1, K),
      send({enter, {N, K}, C}),
      C;
    {value, Value} ->
      Value
  end.

loop(State) ->
  NewState =
  receive
    {From, {enter, {N, K}, C}} ->
      Tree = gb_trees:enter({N, K}, C, State#state.c),
      From ! ok,
      State#state{c = Tree};
    {From, {lookup, {N, K}}} ->
      From ! gb_trees:lookup({N, K}, State#state.c),
      State;
    {From, exit} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("~p~n", [tile(2,50) + tile(3,50) + tile(4,50)]),
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
  send(exit).

tile(TileLength, Width) ->
  lists:sum(
    [
     c(I + Width - I * TileLength, Width - I * TileLength) || 
     I <- lists:seq(1, Width div TileLength)
    ]
   ).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

c_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(1, c(1,0)),
        ?_assertEqual(1, c(1,1)),
        ?_assertEqual(2, c(2,1)),
        ?_assertEqual(3, c(3,2)),
        ?_assertEqual(20, c(6,3))
       ]
   end
  }.

tile_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(7, tile(2, 5)),
        ?_assertEqual(3, tile(3, 5)),
        ?_assertEqual(2, tile(4, 5))
       ]
   end
  }.
-endif.
