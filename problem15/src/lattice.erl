-module(lattice).

-export(
   [
    add/3,
    get/2,
    get_or_walk/2,
    main/0,
    start/0,
    stop/0,
    walk/2,
    walk_task/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          map
        }).

add(Dx, Dy, Routes) ->
  ?MODULE ! {add, Dx, Dy, Routes, self()},
  receive
    ok ->
      true;
    _ ->
      false
  end.

get(Dx, Dy) ->
  ?MODULE ! {get, Dx, Dy, self()},
  receive
    not_found ->
      not_found;
    Value ->
      Value
  end.

get_or_walk(Dx, Dy) ->
  case get(Dx, Dy) of
    not_found ->
      Routes = walk(Dx, Dy),
      add(Dx, Dy, Routes),
      Routes;
    Value ->
      Value
  end.

main() ->
  start(),
  io:format("walk(20,20): ~p~n", [walk(20,20)]),
  stop().

start() ->
  Pid = spawn(?MODULE, walk_task, [#state{map = dict:new()}]),
  register(?MODULE, Pid),
  Pid.

stop() ->
  ?MODULE ! {exit, self()},
  receive
    ok ->
      true;
    _ ->
      false
  end.

walk(0, 0) ->
  1;
walk(0, Dy) when Dy >= 0 ->
  get_or_walk(0, Dy - 1);
walk(Dx, 0) when Dx >= 0 ->
  get_or_walk(Dx - 1, 0);
walk(Dx, Dy) when Dx >= 0 andalso Dy >= 0 ->
  get_or_walk(Dx - 1, Dy) + get_or_walk(Dx, Dy - 1).

walk_task(State) ->
  receive
    {add, Dx, Dy, Routes, Sender} ->
      Sender ! ok,
      walk_task(State#state{map = dict:store({Dx, Dy}, Routes, State#state.map)});
    {get, Dx, Dy, Sender} ->
      Sender !
      case dict:find({Dx, Dy}, State#state.map) of
        {ok, Value} ->
          Value;
        error ->
          not_found
      end,
      walk_task(State);
    {exit, Sender} ->
      Sender ! ok
  end.

%%
%% unit tests
%%
walk_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
    [
      ?_assertEqual(1,   walk(0,0)),
      ?_assertEqual(2,   walk(1,1)),
      ?_assertEqual(6,   walk(2,2)),
      ?_assertEqual(20,  walk(3,3)),
      ?_assertEqual(70,  walk(4,4)),
      ?_assertEqual(252, walk(5,5)),
      ?_assertEqual(924, walk(6,6))
    ]
  }.

walk_task_test_() ->
  {
   setup,
   fun() -> 
       start(),
       add(0, 0, 1),
       add(1, 1, 2),
       add(2, 2, 6)
   end,
   fun(_) -> 
       stop() 
   end,
   [
    ?_assertEqual(1, get(0, 0)),
    ?_assertEqual(2, get(1, 1)),
    ?_assertEqual(6, get(2, 2)),
    ?_assertEqual(not_found, get(3, 3))
   ]
  }.
