-module(powers).

-export(
   [
    find/1,
    find2/1,
    init/0,
    loop/1,
    main/0,
    pow/2,
    pow2/2,
    start/0,
    stop/0,
    uniq/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

-record(state,
        {
         powers = dict:new()
        }
       ).

find(N) ->
  uniq(
    lists:sort(
      [pow(A,B) || A <- lists:seq(2,N), B <- lists:seq(2,N)]
     )
   ).

find2(N) ->
  uniq(
    lists:sort(
      [pow2(A,B) || A <- lists:seq(2,N), B <- lists:seq(2,N)]
     )
   ).

init() ->
  loop(#state{}).

lookup(_A, 0, Powers) ->
  {1, Powers};
lookup(A, B, Powers) ->
  case dict:find({A, B}, Powers) of
    {ok, Power} ->
      {Power, Powers};
    error ->
      {Power_1, Powers_1} = lookup(A, B-1, Powers),
      Power = A * Power_1,
      NewPowers = dict:store({A,B}, Power, Powers_1),
      {Power, NewPowers}
  end.

loop(#state{powers = Powers} = State) ->
  receive
    {power, A, B, Sender} ->
      {Power, NewPowers} = lookup(A, B, Powers),
      Sender ! Power,
      loop(State#state{powers = NewPowers});
    {stop, Sender} ->
      Sender ! ok
  end.

main() ->
  start(),
  {T, Powers}   = timer:tc(?MODULE, find, [100]),
  {T2, Powers2} = timer:tc(?MODULE, find, [100]),
  {T3, Powers3} = timer:tc(?MODULE, find2, [100]),
  io:format("~p in ~p microseconds ~n", [length(Powers), T]),
  io:format("~p in ~p microseconds ~n", [length(Powers2), T2]),
  io:format("~p in ~p microseconds ~n", [length(Powers3), T3]),
  stop(),
  ok.

pow(A, B) ->
  ?MODULE ! {power, A, B, self()},
  receive
    Power -> Power
  end.

pow2(_A,0) ->
  1;
pow2(A,B) ->
  A * pow2(A, B-1).

start() ->
  Pid = spawn(?MODULE, init, []),
  register(?MODULE, Pid),
  Pid.

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Ok -> Ok
  end.

uniq(L) ->
  uniq(L, []).

uniq([], Acc) ->
  lists:reverse(Acc);
uniq([E], Acc) ->
  uniq([], [E | Acc]);
uniq([E, E | T], Acc) ->
  uniq([E | T], Acc);
uniq([E, F | T], Acc) ->
  uniq([F | T], [E | Acc]).

%%
%% unit test
%%
find_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(
       [4,8,9,16,25,27,32,64,81,125,243,256,625,1024,3125], find(5))
   ]
  }.

pow_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(1,  pow(2,0)),
    ?_assertEqual(2,  pow(2,1)),
    ?_assertEqual(4,  pow(2,2)),
    ?_assertEqual(8,  pow(2,3)),
    ?_assertEqual(16, pow(2,4)),
    ?_assertEqual(32, pow(2,5))
   ]
  }.

uniq_test_() ->
  [
   ?_assertEqual([], uniq([])),
   ?_assertEqual([1,2], uniq([1,2,2])),
   ?_assertEqual([1,2,3], uniq([1,2,2,3,3,3])),
   ?_assertEqual([1,2,3], uniq([1,1,1,2,2,3,3,3])),
   ?_assertEqual([1,2,3,2,1], uniq([1,2,2,3,3,3,2,2,1]))
  ].

