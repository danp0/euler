-module(partitions).

-export(
   [
    enter/2,
    find_until/1,
    g/1,
    init/0,
    lookup/1,
    main/0,
    p/1,
    sign/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    p = gb_trees:empty()
   }
  ).

enter(N, P) ->
  ?MODULE ! {enter, N, P, self()},
  receive
    Result ->
      Result
  end.

find_until(F) ->
  find_until(F, 1).

find_until(F, I) ->
  P = p(I),
  case F(P) of
    true ->
      {I, P};
    false ->
      find_until(F, I+1)
  end.

g(K) ->
  K * (3*K - 1) div 2.

init() ->
  loop(#state{}).

lookup(N) ->
  ?MODULE ! {lookup, N, self()},
  receive
    Result ->
      Result
  end.

loop(State) ->
  NewState =
  receive
    {enter, N, P, From} ->
      From ! ok,
      State#state{p = gb_trees:enter(N, P, State#state.p)};
    {lookup, N, From} ->
      From ! gb_trees:lookup(N, State#state.p),
      State;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  io:format("paritions...~n", []),
  start(),
  IP =
  find_until(
    fun(P) ->
      P rem 1000000 =:= 0
    end),
  io:format("~p~n", [IP]),
  ok = stop(),
  ok.

p(N) when N < 0 ->
  0;
p(0) ->
  1;
p(N) ->
  case lookup(N) of
    none ->
      P = p(N, 1, 0),
      enter(N, P),
      P;
    {value, Value} ->
      Value
  end.

p(N, K, Sum) ->
  P1 = sign(K - 1) * p(N - g(K)),
  P2 = sign(-K - 1) * p(N - g(-K)),
  case P1 =:= 0 andalso P2 =:= 0 of
    false ->
      p(N, K+1, Sum + P1 + P2);
    true ->
      Sum
  end.

sign(N) ->
  case abs(N) rem 2 of
    0 ->
      1;
    1 ->
      -1
  end.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Result ->
      Result
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

g_test_() ->
  [
   ?_assertEqual(
      [0,0,1,2,5,7,12,15,22,26,35,40,51,57,70,77], 
      [g(I) || I <- lists:append([[J, -J] || J <- lists:seq(0, 7)])])
  ].

p_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(
       [1,1,2,3,5,7,11,15,22,30,42,56,77,101,135,176,231,297,385,490,627],
       [p(I) || I <- lists:seq(0, 20)])
   ]
  }.

sign_test_() ->
  [
   ?_assertEqual(1, sign(0)),
   ?_assertEqual(-1, sign(-1)),
   ?_assertEqual(-1, sign(1)),
   ?_assertEqual(1, sign(-2)),
   ?_assertEqual(1, sign(2))
  ].

-endif.
