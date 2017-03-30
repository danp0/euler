-module(summations).

-export(
   [
    enter/2,
    expand/1,
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

expand(N) when N =< 1 ->
  [];
expand(N) ->
  lists:append([expand_by(N,I) || I <- lists:seq(1, N-1)]).

expand_by(N, I) when I > N ->
  [];
expand_by(N, I) ->
  case I < N-I of
    true ->
      [[I|E] || E <- expand(N-I), I >= hd(E)];
    false ->
      lists:append([[I, N-I]], [[I|E] || E <- expand(N-I), I >= hd(E)])
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
  start(),
  P = p(100),
  io:format("p(100): ~p, p(100) - 1: ~p~n", [P, P-1]),
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

%%
%% unit tests
%%
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
       [length(expand(I)) || I <- lists:seq(2, 20)],
       [p(I) - 1 || I <- lists:seq(2, 20)])
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
