-module(combinatoric).

-export(
   [
    choose/2,
    factorial/1,
    init/0,
    main/0,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    factorial = gb_trees:empty(),
    combination = gb_trees:empty()
   }
  ).

choose(N, K) when N >= 0 andalso N >= K ->
  combinatoric ! {choose, {N,K}, self()},
  receive
    Result ->
      Result
  end.

choose(_N, 0, Cache) ->
  {1, Cache};
choose(_N, _N, Cache) ->
  {1, Cache};
choose(N, K, Cache) ->
  case gb_trees:lookup({N, K}, Cache) of
    {value, Value} ->
      {Value, Cache};
    none ->
      {Choices2, Cache2} = choose(N-1, K-1, Cache),
      {Choices3, Cache3} = choose(N-1, K, Cache2),
      Choices = Choices2 + Choices3,
      {Choices, gb_trees:enter({N,K}, Choices, Cache3)}
  end.

factorial(N) ->
  combinatoric ! {factorial, N, self()},
  receive
    Result ->
      Result
  end.

factorial(N, Cache) ->
  case gb_trees:lookup(N, Cache) of
    {value, Value} ->
      {Value, Cache};
    none ->
      {Value2, Cache2} = factorial(N-1, Cache),
      Factorial = N * Value2,
      {Factorial, gb_trees:enter(N, Factorial, Cache2)}
  end.

init() ->
  State = 
  #state{
     factorial = 
     gb_trees:from_orddict([{0,1}, {1,1}]),
     combination = 
     gb_trees:empty()
    },
  loop(State).

loop(State) ->
  NewState =
  receive
    {choose, {N,K}, From} ->
      {Choices, Cache} = choose(N, K, State#state.combination),
      From ! Choices,
      State#state{combination = Cache};
    {factorial, N, From} ->
      {Factorial, Cache} = factorial(N, State#state.factorial),
      From ! Factorial,
      State#state{factorial = Cache};
    {stop, From} ->
      From ! ok,
      exit(normal),
      State;
    Msg ->
      io:format("~p~n", [Msg]),
      State
  end,
  loop(NewState).

main() ->
  io:format("combinatoric...~n", []),
  start(),
  {T, Combinations} = 
  timer:tc(
    fun() -> [{N,K} || N <- lists:seq(1,100), K <- lists:seq(1,N), choose(N,K) > 1000000] end),
  io:format("~p in ~p~n", [length(Combinations), T]),
  ok = stop(),
  ok.

start() ->
  register(combinatoric, spawn(?MODULE, init, [])).

stop() ->
  combinatoric ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

choose_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(
       [factorial(N) div (factorial(K) * factorial(N-K)) || N <- lists:seq(0,20), K <- lists:seq(0,N)], 
       [choose(N,K) || N <- lists:seq(0,20), K <- lists:seq(0,N)])
   ]
  }.

factorial_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(1, factorial(0)),
    ?_assertEqual(1, factorial(1)),
    ?_assertEqual(2, factorial(2)),
    ?_assertEqual(6, factorial(3)),
    ?_assertEqual(24, factorial(4)),
    ?_assertEqual(120, factorial(5)),
    ?_assertEqual(720, factorial(6))
   ]
  }.

-endif.

