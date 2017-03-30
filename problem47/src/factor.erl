-module(factor).

-export(
   [
    factor/1,
    find/1,
    init/0,
    main/0,
    reduce/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state, 
   {
    primes = gb_sets:empty()
   }
  ).

factor(N) ->
  primes ! {{factor, N}, self()},
  receive
    Result ->
      Result
  end.

factor(1, _Primes, _Prime, Acc) ->
  lists:reverse(Acc);
factor(N, Primes, Prime, Acc) when N rem Prime =:= 0 ->
  factor(N div Prime, Primes, Prime, [Prime | Acc]);
factor(N, Primes, _Prime, Acc) ->
  case gb_sets:next(Primes) of
    none ->
      factor(1, Primes, none, [N|Acc]);
    {NextPrime, NextPrimes} ->
      factor(N, NextPrimes, NextPrime, Acc)
  end.

find(N) ->
  find(N, 2, []).

find(N, _I, Acc) when length(Acc) =:= N ->
  lists:reverse(Acc);
find(N, I, Acc) ->
  F = factor(I),
  case length(F) =:= N of
    true ->
      find(N, I+1, [I|Acc]);
    false ->
      find(N, I+1, [])
  end.

init() ->
  loop(#state{primes = gb_sets:add_element(2, gb_sets:empty())}).

loop(State) ->
  NewState =
  receive
    {{factor, N}, From} ->
      PrimeSet = State#state.primes,
      {Prime, Primes} = gb_sets:next(gb_sets:iterator(PrimeSet)),
      Factors = factor(N, Primes, Prime, []),
      From ! reduce(Factors),
      if
        length(Factors) =:= 1 ->
          State#state{primes = gb_sets:add_element(hd(Factors), PrimeSet)};
        true ->
          State
      end;
    {stop, From} ->
      From ! ok,
      exit(normal);
    _ ->
      State
  end,
  loop(NewState).

main() ->
  start(),
  {T,V} = timer:tc(?MODULE, find, [4]),
  io:format("~w in ~p~n", [V, T]),
  ok = stop(),
  ok.

reduce([]) ->
  [];
reduce([N|T]) ->
  reduce(T, [N], []).

reduce([], [], Acc) ->
  lists:map(
    fun
      ([N]) ->
        N;
      ([N|T]) ->
        {N, length(T) + 1}
    end,
    lists:reverse(Acc));
reduce([], D, Acc) ->
  reduce([], [], [D|Acc]);
reduce([N|T1], [N|T2], Acc) ->
  reduce(T1, [N,N|T2], Acc);
reduce([M|T1], D, Acc) ->
  reduce(T1, [M], [D|Acc]).

start() ->
  Pid = spawn(?MODULE, init, []),
  register(primes, Pid),
  ok.

stop() ->
  primes ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([14,15], find(2)),
    ?_assertEqual([644,645,646], find(3))
   ]
  }.

reduce_test_() ->
  [
   ?_assertEqual([2], reduce([2])),
   ?_assertEqual([2,3], reduce([2,3])),
   ?_assertEqual([{2,2},3], reduce([2,2,3])),
   ?_assertEqual([{2,3},3], reduce([2,2,2,3])),
   ?_assertEqual([{2,3},{3,2}], reduce([2,2,2,3,3])),
   ?_assertEqual([{2,3},{3,2},4,{5,2}], reduce([2,2,2,3,3,4,5,5]))
  ].

-endif.
