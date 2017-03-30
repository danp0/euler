-module(goldbach).

-export(
   [
    find_non_goldbach/0,
    init/0,
    is_goldbach/1,
    is_prime/1,
    loop/1,
    main/0,
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

find_non_goldbach() ->
  find_non_goldbach(3).

find_non_goldbach(N) ->
  case is_prime(N) of
    true ->
      find_non_goldbach(N+2);
    false ->
      case is_goldbach(N) of
        true ->
          find_non_goldbach(N+2);
        false ->
          N
      end
  end.

init() ->
  loop(#state{primes = gb_sets:add_element(2, gb_sets:empty())}).

is_goldbach(N) ->
  primes ! {{is_goldbach, N}, self()},
  receive
    Result ->
      Result
  end.

is_goldbach(N, Iterator) ->
  case gb_sets:next(Iterator) of
    {Prime, IteratorNext} ->
      if
        Prime =:= 2 ->
          is_goldbach(N, IteratorNext);
        Prime >= N ->
          false;
        true ->
          case is_square((N - Prime) div 2) of
            true ->
              true;
            false ->
              is_goldbach(N, IteratorNext)
          end
      end;
    none ->
      false
  end.

is_prime(N) ->
  primes ! {{is_prime, N}, self()},
  receive
    Result ->
      Result
  end.

is_prime(N, I) when I * I =< N ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(N, I+6)
  end;
is_prime(_N, _I) ->
  true.

is_square(N) ->
  Sqrt = trunc(math:sqrt(N)),
  N =:= Sqrt * Sqrt.

loop(State) ->
  NewState =
  receive
    {{is_goldbach, N}, From} ->
      Result = is_goldbach(N, gb_sets:iterator(State#state.primes)),
      From ! Result,
      State;
    {{is_prime, N}, From} ->
      {Result, UpdatedState} = update_prime(N, State),
      From ! Result,
      UpdatedState;
    {stop, From} ->
      From ! ok,
      exit(normal);
    _ ->
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("non goldbach: ~p~n", [find_non_goldbach()]),
  stop(),
  ok.

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

update_prime(N, State) ->
  Is =
  if
    N < 2 ->
      false;
    N =:= 2 orelse N =:= 3 ->
      true;
    N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
      false;
    true ->
      is_prime(N, 5)
  end,
  case Is of
    true ->
      {Is, State#state{primes = gb_sets:add_element(N, State#state.primes)}};
    false ->
      {Is, State}
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_goldbach_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assert(is_prime(3)),
    ?_assert(is_prime(5)),
    ?_assert(is_prime(7)),
    ?_assert(is_goldbach(9)),
    ?_assert(is_prime(11)),
    ?_assert(is_prime(13)),
    ?_assert(is_goldbach(15)),
    ?_assert(is_prime(17))
   ]
  }.

is_prime_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assert(is_prime(2)),
    ?_assert(is_prime(2)),
    ?_assert(is_prime(3)),
    ?_assertNot(is_prime(4)),
    ?_assert(is_prime(5))
   ]
  }.

is_square_test_() ->
  [
   ?_assertNot(is_square(2)),
   ?_assertNot(is_square(3)),
   ?_assert(is_square(4)),
   ?_assertNot(is_square(5)),
   ?_assert(is_square(9))
  ].

-endif.

