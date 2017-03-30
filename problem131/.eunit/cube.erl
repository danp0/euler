-module(cube).

-export(
   [
    count_prime_cubes/1,
    init/1,
    is_prime/1,
    main/0,
    sieve/1,
    start/1,
    stop/0
   ]
  ).

-record(
   state,
   {
    primes
   }
  ).

count_prime_cubes(Upto) ->
  start(Upto),
  Count =
  count_prime_cubes(1, Upto, 0),
  ok = stop(),
  Count.

count_prime_cubes(I, Upto, Count) ->
  Diff = (I+1)*(I+1)*(I+1) - I*I*I,
  case Diff >= Upto of
    true ->
      Count;
    false ->
      CountNext =
      case is_prime(Diff) of
        true ->
          Count+1;
        false ->
          Count
      end,
      count_prime_cubes(I+1, Upto, CountNext)
  end.

init(Upto) ->
  loop(#state{primes = gb_sets:from_list(sieve(Upto))}).

is_prime(N) ->
  send({is_prime, N}).

loop(State) ->
  NextState =
  receive
    {From, {is_prime, N}} ->
      From ! gb_sets:is_member(N, State#state.primes),
      State;
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NextState).

main() ->
  io:format("~p~n", [count_prime_cubes(1000000)]),
  ok.

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

sieve(N) ->
  Sieve =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{default, true}, {fixed, true}]))),
  sieve(2, N, Sieve).

sieve(I, N, Sieve) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun
        (J, true, Acc) ->
          [J|Acc];
      (_J, false, Acc) ->
          Acc
      end,
      [],
      Sieve));
sieve(I, N, Sieve) ->
  NextSieve =
  case array:get(I, Sieve) of
    false ->
      Sieve;
    true ->
      lists:foldl(
        fun(J, S) -> 
            array:set(J, false, S) 
        end,
        Sieve,
        lists:seq(I*I, N, I))
  end,
  sieve(I+1, N, NextSieve).

start(Upto) ->
  register(?MODULE, spawn(?MODULE, init, [Upto])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

count_prime_cubes_test_() ->
  [
   ?_assertEqual(4, count_prime_cubes(100))
  ].

is_prime_test_() ->
  {
   setup,
   fun() -> start(100) end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([2,3,5,7,11,13,17,19,23,29], 
                      [N || N <- lists:seq(1,30), is_prime(N)])
       ]
   end
  }.

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
