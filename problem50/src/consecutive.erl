-module(consecutive).

-export(
   [
    find_prime_sum/1,
    init/1,
    is_prime/1,
    loop/1,
    main/0,
    primes/0,
    reduce_until_prime/1,
    sieve/1,
    start/1,
    stop/0,
    sum_until/2
   ]
  ).

-record(
   state,
   {
    max = 0,
    primes = gb_sets:empty(),
    sieve = []
   }
  ).

find_prime_sum(Max) ->
  {Sum, Primes} = sum_until(primes(), Max),
  {Sum1, Primes1} = reduce_until_prime({Sum, Primes}),
  {Sum2, Primes2} = reduce_until_prime({Sum, lists:reverse(Primes)}),
  case length(Primes1) >= length(Primes2) of
    true ->
      {Sum1, Primes1};
    false ->
      {Sum2, Primes2}
  end.

init(N) ->
  Sieve = sieve(N),
  loop(#state{max=N, primes=gb_sets:from_list(Sieve), sieve=Sieve}).

is_prime(N) ->
  primes ! {{is_prime, N}, self()},
  receive 
    Result ->
      Result
  end.

is_prime2(N) when N < 2 ->
  false;
is_prime2(N) when N =:= 2 orelse N =:= 3 ->
  true;
is_prime2(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
  false;
is_prime2(N) ->
  is_prime2(5, N).

is_prime2(I, N) when I*I > N ->
  true;
is_prime2(I, N) ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime2(I+6, N)
  end.

loop(State) ->
  receive
    {{is_prime, N}, From} ->
      case N > State#state.max of
        true ->
          From ! is_prime2(N);
        false ->
          From ! gb_sets:is_element(N, State#state.primes)
      end;
    {primes, From} ->
      From ! State#state.sieve;
    {stop, From} ->
      From ! ok,
      exit(normal)
  end,
  loop(State).

main() ->
  start(4000),
  Sum = find_prime_sum(999999),
  io:format("1000000: ~p~n~p~n", [Sum, element(1, Sum)]),
  ok = stop(),
  ok.

primes() ->
  primes ! {primes, self()},
  receive 
    Result ->
      Result
  end.

reduce_until_prime({Sum, [H|T]=Primes}) ->
  case is_prime(Sum) of
    true ->
      {Sum, Primes};
    false ->
      reduce_until_prime({Sum - H, T})
  end.

sieve(N) ->
  sieve(2, N, gb_trees:from_orddict([{I,true} || I <- lists:seq(2,N)])).

sieve(I, N, Primes) when I*I >= N ->
  lists:map(fun({K,_V}) -> K end,
            lists:filter(fun({_K,V}) -> V end, gb_trees:to_list(Primes)));
sieve(I, N, Primes) ->
  case gb_trees:get(I, Primes) of
    true ->
      sieve(I+1, N, non_primes(I, I*I, N, Primes));
    false ->
      sieve(I+1, N, Primes)
  end.

non_primes(_I, ISquaredPlus, N, Primes) when ISquaredPlus > N ->
  Primes;
non_primes(I, ISquaredPlus, N, Primes) ->
  non_primes(I, ISquaredPlus + I, N, 
             gb_trees:enter(ISquaredPlus, false, Primes)).

start(N) ->
  register(primes, spawn(?MODULE, init, [N])).

stop() ->
  primes ! {stop, self()},
  receive
    Result ->
      Result
  end.

sum_until(Primes, Max) ->
  sum_until(Primes, Max, 0, []).

sum_until([], _Max, Sum, Members) ->
  {Sum, lists:reverse(Members)};
sum_until([H|_T], Max, Sum, Members) when H+Sum > Max ->
  sum_until([], Max, Sum, Members);
sum_until([H|T], Max, Sum, Members) ->
  sum_until(T, Max, H+Sum, [H|Members]).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_prime_sum_test_() ->
  {setup,
   fun() -> start(1000) end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(41, element(1, find_prime_sum(99))),
    ?_assertEqual(953, element(1, find_prime_sum(999)))
   ]
  }.

is_prime_test_() ->
  {setup,
   fun() -> start(100) end,
   fun(_) -> stop() end,
   [
    ?_assert(is_prime(2)),
    ?_assert(is_prime(3)),
    ?_assertNot(is_prime(4)),
    ?_assert(is_prime(5))
   ]
  }.

is_prime2_test_() ->
  [
   ?_assert(is_prime2(2)),
   ?_assert(is_prime2(3)),
   ?_assertNot(is_prime2(4)),
   ?_assert(is_prime2(5)),
   ?_assertNot(is_prime2(6)),
   ?_assert(is_prime2(7)),
   ?_assertNot(is_prime2(8)),
   ?_assertNot(is_prime2(9))
  ].

primes_test_() ->
  {setup,
   fun() -> start(10) end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([2,3,5,7], primes())
   ]
  }.

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19], sieve(20))
  ].

-endif.
