-module(summations).

-export(
   [
    init/0,
    is_prime/1,
    main/0,
    primes/1,
    prime_summations/1,
    prime_summations_gt/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    last = 2,
    primes = [2]
   }
  ).

init() ->
  loop(#state{}).

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I + 2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

loop(State) ->
  NewState =
  receive
    {primes, N, From} ->
      case N =< State#state.last of
        true ->
          From ! lists:takewhile(fun(P) -> P =<  N end, State#state.primes),
          State;
        false ->
          Primes = lists:append(
                     State#state.primes,
                     [P || P <- lists:seq(State#state.last + 1, N), 
                           is_prime(P)]),
          From ! Primes,
          State#state{primes = Primes, last = N}
      end;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("~p~n", [prime_summations_gt(5000)]),
  ok = stop(),
  ok.

primes(N) ->
  ?MODULE ! {primes, N, self()},
  receive
    Result ->
      Result
  end.

prime_summations(N) ->
  prime_summations(N, lists:reverse(primes(N-1))).

prime_summations(0, _Primes) ->
  1;
prime_summations(N, Primes) when N < 0; Primes =:= [] ->
  0;
prime_summations(N, [H|T] = Primes) ->
  prime_summations(N, T) + prime_summations(N - H, Primes).

prime_summations_gt(N) ->
  prime_summations_gt(2, N).

prime_summations_gt(I, N) ->
  PrimeSummation = prime_summations(I),
  case PrimeSummation > N of
    true ->
      {I, PrimeSummation};
    false ->
      prime_summations_gt(I+1, N)
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

is_prime_test_() ->
  [
   ?_assertNot(is_prime(0)),
   ?_assert(is_prime(2)),
   ?_assert(is_prime(3)),
   ?_assertNot(is_prime(4)),
   ?_assert(is_prime(5)),
   ?_assertNot(is_prime(6)),
   ?_assert(is_prime(7)),
   ?_assertNot(is_prime(8)),
   ?_assertNot(is_prime(9)),
   ?_assertNot(is_prime(10)),
   ?_assert(is_prime(11)),
   ?_assertNot(is_prime(12)),
   ?_assert(is_prime(13)),
   ?_assertNot(is_prime(14)),
   ?_assertNot(is_prime(15)),
   ?_assertNot(is_prime(16)),
   ?_assert(is_prime(17)),
   ?_assertNot(is_prime(18)),
   ?_assert(is_prime(19)),
   ?_assertNot(is_prime(20))
  ].

primes_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([2], primes(2)),
    ?_assertEqual([2,3], primes(3)),
    ?_assertEqual([2,3], primes(4)),
    ?_assertEqual([2,3,5], primes(5)),
    ?_assertEqual([2,3,5], primes(6)),
    ?_assertEqual([2,3,5,7], primes(7)),
    ?_assertEqual([2,3,5,7,11,13,17,19], primes(20)),
    ?_assertEqual([2,3,5,7,11,13,17,19], primes(20)),
    ?_assertEqual([2,3,5], primes(5))
   ]
  }.

prime_summations_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(0, prime_summations(2)),
    ?_assertEqual(1, prime_summations(4)),
    ?_assertEqual(1, prime_summations(5)),
    ?_assertEqual(2, prime_summations(6)),
    ?_assertEqual(2, prime_summations(7)),
    ?_assertEqual(3, prime_summations(8)),
    ?_assertEqual(4, prime_summations(9)),
    ?_assertEqual(5, prime_summations(10)),
    ?_assertEqual(5, prime_summations(11))
   ]
  }.

-endif.
