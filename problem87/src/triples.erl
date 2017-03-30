-module(triples).

-export(
   [
    is_prime/1,
    main/0,
    max_triple_below/1,
    primes/1,
    triples_below/1
   ]
  ).

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) ->
  case N rem I =:= 0 orelse N rem (I + 2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(I+6, N)
  end.

main() ->
  N = 50000000,
  Triples = triples_below(N),
  io:format("~p~n", [length(Triples)]),
  ok.

max_triple_below(N) ->
  trunc(math:sqrt(N - 24)).

primes(N) ->
  lists:filter(
    fun(I) ->
        is_prime(I)
    end,
    lists:seq(2, N)).

triples_below(N) ->
  Primes = primes(max_triple_below(N)),
  P2 = [P*P || P <- Primes],
  P3 = [P*P*P || P <- Primes],
  P4 = [P*P*P*P || P <- Primes],
  lists:usort(
    [A + B + C || A <- P2, B <- P3, C <- P4, A + B + C < N]
   ).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
  [
   ?_assertNot(is_prime(1)),
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
   ?_assertNot(is_prime(15))
  ].

max_triple_below_test_() ->
  [
   ?_assertEqual(5, max_triple_below(50)),
   ?_assertEqual(7071, max_triple_below(50000000))
  ].

primes_test_() ->
  [
   ?_assertEqual([2], primes(2)),
   ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19], primes(20))
  ].

-endif.
