-module(spiral).

-export(
   [
    is_prime/1,
    main/0,
    spiral_foldl/2
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

is_prime(I, N) when I * I =< N ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(I+6, N)
  end;
is_prime(_I, _N) ->
  true.

main() ->
  io:format("spiral...~n", []),
  {N, {Primes, Total}} =
  spiral_foldl(
    fun(N, Diagonals, {Primes, Total}) ->
        Primes2 =
        Primes +
        lists:foldl(
          fun(E, Count) ->
              case is_prime(E) of
                true ->
                  Count + 1;
                false ->
                  Count
              end
          end,
          0,
          Diagonals),
        Total2 = Total + length(Diagonals),
        PercentagePrimes = 100.0 * Primes2/Total2,
        {N =/= 0 andalso PercentagePrimes < 10.0, {Primes2, Total2}}
    end,
    {0,0}),
  io:format("~p: ~p~n~p~n", [N, {Primes,Total}, 100.0 * Primes/Total]),
  io:format("~p~n", [2*N+1]),
  ok.

spiral_foldl(F, Acc) ->
  spiral_foldl(F, Acc, 0).

spiral_foldl(F, Acc, N) ->
  Diagonals =
  if
    N =:= 0 ->
      [1];
    true ->
      TwoNPlus1 = 2*N + 1,
      Squared = TwoNPlus1 * TwoNPlus1,
      [Squared, Squared - 2*N, Squared - 4*N, Squared - 6*N]
  end,
  case F(N, Diagonals, Acc) of
    {true, Acc2} ->
      {N, Acc2};
    {false, Acc2} ->
      spiral_foldl(F, Acc2, N+1)
  end.

%%
%% unit test
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

-endif.
