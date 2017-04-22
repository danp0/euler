-module(quadratics).

-export(
   [
    count_primes/1,
    find_max_primes/0,
    generate/2,
    main/0,
    sieve/1,
    test_prime/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% For n^2 + an + b where |a| < 1000 and |b| <= 1000,
% find the product of the coefficients, a and b that
% produces the maximum number of primes for consecutive
% values of n >= 0.
%

count_primes(F) ->
  count_primes(F, 0).

count_primes(F, N) ->
  case test_prime(F(N)) of
    true ->
      count_primes(F, N+1);
    false ->
      N
  end.

find_max_primes() ->
  PrimeCounts = 
  [{A, B, count_primes(generate(A,B))} || 
   A <- lists:seq(-999,999), B <- sieve(999), A >= 1 - B],
  lists:foldl(
    fun({_, _, C} = E, {_, _, CMax} = M) ->
        case C > CMax of
          true ->
            E;
          false ->
            M
        end
    end,
    {0, 0, 0},
    PrimeCounts).

generate(A, B) ->
  fun(N) ->
      N*N + A*N + B
  end.

main() ->
  {Microseconds,{A, B, C}} = timer:tc(?MODULE, find_max_primes, []),
  io:format("A: ~p, B: ~p, C: ~p, A*B: ~p in ~p~n", 
            [A, B, C, A*B, Microseconds]),
  ok.

sieve(N) ->
  sieve(lists:seq(2,N), []).

sieve([], Acc) ->
  lists:reverse(Acc);
sieve([H|T], Acc) ->
  Filter =
  lists:filter(
    fun(N) ->
        N rem H =/= 0
    end,
    T),
  sieve(Filter, [H|Acc]).

test_prime(N) when N < 2 ->
  false;
test_prime(N) when N =:= 2 orelse N =:= 3 ->
  true;
test_prime(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
  false;
test_prime(N) ->
  test_prime(5, trunc(math:sqrt(N)) + 1, N).

test_prime(From, To, _N) when From > To ->
  true;
test_prime(From, _To, N) when N rem From =:= 0 orelse N rem (From + 2) =:= 0 ->
  false;
test_prime(From, To, N) ->
  test_prime(From + 1, To, N).

%%
%% unit test
%%
count_primes_test_() ->
 [
  ?_assertEqual(40, count_primes(generate(1, 41))),
  ?_assertEqual(80, count_primes(generate(-79, 1601)))
 ].

generate_test_() ->
  F = generate(1, 2),
  [
   ?_assertEqual(2, F(0)),
   ?_assertEqual(4, F(1)),
   ?_assertEqual(8, F(2))
  ].

test_prime_test_() ->
  [
   ?_assert(test_prime(2)),
   ?_assert(test_prime(3)),
   ?_assertNot(test_prime(4)),
   ?_assert(test_prime(5)),
   ?_assertNot(test_prime(6)),
   ?_assert(test_prime(7)),
   ?_assertNot(test_prime(8)),
   ?_assertNot(test_prime(9)),
   ?_assertNot(test_prime(10)),
   ?_assert(test_prime(11))
  ].
