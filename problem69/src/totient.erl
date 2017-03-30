-module(totient).

-export(
   [
    gcd/2,
    is_prime/1,
    main/0,
    max_totient/1,
    multiply/1,
    primes_upto/1,
    phi/1
   ]
  ).

gcd(A, A) ->
  A;
gcd(A, B) when A > B ->
  gcd(A - B, B);
gcd(A, B) ->
  gcd(A, B - A).

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I * I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

main() ->
  io:format("~p~n", [max_totient(10)]),
  io:format("~p~n", [max_totient(100)]),
  io:format("~p~n", [max_totient(1000)]),
  io:format("~p~n", [max_totient(10000)]),
  io:format("~p~n", [max_totient(100000)]),
  io:format("~p~n", [max_totient(1000000)]),
  ok.

max_totient(N) ->
  Primes = primes_upto(N),
  Product = multiply(Primes),
  Totient = multiply(lists:map(fun(X) -> phi(X) end, Primes)),
  {Product, Product / Totient}.

multiply(L) ->
  lists:foldl(
    fun(X, Product) ->
        X * Product
    end,
    1,
    L).

phi(N) when N >= 2 ->
  lists:foldl(
    fun(I, Sum) ->
        case gcd(I,N) =:= 1 of
          true ->
            Sum + 1;
          false ->
            Sum
        end
    end,
    0,
    lists:seq(1,N-1)).

primes_upto(N) ->
  primes_upto(2, N, 1, []).

primes_upto(I, N, Product, Acc) ->
  case is_prime(I) of
    true ->
      NewProduct = I * Product,
      case NewProduct > N of
        true ->
          lists:reverse(Acc);
        false ->
          primes_upto(I+1, N, NewProduct, [I|Acc])
      end;
    false ->
      primes_upto(I+1, N, Product, Acc)
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2,2)),
   ?_assertEqual(1, gcd(2,3)),
   ?_assertEqual(2, gcd(2,4))
  ].

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
   ?_assert(is_prime(11))
  ].

max_totient_test_() ->
  [
   ?_assertEqual({6,3.0}, max_totient(10))
  ].

multiply_test_() ->
  [
   ?_assertEqual(1, multiply([1])),
   ?_assertEqual(2, multiply([1,2])),
   ?_assertEqual(6, multiply([1,2,3])),
   ?_assertEqual(24, multiply([1,2,3,4])),
   ?_assertEqual(120, multiply([1,2,3,4,5]))
  ].

phi_test_() ->
  [
   ?_assertEqual(1, phi(2)),
   ?_assertEqual(2, phi(3)),
   ?_assertEqual(2, phi(4)),
   ?_assertEqual(4, phi(5)),
   ?_assertEqual(2, phi(6)),
   ?_assertEqual(6, phi(7)),
   ?_assertEqual(4, phi(8)),
   ?_assertEqual(6, phi(9)),
   ?_assertEqual(4, phi(10))
  ].

primes_upto_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17], primes_upto(1000000))
  ].

-endif.
