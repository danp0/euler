-module(repunit).

-export(
   [
    a/1,
    main/0,
    sieve/1
   ]
  ).

a(N) ->
  case gcd(N, 10) =:= 1 of
    true ->
      a(N, 1 rem N, 1);
    false ->
      0
  end.

a(_N, 0, K) ->
  K;
a(N, R, K) ->
  a(N, (R*10 + 1) rem N, K+1).

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

main() ->
  io:format("repunit...~n", []),
  ok.

sieve(N) ->
  Sieve =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{default, true}, {fixed, true}]))),
  sieve(2, N, Sieve).

sieve(I, N, Sieve) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun(Index, true, Primes) ->
          [Index | Primes];
         (_Index, false, Primes) ->
          Primes
      end,
      [],
      Sieve));
sieve(I, N, Sieve) ->
  Next =
  case array:get(I, Sieve) of
    true ->
      lists:foldl(
        fun(J, S) ->
            array:set(J, false, S)
        end,
        Sieve,
        lists:seq(I*I, N, I));
    false ->
      Sieve
  end,
  sieve(I+1, N, Next).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

a_test_() ->
  [
   ?_assertEqual(
      [1,3,6,9,2,6,16,18,6,22,27],
      [a(I) || I <- [1,3,7,9,11,13,17,19,21,23,27]])
  ].

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2, 0)),
   ?_assertEqual(1, gcd(2, 3)),
   ?_assertEqual(1, gcd(3, 2)),
   ?_assertEqual(10, gcd(20, 30)),
   ?_assertEqual(10, gcd(30, 20))
  ].

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
