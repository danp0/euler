-module(repunit).

-export(
   [
    a/1,
    is_prime/1,
    main/0,
    rfactor/2
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

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =< 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

main() ->
  F = rfactor(1000000000, 40),
  io:format("~w~n", [F]),
  io:format("~p~n", [lists:sum(F)]),
  ok.

rfactor(N, Count) ->
  rfactor(N, Count, 3, []).

rfactor(_N, Count, _I, Ack) when length(Ack) >= Count ->
  lists:reverse(Ack);
rfactor(N, Count, I, Ack) ->
  Next =
  case is_prime(I) of
    true ->
      A = a(I),
      case A =/= 0 andalso N rem A =:= 0 of
        true ->
          [I | Ack];
        false ->
          Ack
      end;
    false ->
      Ack
  end,
  rfactor(N, Count, I+2, Next).

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

is_prime_test_() ->
  [
   ?_assertEqual(
      [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97], 
      [P || P <- lists:seq(1,100), is_prime(P)])
  ].

rfactor_test_() ->
  [
   ?_assertEqual([11,41,271,9091], rfactor(10, 4))
  ].

-endif.

