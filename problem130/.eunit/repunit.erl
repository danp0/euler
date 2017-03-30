-module(repunit).

-export(
   [
    a/1,
    find/2,
    gcd/2,
    is_cwprp/2,
    is_prime/1,
    main/0
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

find(Fun, Count) ->
  find(2, Fun, Count, []).

find(_I, _Fun, Count, Ack) when Count =:= length(Ack) ->
  lists:reverse(Ack);
find(I, Fun, Count, Ack) ->
  A = a(I),
  Next =
  case A > 0 andalso Fun(I, A) of
    true ->
      [I | Ack];
    false ->
      Ack
  end,
  find(I+1, Fun, Count, Next).

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

is_cwprp(I,A) ->
  (I-1) rem A =:= 0 andalso not is_prime(I).

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
is_prime(I, N) ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(I+6, N)
  end.

main() ->
  io:format("repunit...~n", []),
  Composites =
  find(fun is_cwprp/2, 25),
  io:format("~p~n", [Composites]),
  io:format("~p~n", [lists:sum(Composites)]),
  ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

a_test_() ->
  [
   ?_assertEqual(
      [1,3,6,9,2,6,16,18,6,22,27],
      [a(I) || I <- [1,3,7,9,11,13,17,19,21,23,27]])
  ].

find_test_() ->
  [
   ?_assertEqual([91, 259, 451, 481, 703], find(fun is_cwprp/2, 5))
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
      [2,3,5,7,11,13,17,19,23,29],
      [P || P <- lists:seq(1,30), is_prime(P)])
  ].

-endif.
