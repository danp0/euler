-module(repunit).

-export(
   [
    a/1,
    find/1,
    gcd/2,
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

find(N) ->
  case N rem 2 =:= 0 of
    true ->
      find(N+1, N);
    false ->
      find(N, N)
  end.

find(I, N) ->
  case I rem 5 =/= 0 of
    true ->
      case a(I) > N of
        true ->
          I;
        false ->
          find(I+2, N)
      end;
    false ->
      find(I+2, N)
  end.

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

main() ->
  io:format("repunit~n", []),
  io:format("~p~n", [find(1000000)]),
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
   ?_assertEqual(
      [find(I) || I <- [10, 100, 250, 1000, 2500, 10000]],
      [17, 109, 257, 1017, 2539, 10007])
  ].

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2, 0)),
   ?_assertEqual(1, gcd(2, 3)),
   ?_assertEqual(1, gcd(3, 2)),
   ?_assertEqual(10, gcd(20, 30)),
   ?_assertEqual(10, gcd(30, 20))
  ].

-endif.
