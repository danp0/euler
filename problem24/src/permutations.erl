-module(permutations).

-export(
   [
    fac/1,
    main/0,
    nth/2,
    seq/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

fac(0) ->
  1;
fac(N) ->
  N * fac(N-1).

main() ->
  io:format("permutations...~n", []),
  io:format("~p~n", [nth(1000000, [0,1,2,3,4,5,6,7,8,9])]).

nth(1, [E]) ->
  [E];
nth(_N, [_E]) ->
  error;
nth(N, L) ->
  Permutations = fac(length(L)),
  SubPermutations = fac(length(L) - 1),
  case N > Permutations of
    true ->
      error;
    false ->
      Nth = (N - 1) div SubPermutations,
      NthElement = lists:nth(Nth + 1, L),
      [NthElement | nth((N-1) rem SubPermutations + 1, L -- [NthElement])]
  end.

seq([H]) ->
  [[H]];
seq(L) ->
  [[E | T] || E <- L, T <- seq(L -- [E])].

%%
%% unit tests
%%
fac_test_() ->
  [
   ?_assertEqual(1,   fac(0)),
   ?_assertEqual(1,   fac(1)),
   ?_assertEqual(2,   fac(2)),
   ?_assertEqual(6,   fac(3)),
   ?_assertEqual(24,  fac(4)),
   ?_assertEqual(120, fac(5))
  ].

nth_test_() ->
  [
   ?_assertEqual(seq([0]), [nth(1, [0])]),
   ?_assertEqual(seq([0,1]), [nth(N, [0,1]) || N <- lists:seq(1,fac(2))]),
   ?_assertEqual(seq([0,1,2]), [nth(N, [0,1,2]) || N <- lists:seq(1,fac(3))]),
   ?_assertEqual(seq([0,1,2,3]), [nth(N, [0,1,2,3]) || N <- lists:seq(1,fac(4))]),
   ?_assertEqual(seq([0,1,2,3,4,5]), [nth(N, [0,1,2,3,4,5]) || N <- lists:seq(1,fac(6))])
  ].

seq_test_() ->
  [
   ?_assertEqual([[0]], seq([0])),
   ?_assertEqual([[0,1], [1,0]], seq([0,1])),
   ?_assertEqual([[0,1,2], [0,2,1], [1,0,2], [1,2,0], [2,0,1], [2,1,0]], seq([0,1,2]))
  ].
