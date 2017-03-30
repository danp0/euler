-module(fraction).

-export(
   [
    equal/2,
    find/0,
    gcd/2,
    main/0,
    multiply/1,
    reduce/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

equal(F1, F2) ->
  reduce(F1) =:= reduce(F2).

find() ->
  Seq = lists:seq(11,99) -- [20, 30, 40, 50, 60, 70, 80, 90],
  Candidates =
  [{F1, F2} || F1 <- Seq, F2 <- Seq, F1 < F2, F1 rem 10 == F2 div 10],
  lists:filter(
   fun({A,B} = F1) -> 
      F2 = {C,D} = {A div 10, B rem 10},
      C < D andalso equal(F1, F2)
   end,
   Candidates).

gcd(M,N) when M =:= N ->
  M;
gcd(M,N) when M > N ->
  gcd(M-N, N);
gcd(M,N) ->
  gcd(M, N-M).

main() ->
  Fractions = find(),
  Product = multiply(Fractions),
  io:format("~p~n~p~n", [Fractions, Product]),
  ok.

multiply(Fractions) ->
  Product =
  lists:foldl(fun({A,B}, {C,D}) -> {A*C, B*D} end, {1,1}, Fractions),
  reduce(Product).
  
reduce({A,B}) ->
  Gcd = gcd(A,B),
  {A div Gcd, B div Gcd}.

%%
%% unit test
%%
equal_test_() ->
  [
   ?_assert(equal({1,2}, {6,12})),
   ?_assert(equal({3,6}, {5,10})),
   ?_assertNot(equal({1,12}, {20, 50}))
  ].

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1,1)),
   ?_assertEqual(8, gcd(8,16)),
   ?_assertEqual(8, gcd(16,8))
  ].

multiply_test_() ->
  [
   ?_assertEqual({1,3}, multiply([{1,2}, {2,3}])),
   ?_assertEqual({1,16}, multiply([{1,2}, {1,4}, {5,10}])),
   ?_assertEqual({1,2}, multiply([{1,2}]))
  ].

reduce_test_() ->
  [
   ?_assertEqual({1,2}, reduce({4,8})),
   ?_assertEqual({1,3}, reduce({9,27})),
   ?_assertEqual({1,4}, reduce({25,100})),
   ?_assertEqual({1,2}, reduce({49, 98})),
   ?_assertEqual({3,5}, reduce({30,50}))
  ].

