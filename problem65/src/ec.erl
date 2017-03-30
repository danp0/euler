-module(ec).

-export(
   [
    c/1,
    e/1,
    gcd/2,
    main/0,
    reduce/1,
    seq/1,
    sum/2
   ]
  ).

c({A0, []}) ->
  A0;
c({A0, S}) ->
  [AN | T] = lists:reverse(S),
  sum(
    A0,
    lists:foldl(
      fun(A, Sum) ->
          {1, sum(A, Sum)}
      end,
      {1,AN},
      T)).

e(N) when N >= 1 ->
  c(seq(N-1)).

fraction(A) when is_integer(A) ->
  {A,1};
fraction({1,{A,B}}) ->
  {B,A};
fraction({A,B}) ->
  {A,B}.

gcd(A,A) ->
  A;
gcd(A,B) when A > B ->
  gcd(A-B,B);
gcd(A,B) when B > A ->
  gcd(A,B-A).

main() ->
  io:format("ec...~n", []),
  {N,D} = e(100),
  io:format("~p/~p = ~p~n", [N,D,N/D]),
  io:format("sum: ~p~n", 
            [lists:sum(
               lists:map(
                 fun(E) -> 
                     E - $0 
                 end, 
                 integer_to_list(N)))]),
  ok.

reduce({F1, F2}) ->
  G = gcd(F1, F2),
  A = F1 div G,
  B = F2 div G,
  if
    B =:= 1 ->
      A;
    true ->
      {F1 div G, F2 div G}
  end.

seq(N) when N >= 0 ->
  Sequence = 
  lists:flatten([[1,K,1] || K <- lists:seq(2, 2 * ((N+2) div 3), 2)]),
  {2, lists:sublist(Sequence, N)}.

sum(F1, F2) ->
  {A,B} = fraction(F1),
  {C,D} = fraction(F2),
  reduce({A*D + C*B, B*D}).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

c_test_() ->
  [
   ?_assertEqual(1, c({1, lists:duplicate(0,2)})),
   ?_assertEqual({3363,2378}, c({1, lists:duplicate(9,2)}))
  ].

e_test_() ->
  [
   ?_assertEqual(
      [2,
       3,
       {8,3},
       {11,4},
       {19,7},
       {87,32},
       {106,39},
       {193,71},
       {1264,465},
       {1457,536}], 
      [e(N) || N <- lists:seq(1,10)])
  ].

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1,2)),
   ?_assertEqual(2, gcd(2,4)),
   ?_assertEqual(3, gcd(3,9))
  ].

reduce_test_() ->
  [
   ?_assertEqual({1,2}, reduce({2,4})),
   ?_assertEqual({1,2}, reduce({1,2})),
   ?_assertEqual(3, reduce({9,3}))
  ].

seq_test_() ->
  [
   ?_assertEqual({2, []}, seq(0)),
   ?_assertEqual({2, [1]}, seq(1)),
   ?_assertEqual({2, [1,2]}, seq(2)),
   ?_assertEqual({2, [1,2,1]}, seq(3)),
   ?_assertEqual({2, [1,2,1,1]}, seq(4)),
   ?_assertEqual({2, [1,2,1,1,4]}, seq(5)),
   ?_assertEqual({2, [1,2,1,1,4,1]}, seq(6)),
   ?_assertEqual({2, [1,2,1,1,4,1,1]}, seq(7)),
   ?_assertEqual({2, [1,2,1,1,4,1,1,6]}, seq(8)),
   ?_assertEqual({2, [1,2,1,1,4,1,1,6,1]}, seq(9))
  ].

sum_test_() ->
  [
   ?_assertEqual(2, sum(1,1)),
   ?_assertEqual({3,2}, sum(1, {1,2})),
   ?_assertEqual({3,2}, sum({1,2}, 1)),
   ?_assertEqual({2,3}, sum({1,3}, {2,6})),
   ?_assertEqual({1,6}, sum({1,12}, {1,12})),
   ?_assertEqual(6, sum({1, {1,3}}, {1, {1,3}}))
  ].

-endif.
