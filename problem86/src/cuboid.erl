-module(cuboid).

-export(
   [
    count/1,
    count_gt/1,
    is_square/1,
    main/0
   ]
  ).

count(M) ->
  length(
    [{A,B,C} || 
     A <- lists:seq(1,M), 
     B <- lists:seq(1,A), 
     C <- lists:seq(1,B),
     is_square(A*A + (B+C)*(B+C))]).

count_gt(N) ->
  count_gt(N, 1, 0).

count_gt(N, M, Sum) when Sum > N ->
  M - 1;
count_gt(N, M, Sum) ->
  M2 = M*M,
  count_gt(
    N,
    M+1,
    Sum +
    length(
      [{M,B,C} ||
       B <- lists:seq(1,M),
       C <- lists:seq(1,B),
       is_square(M2 + (B+C)*(B+C))])).

is_square(N) ->
  Sqrt = trunc(math:sqrt(N)),
  N =:= (Sqrt * Sqrt).

main() ->
  M = count_gt(1000000),
  io:format("~p~n", [M]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

count_test_() ->
  [
   ?_assertEqual(2060, count(100))
  ].

count_gt_test_() ->
  [
   ?_assertEqual(100, count_gt(2000))
  ].

is_square_test_() ->
  [
   ?_assertNot(is_square(2)),
   ?_assertNot(is_square(3)),
   ?_assert(is_square(4)),
   ?_assertNot(is_square(5)),
   ?_assertNot(is_square(6)),
   ?_assertNot(is_square(7)),
   ?_assertNot(is_square(8)),
   ?_assert(is_square(9))
  ].

-endif.
