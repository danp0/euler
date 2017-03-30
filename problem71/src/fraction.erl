-module(fraction).

-export(
   [
    before/2,
    before_range/3,
    compare/2,
    equal/2,
    gcd/2,
    less_than/2,
    main/0,
    maxof/2,
    reduce/1
   ]
  ).

before(D, F) ->
  before({1, D}, {D-1, D}, F).

before({N1, D} = From, {N2, D} = To, F) ->
  Pivot = {(N1 + N2) div 2, D},
  case equal(Pivot, From) orelse equal(Pivot, To) of
    true ->
      From;
    false ->
      case less_than(Pivot, F) of
        true ->
          before(Pivot, To, F);
        false ->
          before(From, Pivot, F)
      end
  end.

before_range(DFrom, DTo, F) ->
  before_range(DFrom, DTo, F, {0, 1}).

before_range(DFrom, DTo, _F, Before) when DFrom > DTo ->
  reduce(Before);
before_range(DFrom, DTo, F, Before) ->
  before_range(DFrom + 1, DTo, F, maxof(Before, before(DFrom, F))).

compare({A, B}, {C, D}) ->
  AD = A*D,
  BC = B*C,
  if
    AD < BC ->
      -1;
    AD > BC ->
      1;
    true ->
      0
  end.

equal(F1, F2) ->
  compare(F1, F2) =:= 0.

gcd(A, A) ->
  A;
gcd(A, B) when A > B ->
  gcd(A - B, B);
gcd(A,B) ->
  gcd(A, B - A).

less_than(F1, F2) ->
  compare(F1, F2) =:= -1.

main() ->
  io:format("~p~n", [before_range(3, 1000000, {3,7})]),
  ok.

maxof(F1, F2) ->
  case less_than(F1, F2) of
    true ->
      F2;
    false ->
      F1
  end.

reduce({A,B}) ->
  Gcd = gcd(A,B),
  {A div Gcd, B div Gcd}.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

before_test_() ->
  [
   ?_assertEqual(
      [{1,3}, {1,4}, {2,5}, {2,6}, {2,7}, {3,8}],
      [before(D, {3,7}) || D <- lists:seq(3,8)])
  ].

before_range_test_() ->
  [
   ?_assertEqual({2,5}, before_range(3, 8, {3,7}))
  ].

compare_test_() ->
  [
   ?_assertEqual(-1, compare({0,1}, {1,2})),
   ?_assertEqual( 0, compare({0,1}, {0,1})),
   ?_assertEqual( 0, compare({0,1}, {0,2})),
   ?_assertEqual( 0, compare({3,7}, {3,7})),
   ?_assertEqual(-1, compare({2,5}, {3,7})),
   ?_assertEqual( 1, compare({3,7}, {2,5}))
  ].

equal_test_() ->
  [
   ?_assert(equal({1,2}, {2,4})),
   ?_assert(equal({1,2}, {1,2})),
   ?_assertNot(equal({1,2}, {1,3}))
  ].

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1, 8)),
   ?_assertEqual(2, gcd(2, 8)),
   ?_assertEqual(3, gcd(9, 15))
  ].

less_than_test_() ->
  [
   ?_assert(less_than({1,3}, {1,2})),
   ?_assertNot(less_than({1,2}, {1,3})),
   ?_assertNot(less_than({1,2}, {1,2}))
  ].

maxof_test_() ->
  [
   ?_assertEqual({1,2}, maxof({1,2}, {1,3})),
   ?_assertEqual({1,2}, maxof({1,3}, {1,2})),
   ?_assertEqual({1,2}, maxof({1,2}, {1,2}))
  ].

reduce_test_() ->
  [
   ?_assertEqual({1,2}, reduce({6,12})),
   ?_assertEqual({1,3}, reduce({3,9})),
   ?_assertEqual({2,3}, reduce({6,9}))
  ].

-endif.
