-module(fraction).

-export(
   [
    between/3,
    compare/2,
    farey/1,
    farey_between/3,
    ge/2,
    gt/2,
    lt/2,
    main/0
   ]
  ).

between(N, From, To) ->
  lists:filter(
    fun({A,B}) ->
        gt({A,B}, From) andalso lt({A,B}, To)
    end,
    farey(N)).

compare({A,B}, {C,D}) ->
  AD = A*D,
  BC = B*C,
  if
    AD < BC -> -1;
    AD > BC ->  1;
    true    ->  0
  end.

farey(N) ->
  farey({0,1}, {1,N}, N, []).

farey({A,B}, {C,_D}, N, Acc) when C > N ->
  lists:reverse([{A,B} | Acc]);
farey({A,B}, {C,D}, N, Acc) ->
  K = (N + B) div D,
  farey({C,D}, {K*C - A, K*D - B}, N, [{A,B} | Acc]).

farey_between(N, From, To) ->
  farey_between({0,1}, {1,N}, N, From, To, 0).

farey_between({A,B}, {C,D}, N, From, To, Acc) ->
  case ge({A,B}, To) of
    true ->
      Acc;
    false ->
      NewAcc =
      case gt({A,B}, From) of
        true ->
          Acc + 1;
        false ->
          Acc
      end,
      K = (N + B) div D,
      farey_between({C,D}, {K*C - A, K*D - B}, N, From, To, NewAcc)
  end.

ge(F1, F2) ->
  compare(F1, F2) >= 0.

gt(F1, F2) ->
  compare(F1, F2) =:= 1.

lt(F1, F2) ->
  compare(F1, F2) =:= -1.

main() ->
  io:format("~p~n", [farey_between(12000, {1,3}, {1,2})]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

between_test_() ->
  [
   ?_assertEqual([{3,8}, {2,5}, {3,7}], between(8, {1,3}, {1,2}))
  ].

compare_test_() ->
  [
   ?_assertEqual(-1, compare({0,1}, {1,1})),
   ?_assertEqual( 1, compare({1,1}, {0,1})),
   ?_assertEqual( 0, compare({1,1}, {1,1})),
   ?_assertEqual(-1, compare({1,3}, {1,2})),
   ?_assertEqual( 1, compare({1,2}, {1,3})),
   ?_assertEqual( 0, compare({1,2}, {2,4}))
  ].

farey_test_() ->
  [
   ?_assertEqual([{0,1}, {1,1}], farey(1)),
   ?_assertEqual([{0,1}, {1,2}, {1,1}], farey(2)),
   ?_assertEqual([{0,1}, {1,3}, {1,2}, {2,3}, {1,1}], farey(3))
  ].

farey_between_test_() ->
  [
   ?_assertEqual(3, farey_between(8, {1,3}, {1,2}))
  ].

ge_test_() ->
  [
   ?_assert(ge({1,2}, {1,3})),
   ?_assertNot(ge({1,3}, {1,2})),
   ?_assert(ge({1,2}, {1,2}))
  ].

gt_test_() ->
  [
   ?_assert(gt({1,2}, {1,3})),
   ?_assertNot(gt({1,3}, {1,2})),
   ?_assertNot(gt({1,2}, {1,2}))
  ].

lt_test_() ->
  [
   ?_assertNot(lt({1,2}, {1,3})),
   ?_assert(lt({1,3}, {1,2})),
   ?_assertNot(lt({1,2}, {1,2}))
  ].

-endif.
