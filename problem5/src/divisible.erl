-module(divisible).

-export(
   [
    difference/2,
    main/0,
    merge/2,
    multiply/1,
    smallest_multiple/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

difference(L1, L2) ->
  lists:reverse(difference(L1, L2, [])).

difference([], _, Difference) ->
  Difference;
difference([H|T], [], Difference) ->
  difference(T, [], [H | Difference]);
difference([H|T1], [H|T2], Difference) ->
  difference(T1, T2, Difference);
difference([H1|T1], [H2|T2], Difference) ->
  if
    H1 < H2 ->
      difference(T1, [H2|T2], [H1|Difference]);
    H1 > H2 ->
      difference([H1|T1], T2, Difference)
  end.

merge(L1, L2) ->
  lists:reverse(merge(L1, L2, [])).

merge([], [], Acc) ->
  Acc;
merge([], [H | T], Acc) ->
  merge([], T, [H | Acc]);
merge([H | T], [], Acc) ->
  merge(T, [], [H | Acc]);
merge([H1 | T1], [H2 | T2], Acc) ->
  if
    H1 < H2 ->
      merge(T1, [H2 | T2], [H1 | Acc]);
    H1 =:= H2 ->
      merge(T1, T2, [H1, H2 | Acc]);
    H1 > H2 ->
      merge([H1 | T1], T2, [H2 | Acc])
  end.

multiply([]) ->
  1;
multiply([H|T]) ->
  H * multiply(T).

smallest_multiple(L) ->
  multiply(
    lists:foldl(
      fun(E, Acc) ->
          merge(difference(E, Acc), Acc)
      end,
      [],
      L)).

main() ->
  Factors1_10 = [[1],[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3],[2,5]],
  Factors1_20 = [[1],[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3],[2,5],[11],[2,2,3],[13],[2,7],[3,5],[2,2,2,2],[17],[2,3,3],[19],[2,2,5]],
  io:format("~w~n", [Factors1_10]),
  io:format("~w~n", [smallest_multiple(Factors1_10)]),
  io:format("~w~n", [Factors1_20]),
  io:format("~w~n", [smallest_multiple(Factors1_20)]).

%%
%% tests
%%
difference_test_() ->
  [
   ?_assert([] =:= difference([], [])),
   ?_assert([] =:= difference([2], [2])),
   ?_assert([3, 11] =:= difference([3, 3, 3, 5, 7, 11], [2, 2, 2, 3, 3, 5, 7])),
   ?_assert([2, 3, 5] =:= difference([2, 3, 5], [])),
   ?_assert([] =:= difference([], [2, 3, 5])),
   ?_assert([2, 3, 5] =:= difference([2, 2, 3, 3, 5, 5], [2, 3, 5])),
   ?_assert([2] =:= difference([2, 2, 2, 2, 3, 5], [2, 2, 2, 3, 5]))
  ].

merge_test_() ->
  [
   ?_assert([] =:= merge([], [])),
   ?_assert([1, 1] =:= merge([1], [1])),
   ?_assert([2, 2, 2, 3, 3, 5, 5, 7, 7, 9] =:= merge([2, 2, 3, 5, 7], [2, 3, 5, 7, 9])),
   ?_assert([2, 3] =:= merge([2, 3], [])),
   ?_assert([2, 3] =:= merge([], [2, 3])),
   ?_assert([2, 3, 5, 7, 9] =:= merge([2, 3], [5, 7, 9])),
   ?_assert([2, 3, 5, 7, 9] =:= merge([5, 7, 9], [2, 3]))
  ].

multiply_test_() ->
  [
   ?_assert(1 =:= multiply([1])),
   ?_assert(2 =:= multiply([1, 2])),
   ?_assert(6 =:= multiply([1, 2, 3])),
   ?_assert(30 =:= multiply([1, 2, 3, 5]))
  ].
