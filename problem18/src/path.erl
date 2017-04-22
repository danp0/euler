-module(path).

-export(
   [
    main/0,
    pair_off/1,
    scan/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% In the triangle below, find the maximum path from top to bottom
% by moving to adjacent numbers below.
%

main() ->
  Triangle =
  [
    [75],
    [95, 64],
    [17, 47, 82],
    [18, 35, 87, 10],
    [20, 04, 82, 47, 65],
    [19, 01, 23, 75, 03, 34],
    [88, 02, 77, 73, 07, 63, 67],
    [99, 65, 04, 28, 06, 16, 70, 92],
    [41, 41, 26, 56, 83, 40, 80, 70, 33],
    [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
    [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
    [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
    [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
    [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
    [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
  ],
  io:format("Max: ~p~n", [scan(Triangle)]).

pair_off(L) ->
  pair_off(L, []).

pair_off([E1, E2 | T], Acc) ->
  pair_off([E2 | T], [{E1, E2} | Acc]);
pair_off(_L, Acc) ->
  lists:reverse(Acc).

scan(Triangle) ->
  Flipped = lists:reverse(Triangle),
  [Max] =
  lists:foldl(
    fun
      (Row, []) ->
        Row;
      (Row2, Row1) ->
        Pairs = pair_off(Row1),
        lists:map(fun({A, {B, C}}) -> A + max(B,C) end, lists:zip(Row2, Pairs))
    end,
    [],
    Flipped),
  Max.

%%
%% unit tests
%%
pair_off_test_() ->
  [
   ?_assertEqual([], pair_off([])),
   ?_assertEqual([], pair_off([1])),
   ?_assertEqual([{1,2}], pair_off([1, 2])),
   ?_assertEqual([{1,2}, {2,3}], pair_off([1, 2, 3])),
   ?_assertEqual([{1,2}, {2,3}, {3,4}], pair_off([1, 2, 3, 4]))
  ].

scan_test_() ->
  [
   ?_assertEqual(1,  scan([[1]])),
   ?_assertEqual(4,  scan([[1], [2,3]])),
   ?_assertEqual(10, scan([[1], [2,3], [4,5,6]])),
   ?_assertEqual(9,  scan([[1], [3,2], [5,4,6]])),
   ?_assertEqual(23, scan([[3],[7,4],[2,4,6],[8,5,9,3]]))
  ].
