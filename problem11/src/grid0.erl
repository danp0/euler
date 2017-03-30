-module(grid0).

-export(
   [
    diagonals/2,
    dimension/1,
    filter/1,
    get/3,
    main/0,
    max_product/2,
    max_product_partition/2,
    max_product_slice/2,
    order_by_col/1,
    order_by_bwd_diagonal/1,
    order_by_fwd_diagonal/1,
    order_by_row/1,
    partition/2,
    print/1,
    product/1,
    scan/1,
    slice/2
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

diagonals(DX, DY) ->
  Min = min(DX, DY),
  Remainder = ((DX * DY) - ((Min - 1) * Min)) div Min,
  lists:flatten([lists:seq(1,Min - 1), lists:duplicate(Remainder, Min), lists:seq(Min - 1,1, -1)]).

dimension(Grid) ->
  lists:foldl(
    fun({_E, {X, Y}}, {X1, Y1}) ->
        {max(X, X1), max(Y, Y1)}
    end,
    {1, 1},
    Grid).

filter(Grid) ->
  lists:map(
    fun(Partition) ->
        lists:map(
          fun({N, {_X, _Y}}) -> N end,
          Partition)
    end,
    Grid).

get(_X, _Y, _Grid) ->
  1.

main() ->
  io:format("grid...~n", []),
  RawGrid =
  [
    [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],
    [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
    [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
    [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
    [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
    [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
    [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
    [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
    [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
    [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
    [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
    [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
    [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
    [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
    [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
    [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
    [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
    [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
    [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
    [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]
  ],
  N = 4,
  Grid = scan(RawGrid),
  io:format("bwd: ~p~n", [max_product_partition(order_by_bwd_diagonal(Grid), N)]),
  print(filter(order_by_bwd_diagonal(Grid))),
  io:format("col: ~p~n", [max_product_partition(order_by_col(Grid), N)]),
  print(filter(order_by_col(Grid))),
  io:format("fwd: ~p~n", [max_product_partition(order_by_fwd_diagonal(Grid), N)]),
  print(filter(order_by_fwd_diagonal(Grid))),
  io:format("row:  ~p~n", [max_product_partition(order_by_row(Grid), N)]),
  print(filter(order_by_row(Grid))),
  io:format("~p~n", [max_product(Grid, N)]).

max_product(Grid, N) ->
  lists:max(
    [
      max_product_partition(order_by_bwd_diagonal(Grid), N),
      max_product_partition(order_by_col(Grid), N),
      max_product_partition(order_by_fwd_diagonal(Grid), N),
      max_product_partition(order_by_row(Grid), N)
    ]
   ).

max_product_partition(Grid, N) ->
  lists:foldl(
    fun(Parition, Max) ->
        max(max_product_slice(Parition, N), Max)
    end,
    0,
    Grid).

max_product_slice(L, N) ->
  lists:foldl(
    fun(Slice, Max) ->
        max(product(Slice), Max)
    end,
    0,
    slice(L, N)).

order_by_col(Grid) ->
  {_DX, DY} = dimension(Grid),
  Sorted =
  lists:sort(
    fun({_E1, {X1, Y1}}, {_E2, {X2, Y2}}) ->
        if
          X1  <  X2 -> true;
          X1  >  X2 -> false;
          X1 =:= X2 -> Y1 =< Y2
        end
    end,
    Grid), 
  partition(Sorted, DY).

get_root(DX, _DY, X, Y) ->
  if
    DX =:= X -> {X, Y};
    Y  =:= 1 -> {X, Y};
    true ->
      Diff = min(DX - X, Y - 1),
      {X + Diff, Y - Diff}
  end.

order_by_bwd_diagonal(Grid) ->
  {DX, DY} = dimension(Grid),
  Sorted =
  lists:sort(
    fun({_E1, {X1, Y1}}, {_E2, {X2, Y2}}) ->
        {RX1, RY1} = get_root(DX, DY, X1, Y1),
        {RX2, RY2} = get_root(DX, DY, X2, Y2),
        if
          {RX1, RY1} =:= {RX2, RY2} -> X1 >= X2;
          true -> X1 * Y1 >= X2 * Y2
        end
    end,
    Grid),
  partition(Sorted, diagonals(DX, DY)).

order_by_fwd_diagonal(Grid) ->
  {DX, DY} = dimension(Grid),
  Sorted =
  lists:sort(
    fun({_E1, {X1, Y1}}, {_E2, {X2, Y2}}) ->
        if
          Y1 - X1  >  Y2 - X2 -> true;
          Y1 - X1 =:= Y2 - X2 -> X1 =< X2;
          Y1 - X1  <  Y2 - X2 -> false
        end
    end,
    Grid),
  partition(Sorted, diagonals(DX, DY)).

order_by_row(Grid) ->
  {DX, _DY} = dimension(Grid),
  Sorted =
  lists:sort(
    fun({_E1, {X1, Y1}}, {_E2, {X2, Y2}}) ->
        if
          Y1  <  Y2 -> true;
          Y1  >  Y2 -> false;
          Y1 =:= Y2 -> X1 =< X2
        end
    end,
    Grid),
  partition(Sorted, DX).

partition([], _) ->
  [];
partition(L, []) ->
  L;
partition(L, [H | T]) ->
  {L1, L2} = lists:split(H, L),
  [L1 | partition(L2, T)];
partition(L, N) ->
  {L1, L2} = lists:split(N, L),
  [L1 | partition(L2, N)].

print(Grid) ->
  lists:foreach(fun(P) -> io:format("~w~n", [P]) end, Grid).

product(L) ->
  lists:foldl(
    fun({N, {_X, _Y}}, Product) ->
        N * Product
    end,
    1,
    L).

scan(Grid) ->
  YZip = lists:zip(Grid, lists:seq(1, length(Grid))),
  XYZip =
  lists:map(
    fun({L, Y}) ->
        lists:zipwith(
          fun(E, X) ->
              {E, {X, Y}}
          end,
          L,
          lists:seq(1, length(L)))
    end,
    YZip),
  lists:flatten(XYZip).

slice(L, N) ->
  slice(L, N, []).

slice(L, N, Slices) when length(L) < N ->
  lists:reverse(Slices);
slice([H|T], N, Slices) ->
  Slice = lists:sublist([H|T], N),
  slice(T, N, [Slice | Slices]).

%%
%% tests
%%
diagonals_test_() ->
  [
   ?_assertEqual([1], diagonals(1,1)),
   ?_assertEqual([1,2,1], diagonals(2,2)),
   ?_assertEqual([1,1], diagonals(2,1)),
   ?_assertEqual([1,1], diagonals(1,2)),
   ?_assertEqual([1,2,3,2,1], diagonals(3,3)),
   ?_assertEqual([1,2,3,4,3,2,1], diagonals(4,4)),
   ?_assertEqual([1,2,3,4,5,4,3,2,1], diagonals(5,5)),
   ?_assertEqual([1,2,2,2,2,1], diagonals(2,5)),
   ?_assertEqual([1,2,2,2,2,1], diagonals(5,2))
  ].

dimension_test_() ->
  [
   ?_assertEqual({3,3}, dimension([{1, {X, Y}} || X <- lists:seq(1,3), Y <- lists:seq(1,3)]))
  ].

max_product_partition_test_() ->
  [
   ?_assertEqual(72,
                 max_product_partition(order_by_row(scan([[1,2,3], [4,5,6], [7,8,9]])), 2)),
   ?_assertEqual(54,
                 max_product_partition(order_by_col(scan([[1,2,3], [4,5,6], [7,8,9]])), 2))
  ].

max_product_slice_test_() ->
  [
   ?_assertEqual(240, 
                 max_product_slice([{1, {1,1}}, {2, {1,2}}, {3, {1,3}}, {4, {1,4}}, {5, {1,5}}, {4, {1,6}}, {3, {1,7}}, {2, {1,8}}, {1, {1,9}}], 4))
  ].

max_product_test_() ->
  [
   ?_assertEqual(7*8*9,
                 max_product(scan([[1,2,3], [4,5,6], [7,8,9]]), 3))
  ].

order_by_bwd_diagonal_test_() ->
  [
   ?_assertEqual([[{9, {3,3}}], [{6, {3,2}}, {8, {2,3}}], [{3, {3,1}}, {5, {2,2}}, {7, {1,3}}], [{2, {2,1}}, {4, {1,2}}], [{1, {1,1}}]], 
                order_by_bwd_diagonal(scan([[1,2,3], [4,5,6], [7,8,9]])))
  ].

order_by_col_test_() ->
  [
   ?_assertEqual([[{1, {1,1}}, {4, {1,2}}, {7, {1,3}}], [{2, {2,1}}, {5, {2,2}}, {8, {2,3}}], [{3, {3,1}}, {6, {3,2}}, {9, {3,3}}]],
                 order_by_col(scan([[1,2,3], [4,5,6], [7,8,9]])))
  ].

order_by_fwd_diagonal_test_() ->
  [
   ?_assertEqual([[{7, {1,3}}], [{4, {1,2}}, {8, {2,3}}], [{1, {1,1}}, {5, {2,2}}, {9, {3,3}}], [{2, {2,1}}, {6, {3,2}}], [{3, {3,1}}]], 
                 order_by_fwd_diagonal(scan([[1,2,3], [4,5,6], [7,8,9]])))
  ].

order_by_row_test_() ->
  [
   ?_assertEqual([[{1, {1,1}}, {2, {2,1}}, {3, {3,1}}], [{4, {1,2}}, {5, {2,2}}, {6, {3,2}}], [{7, {1,3}}, {8, {2,3}}, {9, {3,3}}]],
                 order_by_row(scan([[1,2,3], [4,5,6], [7,8,9]])))
  ].

partition_test_() ->
  [
   ?_assertEqual([[1,2,3], [4,5,6], [7,8,9]], partition(lists:seq(1, 9), 3)),
   ?_assertEqual([[1], [2,3], [4,5,6], [7,8,9,10]], partition(lists:seq(1,10), [1, 2, 3, 4]))
  ].

product_test_() ->
  [
   ?_assertEqual(0, product([{0, {1,1}}])),
   ?_assertEqual(120, product([{1, {1,1}}, {2, {2,1}}, {3, {3,1}}, {4, {4,1}}, {5, {5,1}}]))
  ].

scan_test_() ->
  Grid3x3 = lists:sort([{X+3*(Y-1), {X,Y}} || X <- lists:seq(1,3), Y <- lists:seq(1,3)]),
  Grid2x5 = lists:sort([{X+5*(Y-1), {X,Y}} || X <- lists:seq(1,5), Y <- lists:seq(1,2)]),
  [
   ?_assertEqual(Grid3x3, scan([[1,2,3],[4,5,6],[7,8,9]])),
   ?_assertEqual(Grid2x5, scan([[1,2,3,4,5], [6,7,8,9,10]]))
  ].

slice_test_() ->
  [
   ?_assertEqual([], slice([], 2)),
   ?_assertEqual([[1,2], [2,3]], slice([1,2,3], 2))
  ].
