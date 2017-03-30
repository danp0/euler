-module(spiral).

%%
%%  dimension
%%  ---------
%%  0 1
%%  1 3
%%  2 5
%%  3 7
%%  4 9
%%  5 11
%%
%%  n 2n+1
%%
%%  diagonals
%%  ---------
%%  0  1  2  3  4 ... n
%%  1  7 21 43 73     (2n+1)^2 - 2n
%%  1  3 13 31 57     (2n)^2 + 1 -2n
%%  1  9 25 49 81     (2n+1)^2
%%  1  5 17 37 65     (2n)^2 + 1
%%
%%  walk
%%  ----
%%    {1,0} {0,1} {-1,0} {0,-1} {1,0}
%%  0   1     1      2     2      2
%%  1   1     3      4     4      4
%%  2   1     5      6     6      6
%%  3   1     7      8     8      8
%%
%%  n   1   2n+1   2n+2   2n+2   2n+2
%%

-export(
   [
    diagonal/2,
    diagonal_sum/1,
    diagonal_sum_from_iteration/1,
    dimension/1,
    main/0,
    print/1,
    split/2,
    walk/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

diagonal(Spiral, left) ->
  [{{X,Y}, N} || {{X,Y}, N} <- Spiral, X =:= Y];
diagonal(Spiral, right) ->
  [{{X,Y}, N} || {{X,Y}, N} <- Spiral, X =:= -Y].
  
diagonal_sum(Spiral) ->
  lists:sum([N || {{X,Y}, N} <- Spiral, X =:= Y orelse X =:= -Y]).
  
diagonal_sum_from_iteration(0) ->
  1;
diagonal_sum_from_iteration(N) ->
  TwoN = 2*N,
  TwoNPlus1 = TwoN + 1,
  TwoNSq = TwoN*TwoN,
  TwoNPlus1Sq = TwoNPlus1*TwoNPlus1,
  2 * TwoNPlus1Sq + 2 * TwoNSq - 2 * TwoN  + 2 + diagonal_sum_from_iteration(N-1).

dimension(N) ->
  2*N + 1.

main() ->
  N = 500,
  Spiral = walk(N),
  io:format("~p~n", [diagonal_sum(Spiral)]),
  io:format("~p~n", [diagonal_sum_from_iteration(N)]),
  ok.

print(N) ->
  lists:foreach(
    fun(R) ->
        io:format("~w~n", [lists:map(fun({{_,_}, V}) -> V end, R)])
    end,
    split(dimension(N), walk(N))).
 
split(N, L) ->
  split(N, L, []).

split(_N, [], Acc) ->
  lists:reverse(Acc);
split(N, L, Acc) ->
  {L1, L2} = lists:split(N, L),
  split(N, L2, [L1 | Acc]).

walk(N) when N >= 0 ->
  Points =
  lists:foldl(
    fun(Pass, Points) ->
        walk_perimeter(Pass, Points)
    end,
    [{{0,0}, 1}],
    lists:seq(1,N)),
  lists:sort(
    fun({{X1,Y1},_}, {{X2,Y2},_}) ->
        if
          Y1 =:= Y2 ->
            X1 =< X2;
          true ->
            Y1 =< Y2
        end
    end,
    Points).

walk_perimeter(N, Points) ->
  lists:foldl(
    fun({Count, Direction}, Acc) ->
        walk_path(Count, Direction, Acc)
    end,
    Points,
    [
     {1,     { 1, 0}},
     {2*N-1, { 0, 1}},
     {2*N,   {-1, 0}},
     {2*N,   { 0,-1}},
     {2*N,   { 1, 0}}
    ]
   ).

walk_path(0, _Direction, Acc) ->
  Acc;
walk_path(Count, {Dx,Dy}, [{{X,Y}, N} | T]) ->
  walk_path(Count - 1, {Dx, Dy}, [{{X+Dx, Y+Dy}, N+1}, {{X,Y}, N} | T]).

%%
%% unit test
%%
diagonal_test_() ->
  {setup,
   fun() -> walk(2) end,
   fun(_) -> ok end,
   fun(Spiral) ->
    [
      ?_assertEqual(
         [{{-2,-2}, 21}, {{-1,-1}, 7}, {{0,0}, 1}, {{1,1}, 3}, {{2,2}, 13}], 
         diagonal(Spiral, left)),
      ?_assertEqual(
         [{{2,-2}, 25}, {{1,-1}, 9}, {{0,0}, 1}, {{-1,1}, 5}, {{-2,2}, 17}], 
         diagonal(Spiral, right))
    ]
   end
  }.

diagonal_sum_test_() ->
  {setup,
   fun() -> walk(2) end,
   fun(_) -> ok end,
   fun(Spiral) ->
       [
        ?_assertEqual(101, diagonal_sum(Spiral))
       ]
   end
  }.

diagonal_sum_from_iteration_test_() ->
  [
   ?_assertEqual(1, diagonal_sum_from_iteration(0)),
   ?_assertEqual(25, diagonal_sum_from_iteration(1)),
   ?_assertEqual(101, diagonal_sum_from_iteration(2))
  ].

dimension_test_() ->
  [
   ?_assertEqual(1, dimension(0)),
   ?_assertEqual(3, dimension(1)),
   ?_assertEqual(5, dimension(2))
  ].

split_test_() ->
  [
   ?_assertEqual([[1,2,3], [4,5,6], [7,8,9]], split(3, [1,2,3,4,5,6,7,8,9]))
  ].

walk_test_() ->
  [
   ?_assertEqual([{{0,0},1}], walk(0)),
   ?_assertEqual([{{-1,-1},7}, {{0,-1},8}, {{1,-1},9}, 
                  {{-1, 0},6}, {{0, 0},1}, {{1, 0},2}, 
                  {{-1, 1},5}, {{0, 1},4}, {{1, 1},3}], walk(1))
  ].
