-module(sudoku).

-export(
   [
    box/2,
    box_of/1,
    col/2,
    free_cells/1,
    main/0,
    rank/2,
    rank_cells/1,
    read_puzzles/1,
    replace/2,
    row/2,
    sort_by_box/1,
    sort_by_col/1,
    sort_by_rank/1,
    sort_by_row/1,
    solve/1,
    solved/1
   ]
  ).

-define(SUDOKU_FILE, "p096_sudoku.txt").

%%
%%  1 2 3 4 5 6 7 8 9
%% 1     |     | 
%% 2     |     |
%% 3  1  |  2  |  3
%%  -----------------
%% 4     |     |
%% 5     |     |
%% 6  4  |  5  |  6
%%  -----------------
%% 7     |     |
%% 8     |     |
%% 9  7  |  8  |  9
%%

box({Col, Row}, Puzzle) ->
  N = box_of({Col, Row}),
  box(N, Puzzle);
box(N, Puzzle) ->
  Sublist = lists:sublist(Puzzle, 3 * ((N - 1) div 3) + 1, 3),
  lists:reverse(
    lists:foldl(
      fun(L, Acc) ->
          [lists:sublist(L, 3 * ((N - 1) rem 3) + 1, 3) | Acc]
      end,
      [],
      Sublist)).

box_of({Col, Row}) ->
  3 * ((Row - 1) div 3) + ((Col - 1) div 3) + 1.

col({Col, _Row}, Puzzle) ->
  col(Col, Puzzle);
col(Col, Puzzle) ->
  lists:reverse(
    lists:foldl(
      fun(Row, Acc) ->
          [lists:nth(Col, Row) | Acc]
      end,
      [],
      Puzzle)).

free_cells(Puzzle) ->
  lists:map(
    fun({I, _E}) ->
        {((I-1) rem 9) + 1, ((I-1) div 9) + 1}
    end,
    lists:filter(
      fun({_I, E}) ->
          E =:= 0
      end,
      lists:zip(lists:seq(1,81), lists:flatten(Puzzle)))).

main() ->
  Puzzles = read_puzzles(?SUDOKU_FILE),
  Solutions = 
  lists:map(
    fun({Heading, Puzzle}) ->
        {Heading, Puzzle, solve(Puzzle)}
    end,
    Puzzles),
  Corners =
  lists:map(
    fun({_, _, S}) ->
        list_to_integer([D + $0 || D <- hd(box(1,S))])
    end,
    Solutions),
  io:format("~p~n", [lists:sum(Corners)]),
  ok.

match_line(Line) ->
  re:run(
    Line, 
    "^(?:Grid (\\d{2}))\|(?:(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)(\\d)(\\d))$", 
    [{capture, all_but_first, list}]).

rank(At, Puzzle) ->
  lists:seq(1,9) -- 
  lists:flatten([row(At, Puzzle), col(At, Puzzle), box(At, Puzzle)]).

rank_cells(Puzzle) ->
  sort_by_rank(
    lists:map(
      fun(At) ->
          {At, rank(At, Puzzle)}
      end,
      free_cells(Puzzle))).

read_puzzles(Filename) ->
  {ok, File} = file:read_file(Filename),
  Lines =
  re:split(File, "\n", [{return, list}, trim]),
  Puzzles =
  lists:reverse(
    lists:map(
      fun(Puzzle) ->
          [Heading | Digits] = lists:reverse(Puzzle),
          {
           Heading,
           lists:map(
             fun(Line) ->
                 lists:map(
                   fun(D) ->
                       list_to_integer(D)
                   end,
                   Line)
             end,
            Digits)
          }
      end,
      lists:foldl(
        fun(Line, Acc) ->
            case match_line(Line) of
              {match, [Grid]} ->
                [[{grid, Grid}]|Acc];
              {match, [[] | Digits]} ->
                [[Digits|hd(Acc)]|tl(Acc)]
            end
        end,
        [],
        Lines))),
  Puzzles.

replace(Cells, Puzzle) ->
  {_, FlatPuzzle} =
  lists:unzip(
    lists:umerge(
      fun({{Col1,Row1},_R1}, {{Col2,Row2},_R2}) ->
          {Row1, Col1} =< {Row2, Col2}
      end,
      sort_by_row(Cells),
      sort_by_row(
        lists:map(
          fun({I, E}) ->
              {{((I-1) rem 9) + 1, ((I-1) div 9) + 1}, [E]}
          end,
          lists:zip(lists:seq(1,81), lists:flatten(Puzzle)))))
   ),
  {[], Puzzle1} =
  lists:foldl(
    fun(_, {L, Acc}) ->
        {L1, L2} = lists:split(9, L),
        {L2, [L1|Acc]}
    end,
    {lists:flatten(FlatPuzzle),[]},
    lists:seq(1,9)),
  lists:reverse(Puzzle1).

row({_Col, Row}, Puzzle) ->
  row(Row, Puzzle);
row(Row, Puzzle) ->
  lists:nth(Row, Puzzle).

sort_by_box(Ranks) ->
  lists:sort(
    fun({At1, _Rank1}, {At2, _Rank2}) ->
        B1 = box_of(At1),
        B2 = box_of(At2),
        case B1 =:= B2 of
          true ->
            At1 =< At2;
          false ->
            B1 =< B2
        end
    end,
    Ranks).

sort_by_col(Ranks) ->
  lists:sort(Ranks).

sort_by_rank(Ranks) ->
  lists:sort(
    fun({_At1, Rank1}, {_At2, Rank2}) ->
        length(Rank1) =< length(Rank2)
    end,
    Ranks).

sort_by_row(Ranks) ->
  lists:sort(
    fun({{Col1,Row1},_R1}, {{Col2,Row2},_R2}) ->
        {Row1, Col1} =< {Row2, Col2}
    end,
    Ranks).

solve(Puzzle) ->
  case solve(rank_cells(Puzzle), []) of
    false ->
      false;
    Cells ->
      replace(Cells, Puzzle)
  end.

solve([], Acc) ->
  Acc;
solve([{_At, Rank}|_Tail], _Acc) when length(Rank) =:= 0 ->
  false;
solve([{At, Rank}|Tail], Acc) when length(Rank) =:= 1 ->
  {Col, Row} = At,
  Box = box_of(At),
  Ranks =
  lists:map(
    fun({{Col1, Row1}, Rank1}) ->
        Box1 = box_of({Col1, Row1}),
        if Col =:= Col1 orelse
           Row =:= Row1 orelse
           Box =:= Box1 ->
             {{Col1, Row1}, Rank1 -- Rank};
           true ->
             {{Col1, Row1}, Rank1}
        end
    end,
    Tail),
  SortedRanks = sort_by_rank(Ranks),
  solve(SortedRanks, [{At, Rank}|Acc]);
solve([{At, Rank}|Tail], Acc) ->
  Solutions = 
  lists:filter(
    fun(S) -> S =/= false end,
    lists:map(
      fun(R) -> 
          solve([{At, [R]}|Tail], Acc)
      end, 
      Rank)),
  if 
    length(Solutions) =:= 1 ->
      hd(Solutions);
    true ->
      false
  end.

solved(Puzzle) ->
  Seq = lists:seq(1,9),
  lists:all(
    fun(N) ->
        lists:sort(row(N, Puzzle)) =:= Seq andalso
        lists:sort(col(N, Puzzle)) =:= Seq andalso
        lists:sort(lists:flatten(box(N, Puzzle))) =:= Seq
    end,
    Seq).
        
%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

box_test_() ->
  Puzzle =
  [
   [4,8,3, 9,2,1, 6,5,7],
   [9,6,7, 3,4,5, 8,2,1],
   [2,5,1, 8,7,6, 4,9,3],

   [5,4,8, 1,3,2, 9,7,6],
   [7,2,9, 5,6,4, 1,3,8],
   [1,3,6, 7,9,8, 2,4,5],

   [3,7,2, 6,8,9, 5,1,4],
   [8,1,4, 2,5,3, 7,6,9],
   [6,9,5, 4,1,7, 3,8,2]
  ],
  [
   ?_assertEqual(box(1, Puzzle), box({1,1}, Puzzle)),
   ?_assertEqual(box(2, Puzzle), box({4,2}, Puzzle)),
   ?_assertEqual(box(3, Puzzle), box({7,3}, Puzzle)),
   ?_assertEqual(box(4, Puzzle), box({2,4}, Puzzle)),
   ?_assertEqual(box(5, Puzzle), box({5,5}, Puzzle)),
   ?_assertEqual(box(6, Puzzle), box({8,6}, Puzzle)),
   ?_assertEqual(box(7, Puzzle), box({3,7}, Puzzle)),
   ?_assertEqual(box(8, Puzzle), box({6,8}, Puzzle)),
   ?_assertEqual(box(9, Puzzle), box({9,9}, Puzzle)),
   ?_assertEqual([[4,8,3],[9,6,7],[2,5,1]], box(1, Puzzle)),
   ?_assertEqual([[9,2,1],[3,4,5],[8,7,6]], box(2, Puzzle)),
   ?_assertEqual([[6,5,7],[8,2,1],[4,9,3]], box(3, Puzzle)),
   ?_assertEqual([[5,4,8],[7,2,9],[1,3,6]], box(4, Puzzle)),
   ?_assertEqual([[1,3,2],[5,6,4],[7,9,8]], box(5, Puzzle)),
   ?_assertEqual([[9,7,6],[1,3,8],[2,4,5]], box(6, Puzzle)),
   ?_assertEqual([[3,7,2],[8,1,4],[6,9,5]], box(7, Puzzle)),
   ?_assertEqual([[6,8,9],[2,5,3],[4,1,7]], box(8, Puzzle)),
   ?_assertEqual([[5,1,4],[7,6,9],[3,8,2]], box(9, Puzzle))
  ].

box_of_test_() ->
  [
   ?_assertEqual(1, box_of({1,1})),
   ?_assertEqual(1, box_of({2,1})),
   ?_assertEqual(1, box_of({1,2})),
   ?_assertEqual(2, box_of({6,3})),
   ?_assertEqual(3, box_of({8,2})),
   ?_assertEqual(5, box_of({5,5}))
  ].

col_test_() ->
  Puzzle =
  [
   [4,8,3, 9,2,1, 6,5,7],
   [9,6,7, 3,4,5, 8,2,1],
   [2,5,1, 8,7,6, 4,9,3],

   [5,4,8, 1,3,2, 9,7,6],
   [7,2,9, 5,6,4, 1,3,8],
   [1,3,6, 7,9,8, 2,4,5],

   [3,7,2, 6,8,9, 5,1,4],
   [8,1,4, 2,5,3, 7,6,9],
   [6,9,5, 4,1,7, 3,8,2]
  ],
  [
   ?_assertEqual([4,9,2,5,7,1,3,8,6], col(1, Puzzle)),
   ?_assertEqual([8,6,5,4,2,3,7,1,9], col(2, Puzzle)),
   ?_assertEqual([3,7,1,8,9,6,2,4,5], col(3, Puzzle)),
   ?_assertEqual([9,3,8,1,5,7,6,2,4], col(4, Puzzle)),
   ?_assertEqual([2,4,7,3,6,9,8,5,1], col(5, Puzzle)),
   ?_assertEqual([1,5,6,2,4,8,9,3,7], col(6, Puzzle)),
   ?_assertEqual([6,8,4,9,1,2,5,7,3], col(7, Puzzle)),
   ?_assertEqual([5,2,9,7,3,4,1,6,8], col(8, Puzzle)),
   ?_assertEqual([7,1,3,6,8,5,4,9,2], col(9, Puzzle))
  ].


free_cells_test_() ->
  Puzzle =
  [
   [0,0,3,0,2,0,6,0,0],
   [9,0,0,3,0,5,0,0,1],
   [0,0,1,8,0,6,4,0,0],
   [0,0,8,1,0,2,9,0,0],
   [7,0,0,0,0,0,0,0,8],
   [0,0,6,7,0,8,2,0,0],
   [0,0,2,6,0,9,5,0,0],
   [8,0,0,2,0,3,0,0,9],
   [0,0,5,0,1,0,3,0,0]
  ],
  [
   ?_assertEqual({8,1}, lists:nth(5, free_cells(Puzzle))),
   ?_assertEqual({7,2}, lists:nth(10, free_cells(Puzzle)))
  ].

rank_test_() ->
  Puzzle =
  [
   [0,0,3,0,2,0,6,0,0],
   [9,0,0,3,0,5,0,0,1],
   [0,0,1,8,0,6,4,0,0],
   [0,0,8,1,0,2,9,0,0],
   [7,0,0,0,0,0,0,0,8],
   [0,0,6,7,0,8,2,0,0],
   [0,0,2,6,0,9,5,0,0],
   [8,0,0,2,0,3,0,0,9],
   [0,0,5,0,1,0,3,0,0]
  ],
  [
   ?_assertEqual([4,5], rank({1,1}, Puzzle)),
   ?_assertEqual([4,6], rank({1,9}, Puzzle))
  ].

rank_cells_test_() ->
  Puzzle =
  [
   [0,0,3,0,2,0,6,0,0],
   [9,0,0,3,0,5,0,0,1],
   [0,0,1,8,0,6,4,0,0],
   [0,0,8,1,0,2,9,0,0],
   [7,0,0,0,0,0,0,0,8],
   [0,0,6,7,0,8,2,0,0],
   [0,0,2,6,0,9,5,0,0],
   [8,0,0,2,0,3,0,0,9],
   [0,0,5,0,1,0,3,0,0]
  ],
  [
   ?_assertEqual({{6,5}, [4]}, lists:nth(1,rank_cells(Puzzle))),
   ?_assertEqual({{7,5}, [1]}, lists:nth(2,rank_cells(Puzzle)))
  ].

row_test_() ->
  Puzzle =
  [
   [4,8,3, 9,2,1, 6,5,7],
   [9,6,7, 3,4,5, 8,2,1],
   [2,5,1, 8,7,6, 4,9,3],

   [5,4,8, 1,3,2, 9,7,6],
   [7,2,9, 5,6,4, 1,3,8],
   [1,3,6, 7,9,8, 2,4,5],

   [3,7,2, 6,8,9, 5,1,4],
   [8,1,4, 2,5,3, 7,6,9],
   [6,9,5, 4,1,7, 3,8,2]
  ],
  [
   ?_assertEqual([4,8,3,9,2,1,6,5,7], row(1, Puzzle)),
   ?_assertEqual([9,6,7,3,4,5,8,2,1], row(2, Puzzle)),
   ?_assertEqual([2,5,1,8,7,6,4,9,3], row(3, Puzzle)),
   ?_assertEqual([5,4,8,1,3,2,9,7,6], row(4, Puzzle)),
   ?_assertEqual([7,2,9,5,6,4,1,3,8], row(5, Puzzle)),
   ?_assertEqual([1,3,6,7,9,8,2,4,5], row(6, Puzzle)),
   ?_assertEqual([3,7,2,6,8,9,5,1,4], row(7, Puzzle)),
   ?_assertEqual([8,1,4,2,5,3,7,6,9], row(8, Puzzle)),
   ?_assertEqual([6,9,5,4,1,7,3,8,2], row(9, Puzzle))
  ].

solve_test_() ->
  Puzzle =
  [
   [0,0,3, 0,2,0, 6,0,0],
   [9,0,0, 3,0,5, 0,0,1],
   [0,0,1, 8,0,6, 4,0,0],

   [0,0,8, 1,0,2, 9,0,0],
   [7,0,0, 0,0,0, 0,0,8],
   [0,0,6, 7,0,8, 2,0,0],

   [0,0,2, 6,0,9, 5,0,0],
   [8,0,0, 2,0,3, 0,0,9],
   [0,0,5, 0,1,0, 3,0,0]
  ],
  Solution =
  [
   [4,8,3, 9,2,1, 6,5,7],
   [9,6,7, 3,4,5, 8,2,1],
   [2,5,1, 8,7,6, 4,9,3],

   [5,4,8, 1,3,2, 9,7,6],
   [7,2,9, 5,6,4, 1,3,8],
   [1,3,6, 7,9,8, 2,4,5],

   [3,7,2, 6,8,9, 5,1,4],
   [8,1,4, 2,5,3, 7,6,9],
   [6,9,5, 4,1,7, 3,8,2]
  ],
  [
   ?_assertEqual(Solution, solve(Puzzle))
  ].

solved_test_() ->
  Puzzle =
  [
   [0,0,3, 0,2,0, 6,0,0],
   [9,0,0, 3,0,5, 0,0,1],
   [0,0,1, 8,0,6, 4,0,0],

   [0,0,8, 1,0,2, 9,0,0],
   [7,0,0, 0,0,0, 0,0,8],
   [0,0,6, 7,0,8, 2,0,0],

   [0,0,2, 6,0,9, 5,0,0],
   [8,0,0, 2,0,3, 0,0,9],
   [0,0,5, 0,1,0, 3,0,0]
  ],
  Solution =
  [
   [4,8,3, 9,2,1, 6,5,7],
   [9,6,7, 3,4,5, 8,2,1],
   [2,5,1, 8,7,6, 4,9,3],

   [5,4,8, 1,3,2, 9,7,6],
   [7,2,9, 5,6,4, 1,3,8],
   [1,3,6, 7,9,8, 2,4,5],

   [3,7,2, 6,8,9, 5,1,4],
   [8,1,4, 2,5,3, 7,6,9],
   [6,9,5, 4,1,7, 3,8,2]
  ],
  [
   ?_assertNot(solved(Puzzle)),
   ?_assert(solved(Solution))
  ].

-endif.
