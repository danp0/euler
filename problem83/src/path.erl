-module(path).

-export(
   [
    current/0,
    init/1,
    main/0,
    mdim/1,
    mget/2,
    min_path/1,
    path_sum/2,
    neighbors/1,
    previous/1,
    read_matrix_file/1,
    set_distance/2,
    set_previous/2,
    set_visited/1,
    start/1,
    stop/0
   ]
  ).

-record(
   state,
   {
    distance = gb_trees:empty(),
    unvisited = gb_sets:empty(),
    previous = gb_trees:empty()
   }
  ).

-define(MFILE, "p083_matrix.txt").

current() ->
  ?MODULE ! {current, self()},
  receive
    Result ->
      Result
  end.

init({Rows, Cols}) ->
  State =
  #state{
     distance = 
     gb_trees:from_orddict(
       [{{1, 1}, 0} |
       [{{Row, Col}, infinite} || 
        Row <- lists:seq(1,Rows), 
        Col <- lists:seq(1,Cols), 
        Row =/= 1 orelse Col =/= 1]]),
     unvisited =
     gb_sets:from_list(
       [{Row, Col} ||
        Row <- lists:seq(1,Rows),
        Col <- lists:seq(1,Cols)])
    },
  loop(State).

loop(State) ->
  NewState =
  receive
    {current, From} ->
      case gb_sets:is_empty(State#state.unvisited) of
        true ->
          From ! none;
        false ->
          {Distance, Coordinate} =
          lists:min(
            gb_sets:fold(
              fun(Element, Acc) ->
                  [{gb_trees:get(Element, State#state.distance), Element} |
                   Acc]
              end,
              [],
              State#state.unvisited)),
          From ! {Coordinate, Distance}
      end,
      State;
    {neighbors, {CurRow, CurCol}, From} ->
      From !
      [{Element, gb_trees:get(Element, State#state.distance)} ||
       Element <- lists:filter(
                    fun({Row, Col}) ->
                        Difference = {abs(CurRow - Row), abs(CurCol - Col)},
                        Difference =:= {1, 0} orelse
                        Difference =:= {0, 1}
                    end,
                    gb_sets:to_list(State#state.unvisited))],
      State;
    {previous, Coordinate, From} ->
      From ! gb_trees:lookup(Coordinate, State#state.previous),
      State;
    {set_distance, Coordinate, Distance, From} ->
      Distances = gb_trees:enter(Coordinate, Distance, State#state.distance),
      From ! ok,
      State#state{distance = Distances};
    {set_previous, Neighbor, Current, From} ->
      Previous = gb_trees:enter(Neighbor, Current, State#state.previous),
      From ! ok,
      State#state{previous = Previous};
    {set_visited, Coordinate, From} ->
      Unvisited = gb_sets:delete(Coordinate, State#state.unvisited),
      From ! ok,
      State#state{
        unvisited = Unvisited}; 
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  Matrix = read_matrix_file(?MFILE),
  MinPath = min_path(Matrix),
  Sum = path_sum(MinPath, Matrix),
  io:format("sum: ~p~n", [Sum]),
  ok.

mdim([]) ->
  {0, 0};
mdim(Matrix) ->
  {length(Matrix), length(hd(Matrix))}.

mget({Row, Col}, Matrix) ->
  lists:nth(Col, lists:nth(Row, Matrix)).

min_path(Matrix) ->
  Dimension = mdim(Matrix),
  start(Dimension),
  MinPath = min_path(Matrix, current(), Dimension),
  ok = stop(),
  MinPath.

min_path(_Matrix, none, {Rows, Cols}) ->
  shortest_path({value, {Rows, Cols}}, []);
min_path(Matrix, {{Row, Col}, Distance}, {Rows, Cols}) ->
  Neighbors = neighbors({Row, Col}),
  lists:foreach(
    fun({{NRow, NCol}, NDistance}) ->
        Neighbor = mget({NRow, NCol}, Matrix),
        case Distance + Neighbor < NDistance of
          true ->
            set_previous({NRow, NCol}, {Row, Col}),
            set_distance({NRow, NCol}, Distance + Neighbor);
          false ->
            skip
        end
    end,
    Neighbors),
  set_visited({Row, Col}),
  min_path(Matrix, current(), {Rows, Cols}).

path_sum(Path, Matrix) ->
  lists:sum(
    lists:map(
      fun(Coord) ->
          mget(Coord, Matrix)
      end,
      Path)).

read_matrix_file(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Rows = re:split(Binary, "\n", [{return, list}, trim]),
  lists:map(
    fun(Line) ->
        Row = re:split(Line, ",", [{return, list}, trim]),
        [list_to_integer(E) || E <- Row]
    end,
    Rows).

neighbors(Element) ->
  ?MODULE ! {neighbors, Element, self()},
  receive
    Result ->
      Result
  end.

previous(Coordinate) ->
  ?MODULE ! {previous, Coordinate, self()},
  receive
    Result ->
      Result
  end.

set_distance(Coordinate, Distance) ->
  ?MODULE ! {set_distance, Coordinate, Distance, self()},
  receive
    Result ->
      Result
  end.

set_previous(Neighbor, Current) ->
  ?MODULE ! {set_previous, Neighbor, Current, self()},
  receive
    Result ->
      Result
  end.

set_visited(Coordinate) ->
  ?MODULE ! {set_visited, Coordinate, self()},
  receive
    Result ->
      Result
  end.

shortest_path(none, Acc) ->
  Acc;
shortest_path({value, Coordinate}, Acc) ->
  shortest_path(previous(Coordinate), [Coordinate | Acc]).

start(Dimension) ->
  register(?MODULE, spawn(?MODULE, init, [Dimension])).

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

mdim_test_() ->
  [
   ?_assertEqual({0, 0}, mdim([])),
   ?_assertEqual({1, 1}, mdim([[1]])),
   ?_assertEqual({1, 2}, mdim([[1, 2]])),
   ?_assertEqual({2, 2}, mdim([[1, 2], [3, 4]])),
   ?_assertEqual({3, 2}, mdim([[1, 2], [3, 4], [5, 6]]))
  ].

mget_test_() ->
  Matrix = [[1,2,3], [4,5,6], [7,8,9]],
  [
   ?_assertEqual(
      [1,2,3,4,5,6,7,8,9],
      [mget({Row, Col}, Matrix) || 
       Row <- lists:seq(1,3), Col <- lists:seq(1,3)])
  ].

min_path_test_() ->
  Matrix =
  [
   [131, 673, 234, 103,  18],
   [201,  96, 342, 965, 150],
   [630, 803, 746, 422, 111],
   [537, 699, 497, 121, 956],
   [805, 732, 524,  37, 331]
  ],
  MinPath = min_path(Matrix),
  Sum = path_sum(MinPath, Matrix),
  [
   ?_assertEqual(
      [{1,1}, {2,1}, 
       {2,2}, {2,3}, 
       {1,3}, {1,4}, 
       {1,5}, {2,5}, 
       {3,5}, {3,4}, 
       {4,4}, {5,4}, 
       {5,5}], 
      MinPath),
   ?_assertEqual(2297, Sum)
  ].

-endif.
