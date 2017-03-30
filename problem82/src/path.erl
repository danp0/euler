-module(path).

-export(
   [
    init/0,
    lookup_min_path/1,
    main/0,
    mdim/1,
    mget/2,
    min_path/1,
    read_matrix_file/1,
    set_min_path/2,
    start/0,
    stop/0
   ]
  ).

-define(MFILE, "p082_matrix.txt").

-record(
   state,
   {
    mp = gb_trees:empty()
   }
  ).

init() ->
  loop(#state{}).

lookup_min_path(Key) ->
  ?MODULE ! {lookup_min_path, Key, self()},
  receive
    Result ->
      Result
  end.

loop(State) ->
  NewState =
  receive
    {lookup_min_path, Key, From} ->
      From ! gb_trees:lookup(Key, State#state.mp),
      State;
    {set_min_path, Key, Value, From} ->
      MP = gb_trees:enter(Key, Value, State#state.mp),
      From ! ok,
      State#state{mp = MP};
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  Matrix =
  read_matrix_file(?MFILE),
  io:format("~p~n", [min_path(Matrix)]),
  ok.

mdim([]) ->
  {0, 0};
mdim(Matrix) ->
  {length(Matrix), length(hd(Matrix))}.

mget({Row, Col}, Matrix) ->
  lists:nth(Col, lists:nth(Row, Matrix)).

min_path(Matrix) ->
  start(),
  {Rows, Cols} = mdim(Matrix),
  Paths =
  [
   min_path(Matrix, {Rows, Cols}, right, {Row, 1}) || 
   Row <- lists:seq(1,Rows)
  ],
  MinPath =
  lists:min(Paths),
  ok = stop(),
  MinPath.

min_path(Matrix, {Rows, Cols}, Last, {Row, Col}) ->
  case lookup_min_path({Row, Col}) of
    none ->
      MinPath =
      mget({Row, Col}, Matrix) +
      case {Row, Col} of
        {Row, Cols} ->
          0;
        {1, Col} ->
          Right = min_path(Matrix, {Rows, Cols}, right, {Row, Col+1}),
          if 
            Last =:= up ->
              Right;
            true ->
              min(
                Right,
                min_path(Matrix, {Rows, Cols}, down, {Row+1, Col}))
          end;
        {Rows, Col} ->
          Right = min_path(Matrix, {Rows, Cols}, right, {Row, Col+1}),
          if 
            Last =:= down ->
              Right;
            true ->
              min(
                Right,
                min_path(Matrix, {Rows, Cols}, up, {Row-1, Col}))
          end;
        {Row, Col} ->
          Right = min_path(Matrix, {Rows, Cols}, right, {Row, Col+1}),
          if
            Last =:= up ->
              min(
                Right,
                min_path(Matrix, {Rows, Cols}, up, {Row-1, Col}));
            Last =:= down ->
              min(
                Right,
                min_path(Matrix, {Rows, Cols}, down, {Row+1, Col}));
            Last =:= right ->
              lists:min(
                [
                 Right,
                 min_path(Matrix, {Rows, Cols}, up, {Row-1, Col}),
                 min_path(Matrix, {Rows, Cols}, down, {Row+1, Col})
                ]
               )
          end
      end,
      if
        Last =:= right ->
          set_min_path({Row, Col}, MinPath);
        true ->
          skip
      end,
      MinPath;
    {value, Value} ->
      Value
  end.

read_matrix_file(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Rows = re:split(Binary, "\n", [{return, list}, trim]),
  lists:map(
    fun(Line) ->
        Row = re:split(Line, ",", [{return, list}, trim]),
        [list_to_integer(E) || E <- Row]
    end,
    Rows).

set_min_path(Key, Value) ->
  ?MODULE ! {set_min_path, Key, Value, self()},
  receive
    Result ->
      Result
  end.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

mdim_test_() ->
  [
   ?_assertEqual({0,0}, mdim([])),
   ?_assertEqual({1,1}, mdim([[1]])),
   ?_assertEqual({2,2}, mdim([[1,2],[1,2]])),
   ?_assertEqual({2,3}, mdim([[1,2,3], [1,2,3]])),
   ?_assertEqual({3,2}, mdim([[1,2], [1,2], [1,2]]))
  ].

mget_test_() ->
  Matrix =
  [
   [1, 2, 3],
   [4, 5, 6],
   [7, 8, 9]
  ],
  [
   ?_assertEqual([1,2,3,4,5,6,7,8,9],
                 [mget({X,Y},Matrix) || 
                  X <- lists:seq(1,3), 
                  Y <- lists:seq(1,3)])
  ].

min_path_test_() ->
  Matrix =
  [
   [131, 673, 234, 103, 18 ],
   [201, 96 , 342, 965, 150],
   [630, 803, 746, 422, 111],
   [537, 699, 497, 121, 956],
   [805, 732, 524, 37,  331]
  ],
  [
   ?_assertEqual(994, min_path(Matrix))
  ].

-endif.
