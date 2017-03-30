-module(path).

-export(
   [
    init/0,
    lookup_min_path/1,
    main/0,
    mdim/1,
    mget/3,
    min_path/1,
    read_matrix_file/1,
    set_min_path/2,
    start/0,
    stop/0
   ]
  ).

-define(MFILE, "p081_matrix.txt").

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
  io:format("min path: ~p~n", [min_path(Matrix)]),
  ok.

mdim([]) ->
  {0, 0};
mdim(Matrix) ->
  {length(Matrix), length(hd(Matrix))}.

mget(Row, Col, Matrix) ->
  lists:nth(Col, lists:nth(Row, Matrix)).

min_path(Matrix) ->
  start(),
  {R, C} = mdim(Matrix),
  MinPath = min_path(R, C, Matrix),
  ok = stop(),
  MinPath.

min_path(R, C, Matrix) ->
  case lookup_min_path({R,C}) of
    none ->
      MP =
      mget(R, C, Matrix) + 
      case {R, C} of
        {1, 1} ->
          0;
        {1, C} ->
          min_path(R, C-1, Matrix);
        {R, 1} ->
          min_path(R-1, C, Matrix);
        {R, C} ->
          min(min_path(R-1, C, Matrix), min_path(R, C-1, Matrix))
      end,
      set_min_path({R,C}, MP),
      MP;
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
                 [mget(X,Y,Matrix) || 
                  X <- lists:seq(1,3), 
                  Y <- lists:seq(1,3)])
  ].

min_path_test_() ->
  M =
  [
   [131, 673, 234, 103,  18],
   [201,  96, 342, 965, 150],
   [630, 803, 746, 422, 111],
   [537, 699, 497, 121, 956],
   [805, 732, 524,  37, 331]
  ],
  [
   ?_assertEqual(2427, min_path(M))
  ].

-endif.
