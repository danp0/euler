-module(path).

-export(
   [
    main/0,
    max_path/1,
    read_triangle/1
   ]
  ).

-define(TRIANGLE, "p067_triangle.txt").

main() ->
  Triangle = read_triangle(?TRIANGLE),
  io:format("~p~n", [max_path(Triangle)]),
  ok.

max_path(Triangle) ->
  max_path2(lists:reverse(Triangle)).

max_path2([[Path]]) ->
  Path;
max_path2([R1, R2 | T]) ->
  max_path2([max_path2(R1, R2, []) | T]).

max_path2([_], [], Acc) ->
  lists:reverse(Acc);
max_path2([A1, A2 | A3], [B1 | B2], Acc) ->
  max_path2([A2 | A3], B2, [max(B1+A1, B1+A2) | Acc]).

read_triangle(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  List = binary_to_list(Binary),
  Lines = re:split(List, "\n", [{return, list}, trim]),
  Tokens =
  lists:map(
    fun(Line) ->
        re:split(Line, "\s", [{return, list}])
    end,
    Lines),
  lists:map(
    fun(Line) ->
        [list_to_integer(S) || S <- Line]
    end,
    Tokens).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

max_path_test_() ->
  [
   ?_assertEqual(23, max_path([[3], [7,4], [2,4,6], [8,5,9,3]]))
  ].

read_triangle_test_() ->
  [
   ?_assertEqual(
      lists:seq(1,100),
      lists:map(
        fun(Row) -> 
            length(Row) 
        end,
        read_triangle(filename:join("..", ?TRIANGLE))))
  ].

-endif.
