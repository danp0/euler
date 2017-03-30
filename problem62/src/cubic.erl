-module(cubic).

-export(
   [
    append_cube/2,
    cube/1,
    find/1,
    main/0
   ]
  ).

append_cube(N, Tree) ->
  Key = lists:sort(integer_to_list(cube(N))),
  case gb_trees:lookup(Key, Tree) of
    none ->
      {[N], gb_trees:insert(Key, [N], Tree)};
    {value, Cubes} ->
      {[N|Cubes], gb_trees:update(Key, [N|Cubes], Tree)}
  end.

cube(N) ->
  N * N * N.

find(Permutations) ->
  find(Permutations, 1, gb_trees:empty()).

find(Permutations, N, Tree) ->
  {Cubes, Tree2} = append_cube(N, Tree),
  case length(Cubes) =:= Permutations of
    true ->
      lists:reverse(Cubes);
    false ->
      find(Permutations, N+1, Tree2)
  end.

main() ->
  Cubes = find(5),
  io:format("~p~n", [Cubes]),
  io:format("~p~n", [cube(hd(Cubes))]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cube_test_() ->
  [
   ?_assertEqual(1, cube(1)),
   ?_assertEqual(8, cube(2)),
   ?_assertEqual(27, cube(3))
  ].

find_test_() ->
  [
   ?_assertEqual([5, 8], find(2)),
   ?_assertEqual([345, 384, 405], find(3))
  ].

-endif.
