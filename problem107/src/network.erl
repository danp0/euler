-module(network).

-export(
   [
    from_matrix/1,
    main/0,
    max_savings/1,
    next_edge/2,
    prims/1,
    read_network/1,
    sum_edges/1
   ]
  ).

-define(NETWORK, "p107_network.txt").

from_matrix(Matrix) ->
  Vertices = lists:seq(1, length(Matrix)),
  Edges =
  lists:sort(
    lists:filtermap(
      fun
        ({Weight, {X, Y}}) when is_integer(Weight)->
          case X =< Y of
            true ->
              {true, {Weight, {X, Y}}};
            false ->
              false
          end;
        ({_, _}) ->
          false
      end,
      lists:flatten(
        lists:map(
          fun({Y, Row}) ->
              lists:zip(Row, [{X,Y} || X <- lists:seq(1, length(Row))])
          end,
          lists:zip(Vertices, Matrix))))),
  {Vertices, Edges}.

main() ->
  Matrix = 
  read_network(?NETWORK),
  io:format("~p~n", [max_savings(Matrix)]),
  ok.

max_savings(Matrix) ->
  {V1, E1} = from_matrix(Matrix),
  {_V2, E2} = prims({V1, E1}),
  sum_edges(E1) - sum_edges(E2).

next_edge(Edges, Vertices) ->
  Next =
  lists:dropwhile(
    fun({_Weight, {X, Y}}) ->
        Matches =
        lists:filter(
          fun(E) ->
              X =:= E orelse Y =:= E
          end,
          Vertices),
        length(Matches) =/= 1
    end,
    Edges),
  case Next of
    [] ->
      false;
    [H|_] ->
      H
  end.

prims({Vertices, Edges}) ->
  prims({Vertices, Edges}, {[1], []}).

prims({V, _E}, {Vnew, Enew}) when V =:= Vnew ->
  {Vnew, lists:sort(Enew)};
prims({V, E}, {Vnew, Enew}) ->
  {W, {X,Y}} = next_edge(E, Vnew),
  prims({V,E}, {lists:usort([X,Y|Vnew]), [{W, {X,Y}}|Enew]}).

read_network(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Lines =
  re:split(Binary, "\n", [{return, list}, trim]),
  lists:map(
    fun(Line) ->
        lists:map(
          fun
          ("-") ->
              nil;
          (Weight) ->
              list_to_integer(Weight)
          end,
          re:split(Line, ",", [{return, list}]))
    end,
    Lines).

sum_edges(Edges) ->
  lists:sum([W || {W, _} <- Edges]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

from_matrix_test_() ->
  Matrix =
  [
   [nil,  16,  12,  21, nil, nil, nil],
   [16,  nil, nil,  17,  20, nil, nil],
   [12,  nil, nil,  28, nil,  31, nil],
   [21,   17,  28, nil,  18,  19,  23],
   [nil,  20, nil,  18, nil, nil,  11],
   [nil, nil,  31,  19, nil, nil,  27],
   [nil, nil, nil,  23,  11,  27, nil]
  ],
  [
   ?_assertEqual(
      {lists:seq(1, 7),
       [{11,{5,7}},
        {12,{1,3}},
        {16,{1,2}},
        {17,{2,4}},
        {18,{4,5}},
        {19,{4,6}},
        {20,{2,5}},
        {21,{1,4}},
        {23,{4,7}},
        {27,{6,7}},
        {28,{3,4}},
        {31,{3,6}}]},
      from_matrix(Matrix))
  ].

max_savings_test_() ->
  Matrix =
  [
   [nil,  16,  12,  21, nil, nil, nil],
   [16,  nil, nil,  17,  20, nil, nil],
   [12,  nil, nil,  28, nil,  31, nil],
   [21,   17,  28, nil,  18,  19,  23],
   [nil,  20, nil,  18, nil, nil,  11],
   [nil, nil,  31,  19, nil, nil,  27],
   [nil, nil, nil,  23,  11,  27, nil]
  ],
  [
   ?_assertEqual(150, max_savings(Matrix))
  ].

next_edge_test_() ->
  Edges =
  [{11,{5,7}},
   {12,{1,3}},
   {16,{1,2}},
   {17,{2,4}},
   {18,{4,5}},
   {19,{4,6}},
   {20,{2,5}},
   {21,{1,4}},
   {23,{4,7}},
   {27,{6,7}},
   {28,{3,4}},
   {31,{3,6}}],
  [
   ?_assertEqual({12,{1,3}}, next_edge(Edges, [1])),
   ?_assertEqual({16,{1,2}}, next_edge(Edges, [3,1])),
   ?_assertEqual({17,{2,4}}, next_edge(Edges, [2,3,1]))
  ].

prims_test_() ->
  Matrix =
  [
   [nil,  16,  12,  21, nil, nil, nil],
   [16,  nil, nil,  17,  20, nil, nil],
   [12,  nil, nil,  28, nil,  31, nil],
   [21,   17,  28, nil,  18,  19,  23],
   [nil,  20, nil,  18, nil, nil,  11],
   [nil, nil,  31,  19, nil, nil,  27],
   [nil, nil, nil,  23,  11,  27, nil]
  ],
  Network = from_matrix(Matrix),
  [
   ?_assertEqual(
      {lists:seq(1,7), 
       [{11,{5,7}},{12,{1,3}},{16,{1,2}},{17,{2,4}},{18,{4,5}},{19,{4,6}}]}, 
      prims(Network))
  ].

sum_edges_test_() ->
  Matrix =
  [
   [nil,  16,  12,  21, nil, nil, nil],
   [16,  nil, nil,  17,  20, nil, nil],
   [12,  nil, nil,  28, nil,  31, nil],
   [21,   17,  28, nil,  18,  19,  23],
   [nil,  20, nil,  18, nil, nil,  11],
   [nil, nil,  31,  19, nil, nil,  27],
   [nil, nil, nil,  23,  11,  27, nil]
  ],
  {_, Edges} = from_matrix(Matrix),
  [
   ?_assertEqual(243, sum_edges(Edges))
  ].

-endif.
