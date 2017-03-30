-module(ngon).

-export(
   [
    concat/1,
    five_gon/0,
    is_magic/2,
    main/0,
    quintuple_pairs/1,
    quintuples/1,
    three_gon/0,
    triple_pairs/1,
    triples/1
   ]
  ).

concat(Ngon) ->
  lists:map(
    fun(Seq) ->
        lists:flatten(
        lists:map(
          fun({Outer, [N1, N2]}) ->
              [Outer, N1, N2]
          end,
          Seq))
    end,
    Ngon).

five_gon() ->
  Seq = lists:seq(1,10),
  [lists:zip(Outer, Inner) ||
   Outer <- quintuples(Seq),
   Inner <- quintuple_pairs(Seq -- Outer),
   is_magic(Outer, Inner)].

is_magic(OuterRing, InnerPairs) ->
  Zip = lists:zip(OuterRing, InnerPairs),
  {A, [B,C]} = hd(Zip),
  Sum = A + B + C,
  lists:all(
    fun({X, [Y,Z]}) ->
        Sum =:= X + Y + Z
    end,
    tl(Zip)).

main() ->
  io:format("~p~n", [concat(five_gon())]),
  ok.

quintuple_pairs(Seq) ->
  [[[N1,N2], [N2,N3], [N3,N4], [N4,N5], [N5,N1]] ||
   N1 <- Seq,
   N2 <- Seq -- [N1],
   N3 <- Seq -- [N1, N2],
   N4 <- Seq -- [N1, N2, N3],
   N5 <- Seq -- [N1, N2, N3, N4]].

quintuples(Seq) when length(Seq) < 5 ->
  [];
quintuples(Seq) ->
  [[N1,N2,N3,N4,N5] ||
   N1 <- Seq,
   N2 <- Seq -- [N1],
   N3 <- Seq -- [N1,N2],
   N4 <- Seq -- [N1,N2,N3],
   N5 <- Seq -- [N1,N2,N3,N4],
   N1 < N2, N1 < N3, N1 < N4, N1 < N5].

three_gon() ->
  Seq = lists:seq(1,6),
  [lists:zip(Outer, Inner) ||
   Outer <- triples(Seq),
   Inner <- triple_pairs(Seq -- Outer),
   is_magic(Outer, Inner)].

triple_pairs(Seq) ->
  [[[N1,N2], [N2,N3], [N3,N1]] ||
   N1 <- Seq,
   N2 <- Seq -- [N1],
   N3 <- Seq -- [N1, N2]].

triples(Seq) when length(Seq) < 3 ->
  [];
triples(Seq) ->
  [[N1, N2, N3] ||
   N1 <- Seq,
   N2 <- Seq -- [N1],
   N3 <- Seq -- [N1, N2],
   N1 < N2, N1 < N3].

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_magic_test_() ->
  [
   ?_assert(is_magic([4,5,6], [[2,3], [3,1], [1,2]])),
   ?_assertNot(is_magic([4,5,6], [[1,2], [2,3], [3,1]])),
   ?_assert(is_magic([1,2,3,4,5], [[8,10], [10,7], [7,9], [9,6], [6,8]]))
  ].

quintuple_pairs_test_() ->
  [
   ?_assert(
      lists:all(
        fun([[N1,N2], [N3,N4], [N5,N6], [N7,N8], [N9,N10]]) ->
            N1 =:= N10 andalso
            N2 =:= N3 andalso
            N4 =:= N5 andalso
            N6 =:= N7 andalso
            N8 =:= N9
        end,
        quintuple_pairs([1,2,3,4,5])))
  ].

quintuples_test_() ->
  [
   ?_assert(
      lists:all(
        fun([N1,N2,N3,N4,N5]) ->
            N1 < N2 andalso N1 < N3 andalso N1 < N4 andalso N1 < N5
        end,
        quintuples([1,2,3,4,5])))
  ].

triple_pairs_test_() ->
  [
   ?_assert(
      lists:all(
        fun([[N1,N2],[N3,N4],[N5,N6]]) ->
            N1 =:= N6 andalso N2 =:= N3 andalso N4 =:= N5
        end,
        triple_pairs([1,2,3])))
  ].

triples_test_() ->
  [
   ?_assertEqual([[1,2,3], [1,3,2]], triples([1,2,3]))
  ].

-endif.
