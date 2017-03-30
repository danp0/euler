-module(cube).

-export(
   [
    expand/1,
    expand_pairs/1,
    filter_pairs/1,
    main/0,
    pairs/0
   ]
  ).

%%
%%  1  2  3  4  5  6  7  8  9
%% 01 04 09 16 25 36 49 64 81
%%


expand(L) when length(L) >= 6 ->
  [L];
expand(L) ->
  lists:append([expand([E | L]) || E <- (lists:seq(0,9) -- L)]).

expand_pairs(Pairs) ->
  expand_pairs(Pairs, [], []).

expand_pairs([], [], Acc) ->
  Acc;
expand_pairs([[L1, L2] | T], [], Acc) ->
  expand_pairs(T, [[E1, E2] || E1 <- expand(L1), E2 <- expand(L2)], Acc);
expand_pairs(Pairs, [H | T], Acc) ->
  expand_pairs(Pairs, T, [H | Acc]).

filter_pairs(L) ->
  lists:filter(
    fun([L1, L2]) ->
        length(L1) =< 6 andalso length(L2) =< 6
    end,
    L).

main() ->
  Pairs =
  lists:usort(
    lists:map(
      fun([L1, L2]) ->
          lists:sort([lists:sort(L1), lists:sort(L2)])
      end,
        filter_pairs(
          expand_pairs(
            pairs())))),
  io:format("~p~n", [Pairs]),
  io:format("~p~n", [length(Pairs)]),
  ok.

pairs() ->
  lists:map(
    fun(L) ->
        {L1, L2} = lists:unzip(L),
        [lists:usort(L1), lists:usort(L2)]
    end,
    [
     [P1, P2, P3, P4, P5, P6, P7, P8, P9] ||
     P1 <- [{0,1}, {1,0}],
     P2 <- [{0,4}, {4,0}],
     P3 <- [{0,9}, {9,0}, {0,6}, {6,0}],
     P4 <- [{1,6}, {6,1}, {1,9}, {9,1}],
     P5 <- [{2,5}, {5,2}],
     P6 <- [{3,6}, {6,3}, {3,9}, {9,3}],
     P7 <- [{4,9}, {9,4}, {4,6}, {6,4}],
     P8 <- [{6,4}, {4,6}, {9,4}, {4,9}],
     P9 <- [{8,1}, {1,8}]]).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

expand_test_() ->
  [
   ?_assertEqual([[0,1,2,3,4,5]], expand([0,1,2,3,4,5])),
   ?_assertEqual(
      [
       [5,0,1,2,3,4],
       [6,0,1,2,3,4],
       [7,0,1,2,3,4],
       [8,0,1,2,3,4],
       [9,0,1,2,3,4]
      ], 
      expand([0,1,2,3,4]))
  ].

filter_pairs_test_() ->
  [
   ?_assertEqual([], filter_pairs([[[1,2,3,4,5,6,7], [1,2,3,4,5,6]]])),
   ?_assertEqual([[[1,2,3], [1,2,3,4,5,6]]], filter_pairs([[[1,2,3], [1,2,3,4,5,6]]])),
   ?_assertEqual([[[1,2,3,4,5,6], [1,2,3,4,5,6]]], filter_pairs([[[1,2,3,4,5,6], [1,2,3,4,5,6]]]))
  ].

-endif.
