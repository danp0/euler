-module(dart).

-export(
   [
    checkout/1,
    doubles/0,
    le/2,
    main/0,
    scores/0,
    valueof/1
   ]
  ).

%%
%% S: s1, s2, ... s20, s25
%% D: d1, d2, ... d20, d25
%% T: t1, t2, ... t20
%%

checkout(Fun) ->
  Scores = scores(),
  Doubles = doubles(),
  [S || S <- Doubles, Fun(S)] ++
  [[S1, S2] || S1 <- Scores, S2 <- Doubles, Fun([S1, S2])] ++
  [[S1, S2, S3] || S1 <- Scores, S2 <- Scores, S3 <- Doubles, le(S1, S2), Fun([S1, S2, S3])].

doubles() ->
  [{2, Region} || Region <- lists:seq(1,20) ++ [25]].

le({F1, R1}, {F2, R2}) ->
  100*F1 + R1 =< 100*F2 + R2.

main() ->
  Checkout =
  checkout(
    fun(Score) ->
       valueof(Score) < 100 
    end),
  io:format("~p~n", [length(Checkout)]),
  ok.

scores() ->
  [
   {Factor, Region} ||
   Factor <- lists:seq(1, 3),
   Region <- lists:seq(1, 20) ++ [25],
   Factor =/= 3 orelse Region =/= 25
  ].

valueof({Factor, Region}) ->
  Factor * Region;
valueof(L) when is_list(L) ->
  lists:foldl(
    fun(Region, Sum) ->
        valueof(Region) + Sum
    end,
    0,
    L).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

checkout_test_() ->
  [
   ?_assertEqual(
      11,
      length(
        checkout(
          fun(Score) ->
              valueof(Score) =:= 6
          end))),
   ?_assertEqual(
      42336,
      length(
        checkout(
          fun(_Score) ->
              true
          end)))
  ].

valueof_test_() ->
  [
   ?_assertEqual(1, valueof({1, 1})),
   ?_assertEqual(2, valueof({2, 1})),
   ?_assertEqual(60, valueof([{1, 10}, {2, 10}, {3, 10}]))
  ].

-endif.

