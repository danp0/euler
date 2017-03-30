-module(cycle).

-export(
   [
    between/3,
    main/0,
    pairs/0,
    pairs/1,
    sequences/1,
    triangle/1,
    square/1,
    pentagonal/1,
    hexagonal/1,
    heptagonal/1,
    octagonal/1
   ]
  ).

between(F, M, N) ->
  between(F, M, N, 1, []).

between(F, M, N, I, Acc) ->
  Fi = F(I),
  if
    Fi >= M andalso Fi =< N ->
      between(F, M, N, I+1, [{I, Fi} | Acc]);
    Fi < M ->
      between(F, M, N, I+1, Acc);
    Fi > N ->
      lists:reverse(Acc)
  end.

main() ->
  Sequences = sequences(pairs()),
  Sum =
  lists:sum(
    lists:map(
      fun({_, {P,S}}) -> 
          list_to_integer(lists:flatten([P,S]))
      end,
      Sequences)),
  io:format("~p~n", [Sequences]),
  io:format("~p~n", [Sum]),
  ok.

pairs() ->
  lists:map(
    fun(F) ->
        pairs(F)
    end,
    [fun triangle/1, fun square/1, fun pentagonal/1,
     fun hexagonal/1, fun heptagonal/1, fun octagonal/1]).

pairs(F) ->
  lists:map(
    fun({N,FN}) ->
        {N, lists:split(2, integer_to_list(FN))}
    end,
    between(F, 1000, 9999)).

sequences(Pairs) ->
  Nth = 
  fun(L) -> 
      lists:map(fun(N) -> lists:nth(N, Pairs) end, L) 
  end,
  N = lists:seq(1,6),
  Combinations =
  [
   Nth([N1, N2, N3, N4, N5, N6]) ||
   N1 <- N,
   N2 <- N -- [N1],
   N3 <- N -- [N1,N2],
   N4 <- N -- [N1,N2,N3],
   N5 <- N -- [N1,N2,N3,N4],
   N6 <- N -- [N1,N2,N3,N4,N5]],
  Sequences =
  lists:filter(
    fun(L) ->
        length(L) > 0
    end,
    [sequences2(sequences1(P, [])) || P <- Combinations]),
  lists:flatten(
    lists:usort(
      lists:map(
        fun(L) ->
            lists:sort(L)
        end,
        Sequences))).

sequences1([], Acc) ->
  Acc;
sequences1([L1, L2|Pairs], Acc) ->
  Sequences =
  [{E1, E2} ||
   {I1, {P1, S1}} = E1 <- L1,
   {I2, {P2, S2}} = E2 <- L2,
   I1 =/= I2,
   P1 =:= S2 orelse P2 =:= S1],
  Sequences2 =
    lists:map(
      fun
        ({{I1, {P1, S1}}, {I2, {P2, S2}}}) when P1 =:= S2 ->
         {{I2, {P2, S2}}, {I1, {P1, S1}}};
        (P) ->
          P
      end,
      Sequences),
  sequences1(Pairs, [Sequences2|Acc]).

sequences2([Seq1, Seq2, Seq3]) ->
  [lists:sort([E1, E2, E3, E4, E5, E6]) ||
   {{_I1, {P1,_S1}}, {_I2, {_P2,S2}}} = {E1,E2} <- Seq1,
   {{_I3, {P3,_S3}}, {_I4, {_P4,S4}}} = {E3,E4} <- Seq2,
   {{_I5, {P5,_S5}}, {_I6, {_P6,S6}}} = {E5,E6} <- Seq3,
   S2 =:= P3, S4 =:= P5, S6 =:= P1
  ].
   
triangle(N) when N >= 1 ->
  N * (N+1) div 2.

square(N) when N >= 1 ->
  N * N.

pentagonal(N) when N >= 1 ->
  N * (3*N - 1) div 2.

hexagonal(N) when N >= 1 ->
  N * (2*N - 1).

heptagonal(N) when N >= 1 ->
  N * (5*N - 3) div 2.

octagonal(N) when N >= 1 ->
  N * (3*N - 2).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

between_test_() ->
  [
   ?_assertEqual([{2,3},{3,6 },{4,10},{5,15}], between(fun triangle/1,   3, 15)),
   ?_assertEqual([{2,4},{3,9 },{4,16},{5,25}], between(fun square/1,     4, 25)),
   ?_assertEqual([{2,5},{3,12},{4,22},{5,35}], between(fun pentagonal/1, 5, 35)),
   ?_assertEqual([{2,6},{3,15},{4,28},{5,45}], between(fun hexagonal/1,  6, 45)),
   ?_assertEqual([{2,7},{3,18},{4,34},{5,55}], between(fun heptagonal/1, 7, 55)),
   ?_assertEqual([{2,8},{3,21},{4,40},{5,65}], between(fun octagonal/1,  8, 65))
  ].

triangle_test_() ->
  [
   ?_assertEqual(
      [1,3,6,10,15],
      [triangle(N) || N <- lists:seq(1,5)])
  ].

square_test_() ->
  [
   ?_assertEqual(
      [1,4,9,16,25],
      [square(N) || N <- lists:seq(1,5)])
  ].

pentagonal_test_() ->
  [
   ?_assertEqual(
      [1,5,12,22,35],
      [pentagonal(N) || N <- lists:seq(1,5)])
  ].

hexagonal_test_() ->
  [
   ?_assertEqual(
      [1,6,15,28,45],
      [hexagonal(N) || N <- lists:seq(1,5)])
  ].

heptagonal_test_() ->
  [
   ?_assertEqual(
      [1,7,18,34,55],
      [heptagonal(N) || N <- lists:seq(1,5)])
  ].

octagonal_test_() ->
  [
   ?_assertEqual(
      [1,8,21,40,65],
      [octagonal(N) || N <- lists:seq(1,5)])
  ].

-endif.
