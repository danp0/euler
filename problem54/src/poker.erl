-module(poker).

-export(
   [
    compare_rank/2,
    compare_value/2,
    index_of_rank/1,
    index_of_value/1,
    is_same_suit/1,
    is_sequential/1,
    main/0,
    matching_cards/1,
    matching_cards_length/1,
    play/1,
    play/2,
    rank/1,
    read_hands/1,
    sort_hand/1,
    string_to_deal/1,
    string_to_hand/1,
    value_of_index/1
   ]
  ).

compare(I1, I2) ->
  if
    I1 < I2 ->
      -1;
    I1 =:= I2 ->
      0;
    I1 > I2 ->
      1
  end.

compare_rank(R1, R2) ->
  compare(index_of_rank(R1), index_of_rank(R2)).

compare_value(V1, V2) ->
  compare(index_of_value(V1), index_of_value(V2)).

index_of_rank(R) ->
  Order =
  [
   high_card, one_pair, two_pair, 
   three_of_a_kind, straight, flush, 
   full_house, four_of_a_kind, 
   straight_flush, royal_flush],
  index_of(R, Order, 1).

index_of_value(V) ->
  Order = [$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K,$A],
  index_of(V, Order, 1).

index_of(V, [V | _T], Index) ->
  Index;
index_of(V, [_H | T], Index) ->
  index_of(V, T, Index+1).

is_same_suit(Hand) ->
  {_Value, Suit} = hd(Hand),
  lists:all(fun({_V,S}) -> S =:= Suit end, Hand).

is_sequential([{$A,_}, {$2,_}, {$3,_}, {$4,_}, {$5,_}]) ->
  true;
is_sequential(Hand) ->
  [V1,V2,V3,V4,V5] = [index_of_value(V) || {V,_S} <- Hand],
  [V1,V2,V3,V4,V5] =:= [V1,V1+1,V1+2,V1+3,V1+4].

main() ->
  io:format("poker...~n", []),
  Hands = read_hands("p054_poker.txt"),
  Outcomes = lists:map(fun(Deal) -> play(Deal) end, Hands),
  Ties = lists:filter(fun(Winner) -> Winner =:= 0 end, Outcomes),
  Player1 = lists:filter(fun(Winner) -> Winner =:= 1 end, Outcomes),
  Player2 = lists:filter(fun(Winner) -> Winner =:= 2 end, Outcomes),
  io:format("hands:    ~p~n", [length(Hands)]),
  io:format("ties:     ~p~n", [length(Ties)]),
  io:format("player 1: ~p~n", [length(Player1)]),
  io:format("player 2: ~p~n", [length(Player2)]),
  ok.

matching_cards(Hand) ->
  {V,_} = hd(Hand),
  matching_cards(tl(Hand), [V], []).

matching_cards([], [], Matches) ->
  lists:sort(fun(C1,C2) -> length(C1) >= length(C2) end, Matches);
matching_cards([], Match, Matches) ->
  matching_cards([], [], [Match | Matches]);
matching_cards([{V,_}|T1], [V|T2], Matches) ->
  matching_cards(T1, [V,V|T2], Matches);
matching_cards([{V,_}|T1], Match, Matches) ->
  matching_cards(T1, [V], [Match | Matches]).

matching_cards_length(Hand) ->
  lists:map(fun(V) -> length(V) end, matching_cards(Hand)).

play({H1, H2}) ->
  play(H1, H2).

play(H1, H2) ->
  R1 = rank(H1),
  R2 = rank(H2),
  case compare_rank(R1, R2) of
    -1 ->
      2;
    0 -> 
      play(H1, H2, R1);
    1 ->
      1
  end.

play(H1, H2, Rank) 
  when Rank =:= one_pair; Rank =:= two_pair; Rank =:= three_of_a_kind;
       Rank =:= full_house; Rank =:= four_of_a_kind ->
  [V1|_] = hd(matching_cards(H1)),
  [V2|_] = hd(matching_cards(H2)),
  case play_cards(V1, V2) of
    0 ->
      play_highest_card(H1, H2);
    Winner ->
      Winner
  end;
play(H1, H2, _Rank) ->
  play_highest_card(H1, H2).

play_cards(V1, V2) ->
  case compare_value(V1, V2) of
    -1 ->
      2;
    0 ->
      0;
    1 ->
      1
  end.

play_highest_card(H1, H2) ->
  play_highest_card2(lists:reverse(H1), lists:reverse(H2)).

play_highest_card2([], []) ->
  0;
play_highest_card2([{V1,_}|T1], [{V2,_}|T2]) ->
  case play_cards(V1, V2) of
    0 ->
      play_highest_card2(T1, T2);
    Winner ->
      Winner
  end.

rank(Hand) ->
  case {is_sequential(Hand), is_same_suit(Hand), matching_cards_length(Hand)} of
    {true, true, _} ->
      case hd(Hand) of
        {$T,_} ->
          royal_flush;
        _ ->
          straight_flush
      end;
    {_, _, [4,1]} ->
      four_of_a_kind;
    {_, _, [3,2]} ->
      full_house;
    {false, true, _} ->
      flush;
    {true, false, _} ->
      straight;
    {_, _, [3,1,1]} ->
      three_of_a_kind;
    {_, _, [2,2,1]} ->
      two_pair;
    {_, _, [2,1,1,1]} ->
      one_pair;
    {_, _, _} ->
      high_card
  end.

read_hands(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Hands = string:tokens(binary_to_list(Binary), "\n"),
  lists:map(
    fun string_to_deal/1,
    Hands).

sort_hand(Hand) ->
  lists:sort(
    fun
      ({V1,S1}, {V2,S2}) ->
        I1 = index_of_value(V1),
        I2 = index_of_value(V2),
        if 
          I1 =:= I2 ->
            S1 =< S2;
          true ->
            I1 =< I2
        end
    end,
    Hand).

string_to_deal(S) ->
  Tokens = string:tokens(S, "\s"),
  10 = length(Tokens),
  {L1, L2} = lists:split(5, Tokens),
  {tokens_to_hand(L1), tokens_to_hand(L2)}.

string_to_hand(S) ->
  tokens_to_hand(string:tokens(S, "\s")).

tokens_to_hand(T) ->
  Hand =
  lists:map(
    fun
      ([V, $C]) -> {V, clubs};
      ([V, $D]) -> {V, diamonds};
      ([V, $H]) -> {V, hearts};
      ([V, $S]) -> {V, spades}
    end,
    T),
  sort_hand(Hand).

value_of_index(I) ->
  lists:nth(I, [$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K,$A]).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

compare_rank_test_() ->
  [
   ?_assertEqual(0, compare_rank(high_card, high_card)),
   ?_assertEqual(-1, compare_rank(high_card, one_pair)),
   ?_assertEqual(1, compare_rank(one_pair, high_card))
  ].

compare_value_test_() ->
  [
   ?_assertEqual(0, compare_value($2, $2)),
   ?_assertEqual(-1, compare_value($2, $A)),
   ?_assertEqual(1, compare_value($A,$2))
  ].

index_of_rank_test_() ->
  [
   ?_assertEqual(
      lists:seq(1,10),
      [
       index_of_rank(R) ||
       R <- [high_card, one_pair, two_pair, 
             three_of_a_kind, straight, flush, 
             full_house, four_of_a_kind, straight_flush, 
             royal_flush]])
  ].

index_of_value_test_() ->
  [
   ?_assertEqual(
      [1,2,3,4,5,6,7,8,9,10,11,12,13],
      [index_of_value(V) || V <- [$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K,$A]])
  ].

is_same_suit_test_() ->
  [
   ?_assert(
      is_same_suit(
        [{$2,hearts}, {$2,hearts}, {$3,hearts}, {$3,hearts}, {$3,hearts}])),
   ?_assertNot(
      is_same_suit(
        [{$2,spades}, {$2,hearts}, {$3,hearts}, {$3,hearts}, {$3,hearts}]))
  ].

is_sequential_test_() ->
  [
   ?_assert(
      is_sequential(
        [{$A,hearts}, {$2,hearts}, {$3,hearts}, {$4,hearts}, {$5,hearts}])),
   ?_assert(
      is_sequential(
        [{$5,hearts}, {$6,hearts}, {$7,hearts}, {$8,hearts}, {$9,hearts}])),
   ?_assert(
      is_sequential(
        [{$8,hearts}, {$9,hearts}, {$T,hearts}, {$J,hearts}, {$Q,hearts}])),
   ?_assertNot(
      is_sequential(
        [{$9,hearts}, {$9,hearts}, {$T,hearts}, {$J,hearts}, {$Q,hearts}]))
  ].

matching_cards_length_test_() ->
  [
   ?_assertEqual(
      [1,1,1,1,1],
      matching_cards_length([{$2,hearts}, {$3,hearts}, {$4,hearts}, {$5,hearts}, {$6,hearts}])),
   ?_assertEqual(
      [2,1,1,1],
      matching_cards_length([{$2,hearts}, {$2,hearts}, {$4,hearts}, {$5,hearts}, {$6,hearts}])),
   ?_assertEqual(
      [3,2],
      matching_cards_length([{$2,hearts}, {$2,hearts}, {$3,hearts}, {$3,hearts}, {$3,hearts}]))
  ].

play_test_() ->
  [
   ?_assertEqual(2, play(string_to_deal("5H 5C 6S 7S KD 2C 3S 8S 8D TD"))),
   ?_assertEqual(1, play(string_to_deal("5D 8C 9S JS AC 2C 5C 7D 8S QH"))),
   ?_assertEqual(2, play(string_to_deal("2D 9C AS AH AC 3D 6D 7D TD QD"))),
   ?_assertEqual(1, play(string_to_deal("4D 6S 9H QH QC 3D 6D 7H QD QS"))),
   ?_assertEqual(1, play(string_to_deal("2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")))
  ].

rank_test_() ->
  [
   ?_assertEqual(
      high_card,
      rank(
        [{$2,hearts}, {$3,diamonds}, {$T,diamonds}, {$Q, hearts}, {$A,clubs}])),
   ?_assertEqual(
      one_pair,
      rank(
        [{$2,hearts}, {$2,diamonds}, {$T,diamonds}, {$Q, hearts}, {$A,clubs}])),
   ?_assertEqual(
      two_pair,
      rank(
        [{$2,hearts}, {$2,diamonds}, {$T,diamonds}, {$T, hearts}, {$A,clubs}])),
   ?_assertEqual(
      three_of_a_kind,
      rank(
        [{$2,hearts}, {$2,diamonds}, {$2,diamonds}, {$T, hearts}, {$A,clubs}])),
   ?_assertEqual(
      straight,
      rank(
        [{$2,hearts}, {$3,diamonds}, {$4,diamonds}, {$5, hearts}, {$6,clubs}])),
   ?_assertEqual(
      flush,
      rank(
        [{$2,hearts}, {$3,hearts}, {$T,hearts}, {$Q, hearts}, {$K,hearts}])),
   ?_assertEqual(
      full_house,
      rank(
        [{$2,hearts}, {$2,diamonds}, {$2,hearts}, {$Q, hearts}, {$Q,hearts}])),
   ?_assertEqual(
      four_of_a_kind,
      rank(
        [{$2,hearts}, {$2,diamonds}, {$2,hearts}, {$2, hearts}, {$Q,hearts}])),
   ?_assertEqual(
      straight_flush,
      rank(
        [{$2,hearts}, {$3,hearts}, {$4,hearts}, {$5, hearts}, {$6,hearts}])),
   ?_assertEqual(
      royal_flush, 
      rank(
        [{$T,hearts}, {$J,hearts}, {$Q,hearts}, {$K,hearts}, {$A,hearts}]))
  ].

sort_hand_test_() ->
  [
   ?_assertEqual(
      [{$3,diamonds}, {$8,spades}, {$Q,hearts}, {$A,clubs}], 
      sort_hand([{$Q,hearts}, {$A,clubs}, {$3,diamonds}, {$8,spades}])),
   ?_assertEqual(
      [{$A,clubs}, {$A,diamonds}, {$A,hearts}, {$A,spades}],
      sort_hand([{$A,spades}, {$A,hearts}, {$A,diamonds}, {$A,clubs}]))
  ].

string_to_deal_test_() ->
  [
   ?_assertEqual(
      {
       [{$2,hearts}, {$3,hearts}, {$4,spades}, {$6,spades}, {$7,spades}],
       [{$T,clubs}, {$J,clubs}, {$Q,diamonds}, {$K,diamonds}, {$A,diamonds}]
      },
      string_to_deal("7S 6S 4S 3H 2H TC JC QD KD AD"))
  ].

string_to_hand_test_() ->
  [
   ?_assertEqual(
      [{$2, hearts}, {$3, clubs}, {$3, diamonds}, {$3, hearts}, {$3, spades}],
      string_to_hand("3H 3D 3S 3C 2H")),
   ?_assertEqual(
      [{$2, hearts}, {$3, hearts}, {$4, hearts}, {$5, hearts}, {$6, hearts}],
      string_to_hand("2H 3H 4H 5H 6H"))
  ].

value_of_test_() ->
  [
   ?_assertEqual(
      [$2,$3,$4,$5,$6,$7,$8,$9,$T,$J,$Q,$K,$A],
      [value_of_index(I) || I <- lists:seq(1,13)])
  ].

-endif.

