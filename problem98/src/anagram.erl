-module(anagram).

-export(
   [
    anagramic_squares/2,
    main/0,
    read/1,
    sort_square/2
   ]
  ).

-define(WORDS, "p098_words.txt").

anagramic_squares(Anagrams, Squares) ->
  Mapping =
  lists:foldl(
    fun({A, S}, Tree) ->
        Key = sort_square(A, S),
        case Key =:= undefined of
          true ->
            Tree;
          false ->
            case gb_trees:lookup(Key, Tree) of
              none ->
                gb_trees:insert(Key, [{A,S}], Tree);
              {value, Value} ->
                gb_trees:update(Key, [{A,S}|Value], Tree)
            end
        end
    end,
    gb_trees:empty(),
    [{A, S} || A <- Anagrams, S <- Squares]),
  lists:usort(
    lists:append(
      lists:filtermap(
        fun({_K, Values}) ->
            case length(Values) > 1 of
              true ->
                {true, lists:map(fun({_A, S}) -> S end, Values)};
              false ->
                false
            end
        end,
        gb_trees:to_list(Mapping)))).

main() ->
  Words = read(?WORDS),
  Anagrams = map_anagrams(Words),
  Lengths = word_lengths(Anagrams),
  MinLength = lists:min(Lengths),
  MaxLength = lists:max(Lengths),
  Squares = map_squares(MinLength, MaxLength),
  AnagramicSquares =
  [anagramic_squares(Va, Vs) || {A, Va} <- gb_trees:to_list(Anagrams), {S, Vs} <- gb_trees:to_list(Squares), length(A) =:= length(S)], 
  Max = 
  lists:usort(
    lists:map(
      fun(S) -> 
          list_to_integer(S) 
      end, 
      lists:append(AnagramicSquares))),
  io:format("max: ~p~n", [Max]),
  ok.

map_anagrams(Words) ->
  Anagrams =
  gb_trees:from_orddict(
    lists:filter(
      fun({_Key, Values}) ->
          length(Values) > 1
      end,
      gb_trees:to_list(
        lists:foldl(
          fun(Word, Tree) ->
              Key = lists:sort(Word),
              case gb_trees:lookup(Key, Tree) of
                none ->
                  gb_trees:insert(Key, [Word], Tree);
                {value, Value} ->
                  gb_trees:update(Key, [Word | Value], Tree)
              end
          end,
          gb_trees:empty(),
          Words)))),
  Anagrams.

map_squares(MinLength, MaxLength) ->
  gb_trees:from_orddict(
    lists:filter(
      fun({_Key, Values}) ->
          length(Values) > 1 
      end,
      gb_trees:to_list(
        lists:foldl(
          fun({Sorted, Digits}, Tree) ->
              case gb_trees:lookup(Sorted, Tree) of
                none ->
                  gb_trees:insert(Sorted, [Digits], Tree);
                {value, Value} ->
                  gb_trees:update(Sorted, [Digits | Value], Tree)
              end
          end,
          gb_trees:empty(),
          lists:sort(
            lists:map(
              fun(Digits) ->
                  {lists:sort(Digits), Digits}
              end,
              map_squares(2, MinLength, MaxLength, []))))))).

map_squares(I, MinLength, MaxLength, Acc) ->
  ISqr = integer_to_list(I * I),
  Length = length(ISqr),
  if
    Length < MinLength ->
      map_squares(I+1, MinLength, MaxLength, Acc);
    Length =< MaxLength ->
      map_squares(I+1, MinLength, MaxLength, [ISqr | Acc]);
    true ->
      Acc
  end.

read(Filename) ->
  {ok, File} = file:read_file(Filename),
  re:split(
    re:replace(File, "\"", "", [global]), 
    ",", 
    [{return, list}]).

sort_square(Word, Square) ->
  Zipped = lists:sort(lists:zip(Word, Square)),
  case length(lists:usort(Zipped)) =:= length(lists:usort(Word)) andalso length(lists:usort(Zipped)) =:= length(lists:usort(Square)) of
    true ->
      {_, Sorted} = lists:unzip(Zipped),
      Sorted;
    false ->
      undefined
  end.

word_lengths(Anagrams) ->
  lists:map(
    fun(Key) ->
        length(Key)
    end,
    gb_trees:keys(Anagrams)).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

anagramic_squares_test_() ->
  [
   ?_assertEqual([], anagramic_squares(["ACRE", "CARE", "RACE"], ["1936", "1369"])),
   ?_assertEqual(["1296", "2916", "9216"], anagramic_squares(["ACRE", "CARE", "RACE"], ["1296", "2916", "9216"]))
  ].

map_anagrams_test_() ->
  [
   ?_assertEqual([{"ACER", ["RACE", "CARE", "ACRE"]}, {"ART", ["TAR", "RAT"]}],
                 gb_trees:to_list(map_anagrams(["ACRE", "CARE", "RACE", "RAT", "TAR", "CAT"])))
  ].

map_squares_test_() ->
  [
   ?_assertEqual([{"144", ["441", "144"]}, {"169", ["961", "196", "169"]}, {"256", ["625", "256"]}],
                 gb_trees:to_list(map_squares(3, 3)))
  ].

sort_square_test_() ->
  [
   ?_assertEqual(undefined, sort_square("AA", "81")),
   ?_assertEqual(undefined, sort_square("AAA", "121")),
   ?_assertEqual("2169", sort_square("CARE", "1296")),
   ?_assertEqual("2961", sort_square("CARE", "9216"))
  ].

word_lengths_test_() ->
  Anagrams = map_anagrams(["ACRE", "CARE", "RACE", "RAT", "TAR", "CAT"]),
  [
   ?_assertEqual([4,3], word_lengths(Anagrams))
  ].

-endif.
