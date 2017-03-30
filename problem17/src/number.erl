-module(number).

-export(
   [
    count_letters/2,
    main/0,
    number_to_word/1,
    word_length/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

count_letters(From, To) ->
  lists:sum([word_length(number_to_word(N)) || N <- lists:seq(From, To)]).

main() ->
  io:format("number...~n", []),
  io:format("letters 1 - 1000: ~p~n", [count_letters(1, 1000)]).

number_to_compound_word(N) when N > 20 andalso N < 100 ->
  Ones = N rem 10,
  Tens = N - Ones,
  number_to_word(Tens) ++ "-" ++ number_to_word(Ones);
number_to_compound_word(N) when N > 100 andalso N < 1000 ->
  Tens = N rem 100,
  Hundreds = N div 100,
  if
    Tens =:= 0 ->
      number_to_word(Hundreds) ++ " hundred";
    true ->
      number_to_word(Hundreds) ++ " hundred and " ++ number_to_word(Tens)
  end.

number_to_word(N) when N >= 1 andalso N =< 1000 ->
  Words = 
  [
   {1, "one"}, {2, "two"}, {3, "three"}, {4, "four"}, {5, "five"},
   {6, "six"}, {7, "seven"}, {8, "eight"}, {9, "nine"}, {10, "ten"},
   {11, "eleven"}, {12, "twelve"}, {13, "thirteen"}, {14, "fourteen"}, {15, "fifteen"},
   {16, "sixteen"}, {17, "seventeen"}, {18, "eighteen"}, {19, "nineteen"}, {20, "twenty"},
   {30, "thirty"}, {40, "forty"}, {50, "fifty"}, {60, "sixty"}, {70, "seventy"},
   {80, "eighty"}, {90, "ninety"}, {100, "one hundred"}, {1000, "one thousand"}
  ],
  case lists:keyfind(N, 1, Words) of
    {N, Word} ->
      Word;
    false ->
      number_to_compound_word(N)
  end.

word_length(Word) ->
  length(lists:filter(
           fun(C) ->
               [C] =/= " " andalso [C] =/= "-"
           end,
           Word)).
%%
%% unit tests
%%
count_letters_test_() ->
  [
   ?_assertEqual(19, count_letters(1, 5))
  ].

number_to_word_test_() ->
  [
   ?_assertEqual("one", number_to_word(1)),
   ?_assertEqual("ten", number_to_word(10)),
   ?_assertEqual("one hundred", number_to_word(100))
  ].

word_length_test_() ->
  [
   ?_assertEqual(3, word_length("one")),
   ?_assertEqual(9, word_length("thirty-one")),
   ?_assertEqual(10, word_length("one hundred"))
  ].
