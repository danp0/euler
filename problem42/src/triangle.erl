-module(triangle).

-export(
   [
    find/1,
    find/2,
    is_triangle_number/1,
    main/0,
    nth/1,
    read_words/0,
    score_words/1,
    upto/1,
    word_value/1
   ]
  ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DQUOTE, 34).

find(Words) ->
  lists:filter(
    fun({_W, S}) -> is_triangle_number(S) end, Words).

find(Words, TriangleSet) ->
  lists:filter(
    fun({_W, S}) -> sets:is_element(S, TriangleSet) end, Words).

is_triangle_number(N) when N > 0 ->
  TwoN = 2 * N,
  SqrtN = trunc(math:sqrt(TwoN)),
  TwoN =:= SqrtN * (SqrtN + 1).

main() ->
  Words = score_words(read_words()),
  Max = lists:foldl(
          fun({_,S}, Max) -> max(S, Max) end,
          0,
          Words),
  {T1, TriangleWords1} =
  timer:tc(fun() -> find(Words) end),
  {T2, TriangleWords2} =
  timer:tc(fun() -> find(Words, sets:from_list(upto(Max))) end),
  io:format("triangle words: ~p in ~p~n", [length(TriangleWords1), T1]),
  io:format("triangle words: ~p in ~p~n", [length(TriangleWords2), T2]),
  ok.

nth(N) when N > 0 ->
  N * (N + 1) div 2.

read_words() ->
  {ok, Binary} = file:read_file("p042_words.txt"),
  Words = binary:split(Binary, <<",">>, [global]),
  lists:map(
    fun(Word) ->
        string:strip(binary_to_list(Word), both, ?DQUOTE)
    end,
    Words).

score_words(Words) ->
  lists:map(fun(W) -> {W, word_value(W)} end, Words).

upto(Value) ->
  upto(1,Value, []).

upto(N, Value, Acc) ->
  Nth = nth(N),
  if
    Value =< Nth ->
      lists:reverse([Nth | Acc]);
    true ->
      upto(N+1, Value, [Nth | Acc])
  end.

word_value(Word) ->
  lists:sum(lists:map(fun(C) -> C - $A + 1 end, Word)).

%%
%% unit test
%%
-ifdef(TEST).

is_triangle_number_test_() ->
  NonTriangleNumbers = 
  lists:seq(1, 100) -- [nth(N) || N <- lists:seq(1,100)],
  [
   ?_assertNot(lists:any(fun(N) -> is_triangle_number(N) end, 
                         NonTriangleNumbers))
  ].

is_triangle_number_true_test_() ->
  TriangleNumbers = [nth(N) || N <- lists:seq(1,100)],
  [
   ?_assert(lists:all(fun(N) -> is_triangle_number(N) end, TriangleNumbers))
  ].

nth_test_() ->
  [
   ?_assertEqual(1, nth(1)),
   ?_assertEqual(3, nth(2)),
   ?_assertEqual(6, nth(3))
  ].

upto_test_() ->
  [
   ?_assertEqual([1], upto(1)),
   ?_assertEqual([1,3], upto(2)),
   ?_assertEqual([1,3,6], upto(4))
  ].

-endif.

