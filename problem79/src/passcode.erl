-module(passcode).

-export(
   [
    add/3,
    digits/1,
    main/0,
    new/1,
    read_keys/1,
    sort_by_length/1
   ]
  ).

-define(KEYFILE, "p079_keylog.txt").

add(K, F, KeySet) ->
  {K, Following} = lists:keyfind(K, 1, KeySet),
  lists:keyreplace(K, 1, KeySet, {K, lists:usort([F | Following])}).

digits(Keys) ->
  {K1, K2, K3} = lists:unzip3(Keys),
  lists:usort(lists:flatten([K1, K2, K3])).

main() ->
  Keys = read_keys(?KEYFILE),
  KeySet = new(Keys),
  SortedKeySet = sort_by_length(KeySet),
  io:format("~p~n", [SortedKeySet]),
  {P, _F} = lists:unzip(SortedKeySet),
  Passcode = lists:map(fun(K) -> K+$0 end, P),
  io:format("~p~n", [Passcode]),
  ok.

new(Keys) ->
  Digits =
  digits(Keys),
  KeySet =
  lists:zip(Digits, lists:duplicate(length(Digits), [])),
  lists:foldl(
    fun({K1, K2, K3}, KS) ->
        add(K2, K3, add(K1, K3, add(K1, K2, KS)))
    end,
    KeySet,
    Keys).

read_keys(KeyFile) ->
  {ok, Binary} = file:read_file(KeyFile),
  Keys = re:split(Binary, "\n", [{return, list}, trim]),
  lists:map(
    fun([K1, K2, K3]) -> {K1-$0, K2-$0, K3-$0} end,
    Keys).

sort_by_length(KeySet) ->
  lists:sort(
    fun({_K1, F1}, {_K2, F2}) ->
        length(F1) >= length(F2)
    end,
    KeySet).
%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

digits_test_() ->
  [
   ?_assertEqual([], digits([])),
   ?_assertEqual([1,2,3], digits([{1,2,3}])),
   ?_assertEqual([1,2,3,4,5], digits([{1,2,3}, {2,3,4}, {3,4,5}]))
  ].

new_test_() ->
  [
   ?_assertEqual([{1, [2,3]}, {2, [3]}, {3, []}], new([{1,2,3}])),
   ?_assertEqual([{1, [2,3,4]}, {2, [3,4,5]}, {3, [4,5]}, {4, [5]}, {5, []}], 
                 new([{1,2,3}, {1,2,4}, {2,3,4}, {2,3,5}, {3,4,5}]))
  ].

sort_by_length_test_() ->
  [
   ?_assertEqual(
      [{2, [2,3]}, {1, [1]}, {3, []}], 
      sort_by_length([{1, [1]}, {2, [2,3]}, {3, []}]))
  ].

-endif.
