-module(multiples).

-export(
   [
    find/0,
    is_permuted/1,
    main/0
   ]
  ).

find() ->
  find(1).

find(N) ->
  L = lists:map(fun(I) -> I*N end, lists:seq(2,6)),
  case is_permuted(L) of
    true ->
      {N, L};
    false ->
      find(N+1)
  end.

is_permuted(L) when length(L) =< 1 ->
  false;
is_permuted([H|T]) ->
  Digits = lists:sort(integer_to_list(H)),
  lists:all(fun(E) -> Digits =:= lists:sort(integer_to_list(E)) end, T).

main() ->
  io:format("~p~n", [find()]),
  ok.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_permuted_test_() ->
  [
   ?_assertNot(is_permuted([])),
   ?_assertNot(is_permuted([123])),
   ?_assert(is_permuted([123, 231, 321])),
   ?_assert(is_permuted([123, 321, 231, 213])),
   ?_assertNot(is_permuted([123, 321, 456]))
  ].

-endif.
