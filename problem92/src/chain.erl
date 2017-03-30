-module(chain).

-export(
   [
    chain_until/2,
    digits/1,
    is_1_or_89/1,
    main/0,
    sum_squares/1
   ]
  ).

chain_until(Pred, N) ->
  case Pred(N) of
    true ->
      N;
    false ->
      chain_until(Pred, sum_squares(N))
  end.

digits(N) ->
  lists:map(
    fun(D) ->
        D - $0
    end,
    integer_to_list(N)).

is_1_or_89(N) ->
  N =:= 1 orelse N =:= 89.

main() ->
  Count89 =
  lists:foldl(
    fun(N, Count) ->
        case chain_until(fun is_1_or_89/1, N) of
          89 ->
            Count + 1;
          1 ->
            Count
        end
    end,
    0,
    lists:seq(1,9999999)),
  io:format("~p~n", [Count89]),
  ok.

sum_squares(N) ->
  lists:sum(lists:map(fun(D) -> D*D end, digits(N))).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

chain_until_test_() ->
  [
   ?_assertEqual(1, chain_until(fun is_1_or_89/1, 397)),
   ?_assertEqual(89, chain_until(fun is_1_or_89/1, 85))
  ].

digits_test_() ->
  [
   ?_assertEqual([1], digits(1)),
   ?_assertEqual([1, 0], digits(10)),
   ?_assertEqual([1, 2, 3], digits(123))
  ].

is_1_or_89_test_() ->
  [
   ?_assert(is_1_or_89(1)),
   ?_assert(is_1_or_89(89)),
   ?_assertNot(is_1_or_89(2))
  ].

sum_squares_test_() ->
  [
   ?_assertEqual(89, sum_squares(85)),
   ?_assertEqual(145, sum_squares(89)),
   ?_assertEqual(42, sum_squares(145))
  ].

-endif.
