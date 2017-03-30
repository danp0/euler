-module(lychrel).

-export(
   [
    is_lychrel/1,
    is_palindrome/1,
    main/0
   ]
  ).

is_lychrel(N) ->
  is_lychrel(N, 1, 50).

is_lychrel(_N, I, I) ->
  true;
is_lychrel(N, I, Iterations) ->
  Reversed = 
  list_to_integer(lists:reverse(integer_to_list(N))),
  Sum = N + Reversed,
  case is_palindrome(Sum) of
    true ->
      false;
    false ->
      is_lychrel(Sum, I+1, Iterations)
  end.

is_palindrome(N) ->
  Digits = integer_to_list(N),
  Digits =:= lists:reverse(Digits).

main() ->
  Lychrels = lists:filter(
               fun(N) ->
                   is_lychrel(N)
               end,
               lists:seq(1,9999)),
  io:format("lychrels: ~p~n", [Lychrels]),
  io:format("count: ~p~n", [length(Lychrels)]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_lychrel_test_() ->
  [
   ?_assertNot(is_lychrel(47)),
   ?_assert(is_lychrel(196)),
   ?_assertNot(is_lychrel(349)),
   ?_assert(is_lychrel(4994)),
   ?_assert(is_lychrel(10677))
  ].

is_palindrome_test_() ->
  [
   ?_assert(is_palindrome(1)),
   ?_assert(is_palindrome(11)),
   ?_assert(is_palindrome(121)),
   ?_assertNot(is_palindrome(12)),
   ?_assertNot(is_palindrome(122))
  ].

-endif.
