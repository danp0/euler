-module(base).

-export(
   [
    digits/2,
    find_double_base_palindromes/1,
    is_double_base_palindrome/1,
    is_palindrome/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

digits(0, _Base) ->
  [0];
digits(N, Base) ->
  digits(N, Base, []).

digits(0, _Base, Digits) ->
  Digits;
digits(N, Base, Digits) ->
  digits(N div Base, Base, [N rem Base | Digits]).

find_double_base_palindromes(N) ->
  [M || M <- lists:seq(1,N), is_double_base_palindrome(M)].

is_double_base_palindrome(N) ->
  is_palindrome(digits(N, 2)) andalso is_palindrome(digits(N, 10)).

is_palindrome(Digits) ->
  Digits =:= lists:reverse(Digits).

main() ->
  Palindromes = find_double_base_palindromes(999999), 
  io:format("~w~n~p~n", [Palindromes, lists:sum(Palindromes)]),
  ok.

%%
%% unit tests
%%
digits_test_() ->
  [
   ?_assertEqual([0], digits(0, 2)),
   ?_assertEqual([0], digits(0, 10)),
   ?_assertEqual([1], digits(1, 2)),
   ?_assertEqual([1], digits(1, 10)),
   ?_assertEqual([1,0,1,0], digits(10, 2)),
   ?_assertEqual([1,0], digits(10, 10)),
   ?_assertEqual([1,1,0,0,1,0,0], digits(100, 2)),
   ?_assertEqual([1,0, 0], digits(100, 10))
  ].

is_double_base_palindrome_test_() ->
  [
   ?_assert(is_double_base_palindrome(585))
  ].

is_palindrome_test_() ->
  [
   ?_assert(is_palindrome([0])),
   ?_assert(is_palindrome([0,0])),
   ?_assert(is_palindrome([0,1,0])),
   ?_assert(is_palindrome([0,1,2,1,0])),
   ?_assert(is_palindrome([0,1,2,3,2,1,0])),
   ?_assertNot(is_palindrome([0,1,3,2,1,0])),
   ?_assertNot(is_palindrome([0,1,2,3,2])),
   ?_assertNot(is_palindrome([0,1,2,0])),
   ?_assertNot(is_palindrome([0,1,2])),
   ?_assertNot(is_palindrome([0,1]))
  ].

