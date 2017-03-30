-module(ps).

-export(
   [
    find/1,
    is_palindrome/1,
    main/0,
    sum_of_squares/1
   ]
  ).

%%
%%  _n_
%%  \   k^2 = n(n+1)(2n+1)/6
%%  /__
%%  k=1
%%

find(Below) ->
  lists:usort(
    lists:filter(
      fun(E) -> E =< Below end,
      find(Below, 2, [sum_of_squares(1)], []))).

find(Below, I, _SumOfSquares, PS) when I*I > Below ->
  PS;
find(Below, I, SumOfSquares, PS) ->
  S = sum_of_squares(I),
  PS2 = 
  [E1 || E1 <- [S | [S - E2 || E2 <- tl(SumOfSquares)]], is_palindrome(E1)],
  find(Below, I+1, [S | SumOfSquares], lists:append(PS2, PS)). 
  
is_palindrome(N) ->
  I = integer_to_list(N),
  I =:= lists:reverse(I).

main() ->
  F = find(100000000),
  io:format("~p~n", [F]),
  io:format("~p~n", [lists:sum(F)]),
  ok.

sum_of_squares(N) ->
  (N * (N + 1) * (2*N + 1)) div 6.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_test_() ->
  [
   ?_assertEqual(4164, lists:sum(find(1000)))
  ].

is_palindrome_test_() ->
  [
   ?_assert(is_palindrome(1)),
   ?_assert(is_palindrome(11)),
   ?_assertNot(is_palindrome(12)),
   ?_assert(is_palindrome(121)),
   ?_assertNot(is_palindrome(123)),
   ?_assert(is_palindrome(1221)),
   ?_assertNot(is_palindrome(1223)),
   ?_assert(is_palindrome(12321)),
   ?_assertNot(is_palindrome(12323))
  ].

sum_of_squares_test_() ->
  Sum = fun(N) -> lists:sum([I*I || I <- lists:seq(1,N)]) end,
  [
   ?_assertEqual(Sum(1), sum_of_squares(1)),
   ?_assertEqual(Sum(2), sum_of_squares(2)),
   ?_assertEqual(Sum(3), sum_of_squares(3)),
   ?_assertEqual(Sum(4), sum_of_squares(4)),
   ?_assertEqual(Sum(5), sum_of_squares(5)),
   ?_assertEqual(Sum(10), sum_of_squares(10))
  ].

-endif.
