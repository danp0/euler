-module(palindrome).

-export(
   [
    is_palindrome/1,
    largest/2,
    main/0,
    palindromes/2
   ]
  ).

%
% Find the largest palindrome made from the product of two 3-digit
% numbers.
%

is_palindrome(N) ->
  L = integer_to_list(N),
  L =:= lists:reverse(L).

largest(From, To) ->
  lists:last(lists:sort(palindromes(From, To))).

main() ->
  io:format("~p~n", [largest(100, 999)]).

palindromes(From, To) ->
  [X * Y || 
    X <- lists:seq(From, To), 
    Y <- lists:seq(From, To),
    is_palindrome(X * Y)].
