-module(sum).

-export(
   [
    main/0,
    sum_square_difference/2
   ]
  ).

%
% Find the difference between the sum of the squares of the first
% one hundred natural numbers and the square of the sum.
%

sum_square_difference(From, To) ->
  SumOfSquares = lists:sum([X*X || X <- lists:seq(From, To)]),
  Sum = lists:sum(lists:seq(From, To)),
  SquareOfSum = Sum * Sum,
  SquareOfSum - SumOfSquares.

main() ->
  io:format("sum square difference~n", []),
  io:format("~p~n", [sum_square_difference(1, 10)]),
  io:format("~p~n", [sum_square_difference(1, 100)]).
