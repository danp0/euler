-module(sum).

-export([
   sumof/3,
   main/0
  ]).

%
% Find sum of all multiples of 3 or 5 below 1000.
%

sumof(Ok, From, To) ->
  lists:sum([N || N <- lists:seq(From, To), Ok(N)]).

main() ->
  Sum =
  sumof(fun(N) -> N rem 3 =:= 0 orelse N rem 5 =:= 0 end, 1, 999),
  io:format("sumof: ~p~n", [Sum]).
