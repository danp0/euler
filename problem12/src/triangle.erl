-module(triangle).

-export(
   [
    count_of_factors/1,
    factors/1,
    find/1,
    main/0,
    number/1,
    split_factors/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the first triangle number to have over five hundred divisors.
%

count_of_factors(N) ->
  Powers =
  lists:map(fun(E) -> length(E) + 1 end, split_factors(N)),
  lists:foldl(fun(E, Product) -> E * Product end, 1, Powers).

factors(N) ->
  factors(N, 2, []).

factors(1, _I, Factors) ->
  lists:reverse(Factors);
factors(N, I, Factors) ->
  if 
    N rem I =:= 0 ->
      factors(N div I, I, [I | Factors]);
    true ->
      factors(N, I+1, Factors)
  end.

find(Nth) ->
  Triangle = number(Nth),
  Count = count_of_factors(Triangle),
  case Count > 500 of
    true ->
      Triangle;
    false ->
      find(Nth + 1)
  end.

main() ->
  {Value, Microseconds} = timer:tc(?MODULE, find, [2]),
  io:format("~p in ~p microseconds~n", [Microseconds, Value]).

number(Nth) when Nth >= 1 ->
  Nth * (Nth + 1) div 2.

split_factors(N) ->
  Factors = factors(N),
  split_factors(Factors, []).

split_factors([], Acc) ->
  lists:reverse(Acc);
split_factors([H | _T] = Factors, Acc) ->
  {L1, L2} = lists:splitwith(fun(E) -> E =:= H end, Factors),
  split_factors(L2, [L1 | Acc]).

%%
%% tests
%%
count_of_factors_test_() ->
  [
   ?_assertEqual(2, count_of_factors(3)),
   ?_assertEqual(4, count_of_factors(6)),
   ?_assertEqual(4, count_of_factors(10)),
   ?_assertEqual(4, count_of_factors(15)),
   ?_assertEqual(4, count_of_factors(21)),
   ?_assertEqual(6, count_of_factors(28))
  ].

factors_test_() ->
  [
   ?_assertEqual([2], factors(2)),
   ?_assertEqual([2,2,5,5], factors(100))
  ].

number_test_() ->
  [
   ?_assertEqual(1, number(1)),
   ?_assertEqual(1+2, number(2)),
   ?_assertEqual(1+2+3, number(3)),
   ?_assertEqual(lists:sum(lists:seq(1,100)), number(100))
  ].

split_factors_test_() ->
  [
   ?_assertEqual([[2]], split_factors(2)),
   ?_assertEqual([[2,2], [5,5]], split_factors(100))
  ].
