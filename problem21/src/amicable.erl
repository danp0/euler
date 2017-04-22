-module(amicable).

-export(
   [
    find_amicable_numbers/1,
    main/0,
    proper_divisors/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Let d(n) be defined as the sum of proper divisors of n (numbers
% less than n which evenly divide n). If d(a) = b and d(b) = a when
% a <> b, then a and b are amicable. Find the sum of all amicable
% numbers under 10000.
%

find_amicable_numbers(UpperBound) ->
  Sums = [{N, lists:sum(proper_divisors(N))} || N <- lists:seq(2, UpperBound)],
  lists:filter(
    fun
      ({N, S}) when N < S ->
        case lists:keyfind(S, 1, Sums) of
          {S, N} ->
            true;
          _ ->
            false
        end;
      ({_N, _S}) ->
        false
    end,
    Sums).

main() ->
  io:format("amicable...~n", []),
  AmicableNumbers = find_amicable_numbers(9999),
  SumAmicableNumbers = 
    lists:foldl(fun({A, B}, Sum) -> A+B+Sum end, 0, AmicableNumbers),
  io:format("~p~n", [SumAmicableNumbers]).

proper_divisors(N) when N > 1 ->
  proper_divisors(1, N div 2, N, []).

proper_divisors(From, To, _N, Acc) when From > To ->
  lists:reverse(Acc);
proper_divisors(From, To, N, Acc) when N rem From =/= 0 ->
  proper_divisors(From + 1, To, N, Acc);
proper_divisors(From, To, N, Acc) ->
  proper_divisors(From + 1, To, N, [From | Acc]).

%%
%% unit tests
%%
proper_divisors_test_() ->
  [
   ?_assertEqual([1], proper_divisors(2)),
   ?_assertEqual([1], proper_divisors(3)),
   ?_assertEqual([1,2], proper_divisors(4)),
   ?_assertEqual([1,2,4,5,10,11,20,22,44,55,110], proper_divisors(220)),
   ?_assertEqual([1,2,4,71,142], proper_divisors(284))
  ].
