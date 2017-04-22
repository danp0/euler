-module(abundant).

-export(
   [
    find_abundant_number_set/1,
    find_non_abundant_sum/1,
    has_sum/2,
    main/0,
    proper_divisors/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% A perfect number is a number in which the sum of its proper
% divisors equal the number.
%
% A deficient number is a number in which the sum of its proper
% divisors is less than the number.
%
% An abundant number is a number in which the sum of its proper
% divisors is greater than the number.
%
% All integers greater than 28123 can be written as the sum of
% two abundant numbers.
%
% Find the sum of all positive integers which cannot be written
% as the sum of two abundant numbers.
%

find_abundant_number_set(N) ->
  sets:from_list([A || A <- lists:seq(1,N), A < lists:sum(proper_divisors(A))]).

find_non_abundant_sum(N) ->
  AbundantNumbers = find_abundant_number_set(N),
  NonAbundantSums = [A || A <- lists:seq(1,N), not has_sum(A, AbundantNumbers)],
  lists:sum(NonAbundantSums).

has_sum(N, Set) ->
  [A || A <- lists:seq(1, N div 2), sets:is_element(A, Set), sets:is_element(N-A, Set)] =/= [].

main() ->
  io:format("abundant...~n", []),
  {Time, Value} = timer:tc(?MODULE, find_non_abundant_sum, [28123]),
  io:format("~p in ~p microseconds~n", [Value, Time]).

proper_divisors(N) when N >= 1 ->
  [F || F <- lists:seq(1, N div 2), N rem F =:= 0].

%%
%% unit tests
%%
has_sum_test_() ->
  Set = sets:from_list([1,2,3,4,5]),
  [
   ?_assert(has_sum(2, Set)),
   ?_assert(has_sum(3, Set)),
   ?_assert(has_sum(10, Set)),
   ?_assertNot(has_sum(11, Set))
  ].

proper_divisors_test_() ->
  [
   ?_assertEqual([1], proper_divisors(2)),
   ?_assertEqual([1], proper_divisors(3)),
   ?_assertEqual([1,2], proper_divisors(4)),
   ?_assertEqual([1], proper_divisors(5)),
   ?_assertEqual([1,2,3], proper_divisors(6))
  ].
