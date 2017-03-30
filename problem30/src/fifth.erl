-module(fifth).

-export(
   [
    fifth/1,
    find/3,
    fourth/1,
    main/0,
    split/1,
    sum_powers/2
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%%
%% 0  1   2    3     4     5      6      7      8      9
%% 0, 1, 16,  81,  256,  625,  1296,  2401,  4096,  6561
%% 0, 1, 32, 243, 1024, 3125,  7776, 16807, 32768, 59049
%%

fifth(N) ->
  N * N * N * N * N.

find(F, From, To) ->
  [N || N <- lists:seq(From, To), N =:= sum_powers(F, N)].

fourth(N) ->
  N * N * N * N.

main() ->
  DigitFifthPowers = find(fun fifth/1, 10, 999999),
  io:format("fifths: ~p~n", [DigitFifthPowers]),
  io:format("sum:    ~p~n", [lists:sum(DigitFifthPowers)]),
  ok.

split(0) ->
  [0];
split(N) when N >= 1 ->
  split(N, []).

split(0, Acc) ->
  Acc;
split(N, Acc) ->
  split(N div 10, [N rem 10 | Acc]).

sum_powers(Power, Number) when is_number(Number) ->
  sum_powers(Power, split(Number));
sum_powers(Power, Digits) when is_list(Digits) ->
  lists:sum(lists:map(Power, Digits)).

%%
%% unit test
%%
fifth_test_() ->
  [
   ?_assertEqual(0, fifth(0)),
   ?_assertEqual(1, fifth(1)),
   ?_assertEqual(32, fifth(2))
  ].

find_test_() ->
  [
   ?_assertEqual([1634,8208,9474], find(fun fourth/1, 10, 99999)) 
  ].

fourth_test_() ->
  [
   ?_assertEqual(0, fourth(0)),
   ?_assertEqual(1, fourth(1)),
   ?_assertEqual(16, fourth(2))
  ].

split_test_() ->
  [
   ?_assertEqual([0], split(0)),
   ?_assertEqual([1], split(1)),
   ?_assertEqual([1,2,3], split(123))
  ].

sum_powers_test_() ->
  [
   ?_assertEqual(1634, sum_powers(fun fourth/1, 1634)),
   ?_assertEqual(1634, sum_powers(fun fourth/1, [1,6,3,4]))
  ].

