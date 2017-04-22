-module(primes).

-export(
   [
    main/0,
    sieve/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the sum of all primes below two million.
%

main() ->
  io:format("primes...~n", []),
  {Time, Sum} = timer:tc(fun() -> lists:sum(sieve(2000000)) end),
  io:format("~p microseconds~n", [Time]),
  io:format("~p~n", [Sum]).

sieve(N) ->
  sieve(N, math:sqrt(N), lists:seq(2,N), []).

sieve(_N, _SqrtN, [], Primes) ->
  lists:reverse(Primes);
sieve(N, SqrtN, [H | T], Primes) when H > SqrtN ->
  sieve(N, SqrtN, T, [H | Primes]);
sieve(N, SqrtN, [H | T], Primes) ->
  Filtered =
  lists:filter(
    fun(E) ->
        E rem H =/= 0
    end,
    T),
  sieve(N, SqrtN, Filtered, [H | Primes]).

%%
%% tests
%%
sieve_test_() ->
  [
   ?_assertEqual([2], sieve(2)),
   ?_assertEqual([2,3], sieve(3)),
   ?_assertEqual([2,3], sieve(4)),
   ?_assertEqual([2,3,5], sieve(5)),
   ?_assertEqual([2,3,5], sieve(6)),
   ?_assertEqual([2,3,5,7], sieve(7)),
   ?_assertEqual([2,3,5,7,11,13,17], sieve(18))
  ].
