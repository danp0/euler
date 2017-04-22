-module(prime).

-export(
   [
    estimate_nth_prime/1,
    main/0,
    sieve/1
   ]
  ).

%
% Find the 10,001st prime number.
%

estimate_nth_prime(N) ->
  N * math:log(N).

main() ->
  io:format("prime...", []),
  io:format("~pth prime ~p~n", [10001, estimate_nth_prime(10001)]),
  NthPrime = 2 * trunc(estimate_nth_prime(10001)) + 1,
  Primes = sieve(NthPrime),
  io:format("~p~n", [length(Primes)]),
  io:format("~p ~p~n", [10001, lists:nth(10001, Primes)]).

sieve(N) ->
  sieve(2, N, lists:seq(2, N)).

sieve(N, N, Sieve) ->
  Sieve;
sieve(From, To, Sieve) ->
  NewSieve = lists:filter(
               fun(N) ->
                   if
                     N =:= From ->
                       true;
                     true ->
                       N rem From =/= 0
                   end
               end,
               Sieve),
  case lists:dropwhile(fun(N) -> N =< From  end, NewSieve) of
    [] ->
      NewSieve;
    [H|_] ->
      sieve(H, To, NewSieve)
  end.
