-module(prime_sum).

-export(
   [
    main/0,
    sieve/1
   ]
  ).

main() ->
  {Time, Sum} = timer:tc(fun() -> sieve(2000000) end),
  io:format("~p microseconds~n", [Time]),
  io:format("~p~n", [Sum]).

sieve(N) ->
  sieve(N, math:sqrt(N), lists:seq(2,N), 0).

sieve(_N, _SqrtN, [], Sum) ->
  Sum;
sieve(N, SqrtN, [H | T], Sum) when H > SqrtN ->
  sieve(N, SqrtN, T, H + Sum);
sieve(N, SqrtN, [H | T], Sum) ->
  Filtered =
  lists:filter(
    fun(E) ->
        E rem H =/= 0
    end,
    T),
  sieve(N, SqrtN, Filtered, H + Sum).
