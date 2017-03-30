-module(ora).

-export(
   [
    e/1,
    eth/2,
    factor/1,
    init/1,
    is_prime/1,
    main/0,
    rad/1,
    start/1,
    stop/0
   ]
  ).

e(N) ->
  lists:sort([{rad(I), I} || I <- lists:seq(1, N)]).

eth(K, E) ->
  element(2, lists:nth(K, E)).

factor(1) ->
  [1];
factor(N) ->
  Primes = send({primes, N}),
  factor(Primes, N, []).

factor(_Primes, 1, Factors) ->
  lists:reverse(Factors);
factor([], N, Factors) when N > 1 ->
  factor([], 1, [N | Factors]);
factor([H|_T]=Primes, N, Factors) when N rem H =:= 0 ->
  factor(Primes, N div H, [H|Factors]);
factor([_H|T], N, Factors) ->
  factor(T, N, Factors).
  
init(N) ->
  loop([I || I <- lists:seq(2, trunc(math:sqrt(N)) + 1), is_prime(I)]).

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =< 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

loop(Primes) ->
  receive
    {From, {primes, N}} ->
      Upper = trunc(math:sqrt(N)) + 1,
      From ! lists:takewhile(fun(P) -> P =< Upper end, Primes);
    {From, stop} ->
      From ! ok,
      exit(normal)
  end,
  loop(Primes).

main() ->
  start(100000),
  E = e(100000),
  io:format("~p~n", [eth(10000, E)]),
  ok = stop(),
  ok.

rad(N) ->
  lists:foldl(fun(E, Prod) -> E*Prod end, 1, lists:usort(factor(N))).

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

start(N) ->
  register(?MODULE, spawn(?MODULE, init, [N])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

e_test_() ->
  {
   setup,
   fun() -> start(10) end,
   fun(_) -> stop() end,
   fun(_) ->
       E = e(10),
       [
        ?_assertEqual(8, eth(4, E)),
        ?_assertEqual(9, eth(6, E))
       ]
   end
  }.

factor_test_() ->
  {
   setup,
   fun() -> start(15) end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([1], factor(1)),
        ?_assertEqual([2], factor(2)),
        ?_assertEqual([3], factor(3)),
        ?_assertEqual([2,2], factor(4)),
        ?_assertEqual([5], factor(5)),
        ?_assertEqual([2,3], factor(6)),
        ?_assertEqual([7], factor(7)),
        ?_assertEqual([2,2,2], factor(8)),
        ?_assertEqual([3,3], factor(9)),
        ?_assertEqual([2,5], factor(10)),
        ?_assertEqual([11], factor(11)),
        ?_assertEqual([2,2,3], factor(12)),
        ?_assertEqual([13], factor(13)),
        ?_assertEqual([2,7], factor(14)),
        ?_assertEqual([3,5], factor(15))
       ]
   end
  }.

is_prime_test_() ->
  [
   ?_assertEqual(
      [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47],
      [I || I <- lists:seq(1,50), is_prime(I)])
  ].

rad_test_() ->
  {
   setup,
   fun() -> start(10) end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(
           [1,2,3,2,5,6,7,2,3,10],
           [rad(I) || I <- lists:seq(1, 10)])
       ]
   end
  }.

-endif.
