-module(reciprocals).

-export(
   [
    factor/1,
    find/0,
    init/0,
    is_prime/1,
    main/0,
    primes/1,
    solutions/1,
    solutions_by_factors/1,
    solve/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    last = 2,
    primes = [2]
   }
  ).

factor(1) ->
  [{1,1}];
factor(N) ->
  Primes = primes(trunc(math:sqrt(N)) + 1),
  factor(N, Primes, 0, []).

factor(N, [], _Count, Acc) ->
  NewAcc =
  case N > 1 of
    true ->
      [{N, 1} | Acc];
    false ->
      Acc
  end,
  lists:reverse(NewAcc);
factor(N, [H | T], Count, Acc) when N rem H =:= 0 ->
  factor(N div H, [H|T], Count+1, Acc);
factor(N, [H | T], Count, Acc) ->
  NewAcc =
  case Count > 0 of
    true ->
      [{H, Count} | Acc];
    false ->
      Acc
  end,
  factor(N, T, 0, NewAcc).

find() ->
  find(1).

find(N) ->
  Solutions = solutions_by_factors(N),
  case Solutions > 1000 of
    true ->
      N;
    false ->
      find(N+1)
  end.

init() ->
  loop(#state{}).

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(N, 5, trunc(math:sqrt(N)) + 1).

is_prime(_N, I, To) when I > To ->
  true;
is_prime(N, I, To) ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(N, I+1, To)
  end.

loop(State) ->
  NewState =
  receive
    {{primes, N}, From} ->
      {Last, Primes} =
      case N =< State#state.last of
        true ->
          {State#state.last, State#state.primes};
        false ->
          {
           N,
           lists:append(
             State#state.primes,
             lists:filter(fun is_prime/1, lists:seq(State#state.last + 1, N)))}
      end,
      From ! lists:takewhile(fun(P) -> P =< N end, Primes),
      State#state{last = Last, primes = Primes};
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("~p~n", [find()]),
  ok = stop(),
  ok.

primes(N) ->
  send({primes, N}).

send(Msg) ->
  ?MODULE ! {Msg, self()},
  receive
    Result ->
      Result
  end.

solutions(N) ->
  solutions(N, N+1, false, 0).

solutions(_N, X, Y, Count) when is_integer(Y) andalso X >= Y ->
  Count;
solutions(N, X, _Y, Count) ->
  XN1 = X * N,
  XN2 = X - N,
  {Y, NewCount} =
  case XN1 rem XN2 =:= 0 of
    true ->
      {XN1 div XN2, Count+1};
    false ->
      {false, Count}
  end,
  solutions(N, X+1, Y, NewCount).

solutions_by_factors(N) ->
  Factors = 
  lists:foldl(
    fun(P, Product) ->
        P * Product
    end,
    1,
    lists:map(
      fun({_, P}) ->
          2*P + 1
      end,
      factor(N))),
  (Factors + 1) div 2.

solve(N) ->
  solve(N, N+1, []).

solve(_N, _X, [{X1,Y1} | _T] = Acc) when X1 >= Y1 ->
  lists:reverse(Acc);
solve(N, X, Acc) ->
  XN1 = X * N,
  XN2 = X - N,
  NewAcc =
  case XN1 rem XN2 =:= 0 of
    true ->
      Y = XN1 div XN2,
      [{X,Y} | Acc];
    false ->
      Acc
  end,
  solve(N, X+1, NewAcc).

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

factor_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([{1,1}], factor(1)),
        ?_assertEqual([{2,1}], factor(2)),
        ?_assertEqual([{3,1}], factor(3)),
        ?_assertEqual([{2,2}], factor(4)),
        ?_assertEqual([{5,1}], factor(5)),
        ?_assertEqual([{2,1}, {3,1}], factor(6)),
        ?_assertEqual([{7,1}], factor(7)),
        ?_assertEqual([{2,3}], factor(8)),
        ?_assertEqual([{3,2}], factor(9)),
        ?_assertEqual([{2,1}, {5,1}], factor(10))
       ]
   end
  }.

is_prime_test_() ->
  [
   ?_assertEqual(
      [2,3,5,7,11,13,17,19],
      lists:filter(fun is_prime/1, lists:seq(1, 20)))
  ].

primes_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([], primes(1)),
        ?_assertEqual([2,3,5,7], primes(10)),
        ?_assertEqual([2,3,5,7], primes(10)),
        ?_assertEqual([2,3,5,7], primes(7)),
        ?_assertEqual([2,3,5,7,11,13,17,19], primes(19))
       ]
   end
  }.

solve_test_() ->
  [
   ?_assertEqual([{5,20}, {6,12}, {8,8}], solve(4))
  ].

solutions_test_() ->
  [
   ?_assertEqual(length(solve(1)), solutions(1)),
   ?_assertEqual(length(solve(2)), solutions(2)),
   ?_assertEqual(length(solve(3)), solutions(3)),
   ?_assertEqual(length(solve(4)), solutions(4)),
   ?_assertEqual(length(solve(5)), solutions(5))
  ].

solutions_by_factors_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(solutions(2), solutions_by_factors(2)),
        ?_assertEqual(solutions(3), solutions_by_factors(3)),
        ?_assertEqual(solutions(4), solutions_by_factors(4)),
        ?_assertEqual(solutions(5), solutions_by_factors(5)),
        ?_assertEqual(solutions(6), solutions_by_factors(6)),
        ?_assertEqual(solutions(10), solutions_by_factors(10))
       ]
   end
  }.

-endif.
