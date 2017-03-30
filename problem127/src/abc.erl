-module(abc).

-export(
   [
    abc_hits/1,
    factor/1,
    gcd/2,
    init/1,
    main/0,
    rad/1,
    sieve/1,
    start/1,
    stop/0
   ]
  ).

-record(
   state,
   {
    primes,
    radicals = gb_trees:empty()
   }
  ).

%%
%% http://www.abcathome.com/algorithm.php
%%

xradical(N, Yr) ->
  NDivYr2 = N div (Yr*Yr),
  lists:takewhile(fun(R) -> R < NDivYr2 end, send(radicals)).

yradical(N) ->
  SqrtN = trunc(math:sqrt(N)) + 1,
  lists:takewhile(fun(R) -> R < SqrtN end, send(radicals)).

abc_hits(N) ->
  start(N),
  ABC_hits =
  [
   {X1,Y1,Z1} ||
   Yr <- yradical(N),
   Y <- send({radical_of, Yr}),
   Xr <- xradical(N, Yr),
   X <- send({radical_of, Xr}),
   Z <- [X+Y, abs(X-Y)],
   [X1, Y1, Z1] <- [lists:sort([X,Y,Z])],
   X1 > 0,
   Z1 < N,
   gcd(X1, Y1) =:= 1,
   Xr * Yr * rad(Z) < Z1
  ],
  ok = stop(),
  lists:usort(ABC_hits).

factor(N) when N < 2 ->
  [N];
factor(N) ->
  Primes = send({primes, trunc(math:sqrt(N)) + 1}),
  factor(N, Primes, []).

factor(1, [], Acc) ->
  lists:reverse(Acc);
factor(N, [], Acc) ->
  factor(1, [], [N|Acc]);
factor(N, [P|_T], Acc) when P*P > N ->
  AccNext =
  case N > 1 of
    true ->
      [N | Acc];
    false ->
      Acc
  end,
  factor(1, [], AccNext);
factor(N, [P|_T] = Primes, Acc) when N rem P =:= 0 ->
  factor(N div P, Primes, [P|Acc]);
factor(N, [_P|T], Acc) ->
  factor(N, T, Acc).

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

init(N) ->
  loop(#state{primes = sieve(trunc(math:sqrt(N)) + 1)}).

loop(State) ->
  NextState =
  receive
    {From, {enter_radical, R, N}} ->
      L =
      case gb_trees:lookup(R, State#state.radicals) of
        none ->
          [N];
        {value, Value} ->
          [N | Value]
      end,
      Radicals = gb_trees:enter(R, L, State#state.radicals),
      From ! ok,
      State#state{radicals = Radicals};
    {From, {primes, N}} ->
      From ! lists:takewhile(fun(P) -> P =< N end, State#state.primes),
      State;
    {From, {radical_of, N}} ->
      From !
      case gb_trees:lookup(N, State#state.radicals) of
        none ->
          [];
        {value, Value} ->
          Value
      end,
      State;
    {From, radicals} ->
      From ! gb_trees:keys(State#state.radicals),
      State;
    {From, sort_radicals} ->
      Radicals =
      gb_trees:map(fun(_K, V) -> lists:sort(V) end, State#state.radicals),
      From ! ok,
      State#state{radicals = Radicals};
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NextState).

main() ->
  ABC_hits = abc_hits(120000),
  io:format("~p~n", [ABC_hits]),
  io:format("~p~n", [length(ABC_hits)]),
  io:format("~p~n", [lists:sum(lists:map(fun({_,_,C}) -> C end, ABC_hits))]),
  ok.

rad(L) when is_list(L) ->
  lists:foldl(
    fun(I, Acc) ->
        I*Acc
    end,
    1,
    lists:usort(
      lists:append(
        lists:map(fun(N) -> factor(N) end, L))));
rad(N) ->
  lists:foldl(
    fun(I, Acc) ->
        I*Acc
    end,
    1,
    lists:usort(factor(N))).

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

sieve(N) ->
  Sieve =
  array:set(0, false, 
            array:set(1, false, 
                      array:new(N+1, [{fixed, true}, {default, true}]))),
  sieve(2, N, Sieve).

sieve(I, N, Sieve) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun(J, true, Acc) ->
          [J|Acc];
         (_J, false, Acc) ->
          Acc
      end,
      [],
      Sieve));
sieve(I, N, Sieve) ->
  SieveNext =
  case array:get(I, Sieve) of
    true ->
      lists:foldl(
        fun(J, S) ->
            array:set(J, false, S)
        end,
        Sieve,
        lists:seq(I*I, N, I));
    false ->
      Sieve
  end,
  sieve(I+1, N, SieveNext).

start(N) ->
  register(?MODULE, spawn(?MODULE, init, [N])),
  lists:foreach(
    fun({I, R}) ->
        send({enter_radical, R, I})
    end,
    [{I, rad(I)} || I <- lists:seq(1,N)]),
  send(sort_radicals),
  ok.

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

abc_hits_test_() ->
  ABC_hits = abc_hits(1000),
  [
   ?_assertEqual(31, length(ABC_hits)),
   ?_assertEqual(12523, lists:sum([C || {_,_,C} <- ABC_hits]))
  ].

factor_test_() ->
  {
   setup,
   fun() -> start(20) end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(
           [[1],[2],[3],[2,2],[5],[2,3],[7],[2,2,2],[3,3],[2,5],
            [11],[2,2,3],[13],[2,7],[3,5],[2,2,2,2],[17],[2,3,3],[19],[2,2,5]], 
           [factor(N) || N <- lists:seq(1,20)]),
        ?_assertEqual([2,2,2,2,2], factor(32))
       ]
   end
  }.

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2,0)),
   ?_assertEqual(2, gcd(0,2)),
   ?_assertEqual(2, gcd(2,8)),
   ?_assertEqual(2, gcd(8,2)),
   ?_assertEqual(1, gcd(11,13)),
   ?_assertEqual(1, gcd(13,11))
  ].

rad_test_() ->
  {
   setup,
   fun() -> io:format("rad_test_", []), start(504) end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(
           [1,2,3,2,5,6,7,2,3,10,11,6,13,14,15,2,17,6,19,10],
           [rad(I) || I <- lists:seq(1,20)]),
        ?_assertEqual(42, rad(504)),
        ?_assertEqual(rad(21), rad([3,7]))
       ]
   end
  }.

sieve_test_() ->
  [
   ?_assertEqual([], sieve(1)),
   ?_assertEqual([2], sieve(2)),
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.

