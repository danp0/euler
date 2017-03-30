-module(amicable).

-export(
   [
    chain/1,
    init/1,
    main/0,
    map_divisible_sums/1,
    next/1,
    proper_divisors/1,
    start/1,
    stop/0,
    sum_proper_divisors/1
   ]
  ).

-record(
   state,
   {
    sums
   }
  ).

chain(N) ->
  chain(N, next(N), [N]).

chain(_N, none, _Acc) ->
  none;
chain(N, {value, N}, Acc) ->
  lists:reverse([N|Acc]);
chain(N, {value, Value}, Acc) ->
  case lists:any(fun(E) -> E =:= Value end, Acc) of
    true ->
      none;
    false ->
      chain(N, next(Value), [Value|Acc])
  end.

init(Sums) ->
  loop(#state{sums = gb_trees:from_orddict(lists:sort(Sums))}).

loop(State) ->
  NewState =
  receive 
    {next, N, From} ->
      From ! gb_trees:lookup(N, State#state.sums),
      State;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  Max = 1000000,
  Sums = map_divisible_sums(Max),
  start(Sums),
  Chains =
  lists:filtermap(
    fun(N) ->
        case chain(N) of
          none ->
            false;
          Chain ->
            {true, {length(Chain), Chain}}
        end
    end,
    lists:seq(2, Max)),
  SortedChains = lists:sort(fun(L1, L2) -> L2 =< L1 end, Chains),
  {MaxLength, _} = hd(SortedChains),
  MaxChains =
  lists:takewhile(
    fun({L, _C}) -> L =:= MaxLength end,
    SortedChains),
  Min = 
  lists:min(
    lists:flatten(
      lists:map(
        fun({_L, C}) -> 
            C 
        end, 
        MaxChains))),
  io:format("min: ~p~n", [Min]),
  ok = stop(),
  ok.

map_divisible_sums(UpTo) ->
  lists:filter(
    fun({_I, Sum}) ->
        Sum > 1 andalso Sum =< UpTo
    end,
    lists:map(
      fun(I) ->
          {I, sum_proper_divisors(I)}
      end,
      lists:seq(2, UpTo))).

next(N) ->
  ?MODULE ! {next, N, self()},
  receive
    Result ->
      Result
  end.

proper_divisors(N) ->
  proper_divisors(2, trunc(math:sqrt(N)), N, [1]).

proper_divisors(I, SqrtN, _N, Acc) when I > SqrtN ->
  lists:sort(Acc);
proper_divisors(I, SqrtN, N, Acc) ->
  AccNew =
  case N rem I =:= 0 of
    true ->
      case {I, N div I} of
        {I, I} ->
          [I|Acc];
        {I, NdivI} ->
          [I, NdivI|Acc]
      end;
    false ->
      Acc
  end,
  proper_divisors(I+1, SqrtN, N, AccNew).

start(Sums) ->
  register(?MODULE, spawn(?MODULE, init, [Sums])).

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Result ->
      Result
  end.

sum_proper_divisors(N) ->
  sum_proper_divisors(2, trunc(math:sqrt(N)), N, 1).

sum_proper_divisors(I, SqrtN, _N, Sum) when I > SqrtN ->
  Sum;
sum_proper_divisors(I, SqrtN, N, Sum) ->
  SumNew =
  case N rem I =:= 0 of
    true ->
      case {I, N div I} of
        {I, I} ->
          Sum + I;
        {I, NdivI} ->
          Sum + I + NdivI
      end;
    false ->
      Sum
  end,
  sum_proper_divisors(I+1, SqrtN, N, SumNew).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

chain_test_() ->
  {
   setup,
   fun() -> start(map_divisible_sums(16000)) end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(none, chain(27)),
    ?_assertEqual([28, 28], chain(28)),
    ?_assertEqual([220, 284, 220], chain(220)),
    ?_assertEqual([12496, 14288, 15472, 14536, 14264, 12496], chain(12496))
   ]
  }.

map_divisible_sums_test_() ->
  [
   ?_assertEqual(
      [{4, 3}, {6, 6}, {8, 7}, {9, 4}, {10, 8}],
      map_divisible_sums(10))
  ].

next_test_() ->
  {
   setup,
   fun() -> start(map_divisible_sums(10)) end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(none, next(2)),
    ?_assertEqual(none, next(3)),
    ?_assertEqual({value, 3}, next(4)),
    ?_assertEqual(none, next(5)),
    ?_assertEqual({value, 6}, next(6)),
    ?_assertEqual(none, next(7)),
    ?_assertEqual({value, 7}, next(8)),
    ?_assertEqual({value, 4}, next(9)),
    ?_assertEqual({value, 8}, next(10)),
    ?_assertEqual(none, next(11))
   ]
  }.

proper_divisors_test_() ->
  [
   ?_assertEqual([1], proper_divisors(1)),
   ?_assertEqual([1], proper_divisors(2)),
   ?_assertEqual([1], proper_divisors(3)),
   ?_assertEqual([1,2], proper_divisors(4)),
   ?_assertEqual([1,2,4,5,10], proper_divisors(20)),
   ?_assertEqual([1,2,4,5,10,20,25,50], proper_divisors(100))
  ].

sum_proper_divisors_test_() ->
  [
   ?_assertEqual(lists:sum(proper_divisors(1)), sum_proper_divisors(1)),
   ?_assertEqual(lists:sum(proper_divisors(2)), sum_proper_divisors(2)),
   ?_assertEqual(lists:sum(proper_divisors(3)), sum_proper_divisors(3)),
   ?_assertEqual(lists:sum(proper_divisors(4)), sum_proper_divisors(4)),
   ?_assertEqual(lists:sum(proper_divisors(20)), sum_proper_divisors(20)),
   ?_assertEqual(lists:sum(proper_divisors(100)), sum_proper_divisors(100))
  ].

-endif.
