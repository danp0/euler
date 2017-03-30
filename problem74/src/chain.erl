-module(chain).

-export(
   [
    cf/1,
    find/2,
    fsum/1,
    init/0,
    main/0,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    factorial,
    fsum=gb_trees:empty()
   }
  ).

cf(N) ->
  cf(N, []).

cf(N, Acc) ->
  case lists:member(N, Acc) of
    true ->
      {lists:reverse(Acc), N};
    false ->
      cf(fsum(N), [N | Acc])
  end.

find(F, N) ->
  find(1, N, F, []).

find(I, N, _F, Acc) when I > N ->
  lists:reverse(Acc);
find(I, N, F, Acc) ->
  CF = cf(I),
  case F(CF) of
    true ->
      find(I+1, N, F, [CF | Acc]);
    false ->
      find(I+1, N, F, Acc)
  end.

fsum(N) ->
  chain ! {fsum, N, self()},
  receive
    Result ->
      Result
  end.

fsum(N, State) ->
  Digits = 
  lists:sort(lists:map(fun(D) -> D - $0 end, integer_to_list(N))),
  case gb_trees:lookup(Digits, State#state.fsum) of
    none ->
      FSum =
      lists:sum(
        lists:map(
          fun(D) -> array:get(D, State#state.factorial) end, 
          Digits)),
      {FSum, 
       State#state{fsum = gb_trees:insert(Digits, FSum, State#state.fsum)}};
    {value, Value} ->
      {Value, State}
  end.

init() ->
  Factorial = 
  lists:map(
    fun (0) ->
        1;
        (N) -> 
        lists:foldl(fun(I,P) -> I*P end, 1, lists:seq(1,N))
    end,
    lists:seq(0,9)),
  loop(#state{factorial=array:from_list(Factorial)}).

loop(State) ->
  NewState =
  receive
    {fsum, N, From} ->
      {FSum, State2} = fsum(N, State),
      From ! FSum,
      State2;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  CFs = 
  find(
    fun({CF, _N}) -> 
        length(CF) =:= 60 
    end, 
    999999),
  io:format("~p~n", [CFs]),
  io:format("count: ~p~n", [length(CFs)]),
  ok = stop(),
  ok.

start() ->
  register(chain, spawn(?MODULE, init, [])).

stop() ->
  chain ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cf_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual({[145], 145}, cf(145)),
    ?_assertEqual({[169, 363601, 1454], 169}, cf(169)),
    ?_assertEqual({[871, 45361], 871}, cf(871)),
    ?_assertEqual({[69, 363600, 1454, 169, 363601], 1454}, cf(69)),
    ?_assertEqual({[78, 45360, 871, 45361], 871}, cf(78))
   ]
  }.

fsum_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(145, fsum(145)),
    ?_assertEqual(145, fsum(451)),
    ?_assertEqual(145, fsum(541))
   ]
  }.

-endif.
