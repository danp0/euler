-module(bouncy).

-export(
   [
    choose/2,
    count_non_bouncy/1,
    count_non_bouncy_upto/1,
    init/0,
    is_bouncy/1,
    main/0,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    choose
   }
  ).

%%
%% increasing:
%%
%%      (0,0)
%%           1 2 3 4 5 6 7 8 9            
%%           1 2 3 4 5 6 7 8 9 (8,1)
%%           1 2 3 4 5 6 7 8 9 (8,2)
%%           1 2 3 4 5 6 7 8 9 (8,3)
%%
%% decreasing:
%%
%%      (0,0)
%%           9 8 7 6 5 4 3 2 1 0
%%           9 8 7 6 5 4 3 2 1 0 (9,1)
%%           9 8 7 6 5 4 3 2 1 0 (9,2)
%%           9 8 7 6 5 4 3 2 1 0 (9,3)
%%
%%    NB(1) =  9 C 8 + 10 C 9 - 10 =  9 + 10 - 10   =   9
%%    NB(2) = 10 C 8 + 11 C 9 - 10 = 45 + 55 - 10   =  90
%%    NB(3) = 11 C 8 + 12 C 9 - 10 = 165 + 220 - 10 = 375
%%
%%    NB(N) = C(N+8,8) + C(N+9,9) - 10
%%
choose(_N, 0) ->
  1;
choose(N, N) ->
  1;
choose(N, K) ->
  case send({lookup, N, K}) of
    none ->
      Choose = choose(N-1, K-1) + choose(N-1, K),
      send({enter, N, K, Choose}),
      Choose;
    {value, Value} ->
      Value
  end.

count_non_bouncy(Digits) ->
  lists:sum(
    [choose(N+8,8) + choose(N+9,9) - 10 || N <- lists:seq(1,Digits)]).

count_non_bouncy_upto(N) ->
  count_non_bouncy_upto(1, N, 0).

count_non_bouncy_upto(From, To, Sum) when From > To ->
  Sum;
count_non_bouncy_upto(From, To, Sum) ->
  count_non_bouncy_upto(From+1, To, Sum +
                        case is_bouncy(From) of
                          true ->
                            0;
                          false ->
                            1
                        end).

init() ->
  loop(#state{choose=gb_trees:empty()}).

is_bouncy(N) ->
  Digits = integer_to_list(N),
  is_bouncy(hd(Digits), tl(Digits)).

is_bouncy(_H, []) ->
  false;
is_bouncy(H1, [H2|T] = L) ->
  if
    H1 =:= H2 ->
      is_bouncy(H2, T);
    H1 > H2 ->
      L =/= lists:sort(fun(D1,D2) -> D1 > D2 end, L);
    H1 < H2 ->
      L =/= lists:sort(L)
  end.

loop(State) ->
  NewState =
  receive
    {{enter, N, K, Choose}, From} ->
      NewTree = gb_trees:enter({N,K}, Choose, State#state.choose),
      From ! ok,
      State#state{choose = NewTree};
    {{lookup, N, K}, From} ->
      From ! gb_trees:lookup({N,K}, State#state.choose),
      State;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("~p: ~p~n", [100, count_non_bouncy(100)]),
  ok = stop(),
  ok.

send(Msg) ->
  ?MODULE ! {Msg, self()},
  receive
    Result ->
      Result
  end.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

choose_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(1, choose(1,1)),
    ?_assertEqual(1, choose(1,0)),
    ?_assertEqual(2, choose(2,1)),
    ?_assertEqual(3, choose(3,2)),
    ?_assertEqual(15, choose(6,2))
   ]
  }.

count_non_bouncy_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(9, count_non_bouncy(1)),
    ?_assertEqual(99, count_non_bouncy(2)),
    ?_assertEqual(474, count_non_bouncy(3)),
    ?_assertEqual(1674, count_non_bouncy(4)),
    ?_assertEqual(4953, count_non_bouncy(5)),
    ?_assertEqual(12951, count_non_bouncy(6)),
    ?_assertEqual(277032, count_non_bouncy(10))
   ]
  }.

is_bouncy_test_() ->
  [
   ?_assertNot(is_bouncy(1)),
   ?_assertNot(is_bouncy(9)),
   ?_assertNot(is_bouncy(10)),
   ?_assertNot(is_bouncy(11)),
   ?_assertNot(is_bouncy(12)),
   ?_assertNot(is_bouncy(23)),
   ?_assertNot(is_bouncy(99)),
   ?_assertNot(is_bouncy(100)),
   ?_assert(is_bouncy(101)),
   ?_assert(is_bouncy(102)),
   ?_assertNot(is_bouncy(110)),
   ?_assert(is_bouncy(120)),
   ?_assertNot(is_bouncy(123)),
   ?_assertNot(is_bouncy(321)),
   ?_assertNot(is_bouncy(600)),
   ?_assert(is_bouncy(989)),
   ?_assertNot(is_bouncy(999)),
   ?_assertNot(is_bouncy(1000)),
   ?_assert(is_bouncy(1001)),
   ?_assertNot(is_bouncy(1100)),
   ?_assert(is_bouncy(1101)),
   ?_assert(is_bouncy(9102)),
   ?_assert(is_bouncy(91023)),
   ?_assert(is_bouncy(91523)),
   ?_assertNot(is_bouncy(99983)),
   ?_assertNot(is_bouncy(123456)),
   ?_assertNot(is_bouncy(999999))
  ].

-endif.
