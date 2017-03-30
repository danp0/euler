-module(pp).

-export(
   [
    count_subsets/1,
    init/1,
    is_prime/1,
    is_sublist/2,
    iterate/1,
    main/0,
    start/1,
    stop/0,
    subseq/1,
    subseq/2,
    subseq/3,
    subsets/1
   ]
  ).

-record(
   state,
   {
    tree = gb_trees:empty()
   }
  ).

count_subsets(Subsets) ->
  lists:sum(
  lists:map(
    fun(S) ->
        lists:foldl(
          fun(Key, Prod) ->
              {value, Value} = send({lookup, Key}),
              length(Value) * Prod
          end,
          1,
          S)
    end,
    Subsets)).

init(Digits) ->
  Tree =
  lists:foldl(
    fun(S, T) ->
        Key = lists:sort(S),
        case gb_trees:lookup(Key, T) of
          none ->
            gb_trees:enter(Key, [S], T);
          {value, Value} ->
            gb_trees:enter(Key, [S | Value], T)
        end
    end,
    gb_trees:empty(),
    subseq(Digits)),
  loop(#state{tree = Tree}).

is_prime(N) when is_list(N) ->
  is_prime(list_to_integer(N));
is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I * I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I + 2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

is_sublist([], _L2) ->
  true;
is_sublist(_L1, []) ->
  false;
is_sublist([H1|T1], [H2|T2]) ->
  if
    H1 =:= H2 ->
      is_sublist(T1, T2);
    H1 > H2 ->
      is_sublist([H1|T1], T2);
    H1 < H2 ->
      false
  end.

iterate(Fun) ->
  iterator(Fun, send({next, send(iterator)})).

iterator(Fun, none) ->
  Fun(none);
iterator(Fun, {Key, Value, Iterator}) ->
  Fun({Key, Value}),
  iterator(Fun, send({next, Iterator})).

loop(State) ->
  NewState =
  receive
    {From, iterator} ->
      From ! gb_trees:iterator(State#state.tree),
      State;
    {From, keys} ->
      From ! gb_trees:keys(State#state.tree),
      State;
    {From, {lookup, Key}} ->
      From ! gb_trees:lookup(Key, State#state.tree),
      State;
    {From, {next, Iterator}} ->
      From ! gb_trees:next(Iterator),
      State;
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  Digits = "123456789",
  start(Digits),
  Subsets = subsets(Digits),
  io:format("count: ~p~n", [count_subsets(Subsets)]),
  ok = stop(),
  ok.

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

start(Digits) ->
  register(?MODULE, spawn(?MODULE, init, [Digits])).

stop() ->
  send(stop).

subseq(Digits) ->
  [S || N <- lists:seq(1, length(Digits)), S <- subseq(fun is_prime/1, N, Digits)].

subseq(1, Digits) ->
  [[D] || D <- Digits];
subseq(N, Digits) ->
  [[D] ++ S || D <- Digits, S <- subseq(N-1, Digits -- [D])].

subseq(Filter, N, Digits) ->
  [S || S <- subseq(N, Digits), Filter(S)].

subsets(Digits) ->
  lists:usort(
    lists:map(fun lists:sort/1,
              subsets(Digits, send(keys), []))).

subsets(_Digits, [], Ack) ->
  Ack;
subsets(Digits, [K|T], Ack) ->
  case is_sublist(K, Digits) of
    true ->
      case K =:= Digits of
        true ->
          subsets(Digits, T, [[K] | Ack]);
        false ->
          Subsets = lists:map(fun(S) -> [K|S] end, subsets(Digits -- K)),
          subsets(Digits, T, lists:append([Subsets, Ack]))
      end;
    false ->
      subsets(Digits, T, Ack)
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
  [
   ?_assert(is_prime(2)),
   ?_assert(is_prime(3)),
   ?_assert(is_prime(5)),
   ?_assert(is_prime(7)),
   ?_assert(is_prime(11)),
   ?_assert(is_prime(13)),
   ?_assert(is_prime(17)),
   ?_assert(is_prime(19)),
   ?_assert(is_prime(23)),
   ?_assertNot(is_prime(1)),
   ?_assertNot(is_prime(4)),
   ?_assertNot(is_prime(6)),
   ?_assertNot(is_prime(8)),
   ?_assertNot(is_prime(9)),
   ?_assertNot(is_prime(10)),
   ?_assertNot(is_prime(12)),
   ?_assertNot(is_prime(14)),
   ?_assertNot(is_prime(15)),
   ?_assertNot(is_prime(16)),
   ?_assertNot(is_prime(18)),
   ?_assertNot(is_prime(20))
  ].

is_sublist_test_() ->
  [
   ?_assert(is_sublist("1", "123")),
   ?_assert(is_sublist("12", "123")),
   ?_assert(is_sublist("23", "123")),
   ?_assert(is_sublist("123", "123")),
   ?_assertNot(is_sublist("12345", "12356")),
   ?_assertNot(is_sublist("12345", "12456")),
   ?_assertNot(is_sublist("12345", "13456")),
   ?_assertNot(is_sublist("12345", "2346"))
  ].

keys_test_() ->
  {
   setup,
   fun() -> start("123") end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([2,3,13,23], lists:sort(lists:map(fun list_to_integer/1, send(keys))))
       ]
   end
  }.

subseq_test_() ->
  [
   ?_assertEqual(["2"], subseq("12")),
   ?_assertEqual(["2", "3", "13", "23", "31"], subseq("123"))
  ].

subsets_test_() ->
  {
   setup,
   fun() -> start("123") end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual([["13", "2"]], subsets("123")),
        ?_assertEqual(2, count_subsets(subsets("123")))
       ]
   end
  }.

-endif.
