-module(pp1).

-export(
   [
    generate/0,
    generate/2,
    generate/3,
    init/0,
    is_pandigital/1,
    is_prime/1,
    main/0,
    start/0,
    stop/0,
    subsets/1
   ]
  ).

-record(
   state,
   {
    subsets = gb_trees:empty(),
    subseqs = gb_trees:empty()
   }
  ).

%% {123}, {1, 23}, {2, 13}, {3, 12}, {1, 2, 3}
%% {1234}, {1, 234}, {2, 134}, {3, 124}, {4, 123}
%%         {12, 34}, {13, 24}, {14, 23}
%%         {1, 2, 34}, {1, 3, 24}, {1, 4, 23}, {3, 4, 12}, {2, 4, 13},
%%         {2, 3, 14}
%%         {1, 2, 3, 4}
%%

generate() ->
  [
   S || 
   N <- lists:seq(1,9), 
   S <- generate(N, "123456789"),
   is_prime(list_to_integer(S))].

generate(1, Digits) ->
  [[D] || D <- Digits];
generate(N, Digits) ->
  case send({subseqs, {N, Digits}}) of
    none ->
      Subseqs = [[D] ++ S || D <- Digits, S <- generate(N-1, Digits -- [D])],
      send({subseqs, {N, Digits}, Subseqs}),
      Subseqs;
    {value, Value} ->
      Value
  end.

generate(N, Digits, Fun) ->
  [
   S ||
   S <- generate(N, Digits),
   Fun(S)].

init() ->
  loop(#state{}).

is_pandigital(N) ->
  length(N) =:= length(lists:usort(N)).

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

loop(State) ->
  NewState =
  receive
    {From, {subseqs, Key}} ->
      Subseqs = gb_trees:lookup(Key, State#state.subseqs),
      From ! Subseqs,
      State;
    {From, {subseqs, Key, Value}} ->
      NewSubseqs = gb_trees:enter(Key, Value, State#state.subseqs),
      From ! ok,
      State#state{subseqs = NewSubseqs};
    {From, {subsets, Key}} ->
      Subsets = gb_trees:lookup(Key, State#state.subsets),
      From ! Subsets,
      State;
    {From, {subsets, Key, Value}} ->
      NewSubsets = gb_trees:enter(Key, Value, State#state.subsets),
      From ! ok,
      State#state{subsets = NewSubsets};
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  io:format("pandigital prime sets~n", []),
  start(),
  %PanDigitalPrimes = generate(),
  %io:format("~p~n", [PanDigitalPrimes]),
  %io:format("length: ~p~n", [length(PanDigitalPrimes)]),
  %%io:format("~p~n", [generate(1, "123456789", fun(S) -> is_prime(list_to_integer(S)) end)]),
  %%io:format("~p~n", [generate(2, "123456789", fun(S) -> is_prime(list_to_integer(S)) end)]),
  %%io:format("~p~n", [generate(3, "123456789", fun(S) -> is_prime(list_to_integer(S)) end)]),
  %%Set =
  %%[
  %% {E1, E2} ||
  %% E1 <- generate(1, "123456789", fun(S) -> is_prime(list_to_integer(S)) end),
  %% E2 <- generate(8, "123456789" -- E1, fun(S) -> is_prime(list_to_integer(S)) end)
  %%],
  %%io:format("~p~n", [Set]),
  lists:foreach(
    fun(S) ->
        Subsets = subsets(S),
        io:format("~p~n~p~n~p~n", [S, Subsets, length(Subsets)])
    end,
   ["123456789"]), 
  %%io:format("~p~n", [proper_subsets("1234")]),
  ok = stop(),
  ok.

proper_subsets(S) ->
  case send({subsets, S}) of
    none ->
      Subsets = [S1 || N <- lists:seq(1, length(S)), D <- generate(N, S, fun is_prime/1), S1 <- subsets(D, S -- D)],
      send({subsets, S, Subsets}),
      Subsets;
    {value, Value} ->
      Value
  end.

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  send(stop).

subsets(S) ->
  Subsets =
  case is_prime(S) of
    true ->
      [[S] | proper_subsets(S)];
    false ->
      proper_subsets(S)
  end,
  lists:filter(
    fun(S1) ->
        lists:all(fun is_prime/1, S1)
    end,
  lists:usort(lists:map(fun lists:sort/1, Subsets))).

subsets(S1, []) ->
  [S1];
subsets(S1, S2) ->
  case is_prime(S2) of
    true ->
      [[S1, S2] | [[S1 | S3] || S3 <- proper_subsets(S2)]];
    false ->
      [[S1 | S3] || S3 <- proper_subsets(S2)]
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_pandigital_test_() ->
  [
   ?_assert(is_pandigital("123")),
   ?_assertNot(is_pandigital("111")),
   ?_assertNot(is_pandigital("121")),
   ?_assertNot(is_pandigital("122")),
   ?_assertNot(is_pandigital("221"))
  ].

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

-endif.
