-module(blocks).

-export(
   [
    c/2,
    exceeds/2,
    f/2,
    init/0,
    main/0,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    c = gb_trees:empty()
   }
  ).

c(_N, 0) ->
  1;
c(N, N) ->
  1;
c(N, K) when N < K ->
  0;
c(N, K) ->
  lookup(N, K).

exceeds(M, Boundary) ->
  exceeds(M, M, Boundary).

exceeds(M, N, Boundary) ->
  case f(M, N) > Boundary of
    true ->
      N;
    false ->
      exceeds(M, N+1, Boundary)
  end.

%%
%% M: miniumum block width.
%% N: row length.
%%
%% x1 + x2 + ... xi = N
%% xi > ai
%% Number of integral solutions: c(N - a1 - a2 - ... - ai - 1, i - 1).
%%
%% Blocks:
%% b1 + r2 + ...  = N
%% r1 + b2 + ...  = N 
%% bi > 0 and ri > M-1
%% f(M,N) = sum(c(N - (M-1)*(I/2) - 1, I-1) + c(N - (M-1)*(I/2 + I%2) - 1, I-1))
%% for I >= 1
%%
f(M, N) ->
  f(M, N, 1, 0).

f(M, N, I, Ack) ->
  C1 = c(N - (M - 1)*(I div 2) - 1, I - 1),
  C2 = c(N - (M - 1)*((I div 2) + (I rem 2)) - 1, I - 1),
  case C1 + C2 of
    0 ->
      Ack;
    Sum ->
      f(M, N, I+1, Ack + Sum)
  end.

init() ->
  loop(#state{}).

lookup(N, K) ->
  case send({lookup, {N, K}}) of
    none ->
      C = c(N-1, K-1) + c(N-1, K),
      send({enter, {N, K}, C}),
      C;
    {value, Value} ->
      Value
  end.

loop(State) ->
  NewState =
  receive
    {From, {enter, Key, Value}} ->
      C = gb_trees:enter(Key, Value, State#state.c),
      From ! ok,
      State#state{c = C};
    {From, {lookup, Key}} ->
      From ! gb_trees:lookup(Key, State#state.c),
      State;
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("exceeds 50: ~p~n", [exceeds(50, 1000000)]),
  ok = stop(),
  ok.

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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

c_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(0, c(1,2)),
        ?_assertEqual(1, c(1,0)),
        ?_assertEqual(1, c(1,1)),
        ?_assertEqual(3, c(3,2)),
        ?_assertEqual(15, c(6,2)),
        ?_assertEqual(20, c(6,3))
       ]
   end
  }.

exceeds_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(30, exceeds(3, 1000000)),
        ?_assertEqual(57, exceeds(10, 1000000))
       ]
   end
  }.

f_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(2, f(3,3)),
        ?_assertEqual(4, f(3,4)),
        ?_assertEqual(7, f(3,5)),
        ?_assertEqual(673135, f(3,29)),
        ?_assertEqual(1089155, f(3,30)),
        ?_assertEqual(880711, f(10, 56)),
        ?_assertEqual(1148904, f(10, 57))
       ]
   end
  }.

-endif.
