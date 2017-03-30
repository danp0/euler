-module(same).

-export(
   [
    count/1,
    difference/2,
    init/0,
    main/0,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    counts = gb_trees:empty()
   }
  ).

%%
%% n =
%% x^2 - (x-m)^2 - (x-2m)^2 =
%% x^2 - (x^2 - 2xm + m^2) - (x^2 - 4xm + 4m^2) =
%% -x^2 + 6mx - 5m^2 =
%% -(x^2 - 6mx + 5m^2) =
%% -(x - 5m)(x - m)
%%
%% n1 =
%% (x+2m)^2 - (x+m)^2 - x^2 =
%% x^2 + 4mx + 4m^2 - x^2 - 2mx - m^2 - x^2 =
%% -x^2 + 2mx + 3m^2 =
%% -(x^2 - 2mx - 3m^2) =
%% -(x - 3m)(x + m)
%%
%% 2m - 2x = 0
%% x = m
%%
%% n1 > 0 when x < 3m
%%
%% n2 =
%% -(x+m)^2 + 2m(x+m) + 3m^2 =
%% -(x^2 + 2mx + m^2) + 2mx + 2m^2 + 3m^2 =
%% -x^2 - 2mx - m^2 + 2mx + 5m^2 =
%% -x^2 + 4m^2
%%
%% n2 - n1 =
%% -x^2 + 4m^2 + x^2 - 2mx - 3m^2 =
%% m^2 - 2mx
%%

count(Upto) ->
  Tree = count(1, 1, difference(1,1), Upto, false, gb_trees:empty()),
  gb_trees:to_list(Tree).

count(_X, _M, N, _Upto, false, Ack) when N =< 0 ->
  Ack;
count(_X, M, N, Upto, true, Ack) when N =< 0 ->
  count(1, M+1, difference(1, M+1), Upto, false, Ack);
count(X, M, N, Upto, Changed, Ack) when N >= Upto ->
  count(X+1, M, difference(X+1, M), Upto, Changed, Ack);
count(X, M, N, Upto, _Changed, Ack) ->
  AckNext =
  case gb_trees:lookup(N, Ack) of
    none ->
      gb_trees:enter(N, 1, Ack);
    {value, Value} ->
      gb_trees:enter(N, Value+1, Ack)
  end,
  count(X+1, M, difference(X+1, M), Upto, true, AckNext).

difference(X, M) ->
  X1 = X,
  X2 = X+M,
  X3 = X+2*M,
  X3*X3 - X2*X2 - X1*X1.

init() ->
  loop(#state{}).

loop(State) ->
  NewState =
  receive
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  io:format("same difference...~n", []),
  start(),
  lists:foreach(
    fun(M) ->
        lists:foreach(
          fun(X) ->
              io:format("M: ~p, X: ~p: ~p~n", [M, X, difference(X, M)])
          end,
          lists:seq(1, 20))
    end,
    lists:seq(3, 7)),
  ok = stop(),
  Counts = 
  lists:filter(
    fun({_N, C}) -> C =:= 10 end, 
    count(1000000)),
  io:format("~p~n", [Counts]),
  io:format("~p~n", [length(Counts)]),
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

-endif.
