-module(exponentiation).

-export(
   [
    init/0,
    m/1,
    main/0,
    start/0,
    stop/0,
    sum/1
   ]
  ).

-record(
   state,
   {
    tree = gb_trees:empty()
   }
  ).

%%
%% 1: 1
%% 2: 2
%% 3: 3 4
%% 4: 5 6 8
%% 5: 7 9 10 11 12 16
%% 6: 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24
%%
%%                              1
%%                              2
%%              3                               4
%%    4         5        6          5           6         8
%% 5,6,7,8  6,7,8,10  7,8,9,12   6,7,9,10   7,8,10,12  9,10,12,16
%% 
%% [1,[[2, [[3, [4,5,6]], [4, [5,6,8]]]]]]

init() ->
  loop(#state{}).

loop(State) ->
  NewState =
  receive
    {From, {enter, Key, Value}} ->
      Tree = gb_trees:enter(Key, Value, State#state.tree),
      From ! ok,
      State#state{tree = Tree};
    {From, {is_defined, Key}} ->
      From ! gb_trees:is_defined(Key, State#state.tree),
      State;
    {From, {lookup, Key}} ->
      From ! gb_trees:lookup(Key, State#state.tree),
      State;
    {From, size} ->
      From ! gb_trees:size(State#state.tree),
      State;
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

m(Upto) ->
  send({enter, 1, 0}),
  m(1, Upto, [1]),
  lists:map(
    fun
      (none) -> none;
      ({value, Value}) -> Value
    end,
    [send({lookup, I}) || I <- lists:seq(1,Upto)]).

m(I, Upto, [H|_T]=Seq) ->
  %io:format("~p: ~p~n", [I, Seq]),
  Next = [H+E || E <- Seq],
  lists:foreach(
    fun(E) ->
        case (E > Upto) orelse send({is_defined, E}) of
          true ->
            skip;
          false ->
            send({enter, E, I})
        end
    end,
    Next),
  case send(size) < Upto of
    true ->
      [m(I+1, Upto, [E|Seq]) || E <- Next, E < Upto];
    false ->
      ok
  end.

main() ->
  io:format("exponentation...~n", []),
  io:format("~p~n", [sum(200)]),
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

sum(Upto) ->
  {ok, File} = file:read_file("b003313.txt"),
  lists:sum(
    lists:map(
      fun({_I, M}) ->
          M
      end,
      lists:takewhile(
        fun({I, _M}) ->
            I =< Upto
        end,
        lists:map(
          fun(Line) ->
              [I, M] = re:split(Line, "\s+", [{return, list}]),
              {list_to_integer(I), list_to_integer(M)}
          end,
          re:split(File, "\n", [{return, list}, trim]))))).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
