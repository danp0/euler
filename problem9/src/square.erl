-module(square).

-behaviour(gen_server).

-export(
   [
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2
   ]
  ).

-export(
   [
    sqr/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state, 
    {
      dict
    }
  ).

-include_lib("eunit/include/eunit.hrl").

sqr(N) ->
  gen_server:call(?MODULE, {sqr, N}).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call({sqr, N}, _From, #state{dict = Dict} = State) ->
  {Reply, NewDict} =
  case dict:find(N, Dict) of
    {ok, Squared} ->
      {Squared, Dict};
    _ ->
      Squared = N * N,
      {Squared, dict:store(N, Squared, Dict)}
  end,
  {reply, Reply, State#state{dict = NewDict}};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
  {reply, undefined, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

init([]) ->
  {ok, #state{dict = dict:new()}}.

terminate(_Reason, _State) ->
  ok.

%%
%% tests
%%
square_test_() ->
  {
   setup,
   fun() -> square:start() end,
   fun(_) -> square:stop() end,
   [
    ?_assertEqual(1, square:sqr(1)),
    ?_assertEqual(4, square:sqr(2)),
    ?_assertEqual(9, square:sqr(3)),
    ?_assertEqual(9, square:sqr(3)),
    ?_assertEqual(81, square:sqr(9))
   ]
  }.

