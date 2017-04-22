-module(power_digit_sum).

-export(
   [
    add/3,
    bc/2,
    bc_task/1,
    explode/1,
    get/2,
    get_or_set/2,
    main/0,
    power_of_2/1,
    start/0,
    stop/0,
    sum_digits_power_of_2/1,
    sum_pow/2
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the sum of the digits in 2^1000.
%

-record(state, {
          map = dict:new()
         }
       ).

add(N, K, Choices) ->
  ?MODULE ! {add, N, K, Choices, self()},
  receive
    ok ->
      true;
    _ ->
      false
  end.

bc(N, 0) when N >= 0 ->
  1;
bc(N, N) when N >= 0 ->
  1;
bc(N, K) when N >= 2, K >= 1, K =< N-1 ->
  get_or_set(N-1, K-1) + get_or_set(N-1, K).

bc_task(State) ->
  receive
    {add, N, K, Choices, Sender} ->
      Sender ! ok,
      bc_task(State#state{map = dict:store({N,K}, Choices, State#state.map)});
    {get, N, K, Sender} ->
      Sender !
      case dict:find({N,K}, State#state.map) of
        {ok, Choices} ->
          Choices;
        error ->
          not_found
      end,
      bc_task(State);
    {exit, Sender} ->
      Sender ! ok
  end.

explode(N) ->
  explode(N, []).

explode(N, Acc) when N < 10 ->
  [N | Acc];
explode(N, Acc) ->
  explode(N div 10, [N rem 10 | Acc]).

get(N, K) ->
  ?MODULE ! {get, N, K, self()},
  receive
    Result ->
      Result
  end.

get_or_set(N, K) ->
  case get(N, K) of
    not_found ->
      Choices = bc(N, K),
      add(N, K, Choices),
      Choices;
    Choices ->
      Choices
  end.

main() ->
  start(),
  {T1, S1} = timer:tc(?MODULE, sum_digits_power_of_2, [1000]),
  {T2, S2} = timer:tc(?MODULE, sum_pow, [2, 1000]),
  io:format("bc:  ~p in ~p microseconds~n", [S1, T1]),
  io:format("pow: ~p in ~p microseconds~n", [S2, T2]),
  stop().

power_of_2(N) ->
  power_of_2(0, N).

power_of_2(N, N) ->
  bc(N, N);
power_of_2(K, N) when K >= 0, K =< N ->
  bc(N, K) + power_of_2(K+1, N).

start() ->
  Pid = spawn(?MODULE, bc_task, [#state{}]),
  register(?MODULE, Pid),
  Pid.

stop() ->
  ?MODULE ! {exit, self()},
  receive
    ok ->
      true;
    _ ->
      false
  end.

sum_digits_power_of_2(N) ->
  lists:sum(explode(power_of_2(N))).

sum_pow(X, Y) ->
  lists:sum(explode(trunc(math:pow(X, Y)))).

%%
%% unit tests
%%
bc_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
    [
      ?_assertEqual(1, bc(0, 0)),
      ?_assertEqual(1, bc(1, 0)),
      ?_assertEqual(1, bc(1, 1)),
      ?_assertEqual(2, bc(2, 1)),
      ?_assertEqual(56, bc(8, 3))
    ]
  }.

bc_task_test_() ->
  {setup,
   fun() ->
       start(),
       add(0, 0, 1),
       add(1, 0, 1)
   end,
   fun(_) ->
       stop()
   end,
   [
    ?_assertEqual(1, get(0, 0)),
    ?_assertEqual(1, get(1, 0)),
    ?_assertEqual(not_found, get(2, 2))
   ]
  }.

explode_test_() ->
  [
   ?_assertEqual([0], explode(0)),
   ?_assertEqual([1,2,3], explode(123)),
   ?_assertEqual([1,2,3,3,2,1], explode(123321))
  ].

power_of_2_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(1, power_of_2(0)),
    ?_assertEqual(2, power_of_2(1)),
    ?_assertEqual(32, power_of_2(5)),
    ?_assertEqual(1024, power_of_2(10))
   ]
  }.

sum_digits_power_of_2_test_() ->
  {setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(5, sum_digits_power_of_2(5)),
    ?_assertEqual(7, sum_digits_power_of_2(10)),
    ?_assertEqual(26, sum_digits_power_of_2(15))
   ]
  }.
