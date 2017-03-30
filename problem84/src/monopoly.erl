-module(monopoly).

-export(
   [
    get_option/3,
    get_visited/0,
    init/1,
    main/0,
    move/0,
    new_game/0,
    next/2,
    next/3,
    next_from/2,
    roll/1,
    run/3,
    seed/0,
    shuffle/1,
    start/1,
    stop/0,
    trace/3
   ]
  ).

-record(
   state,
   {
    trace,
    board_length,
    from,
    current,
    action,
    visited,
    sides_dice,
    top_community_chest,
    community_chest,
    top_chance,
    chance,
    doubles,
    jail
   }
  ).

get_option(Option, Options, Default) ->
  case lists:keyfind(Option, 1, Options) of
    false ->
      Default;
    {Option, Value} ->
      Value
  end.

get_visited() ->
  ?MODULE ! {get_visited, self()},
  receive
    Result ->
      Result
  end.

init(Options) ->
  seed(),
  Board =
  [go,   a1, cc1,  a2, t1, r1, b1, ch1, b2, b3, 
   jail, c1,  u1,  c2, c3, r2, d1, cc2, d2, d3,
   fp,   e1, ch2,  e2, e3, r3, f1,  f2, u2, f3,
   g2j,  g1,  g2, cc3, g3, r4, ch3, h1, t2, h2],
  Order = lists:sort(lists:zip(Board, lists:seq(1, length(Board)))),
  Mapping = gb_trees:from_orddict(Order),
  Go = gb_trees:get(go, Mapping),
  Jail = gb_trees:get(jail, Mapping),
  C1 = gb_trees:get(c1, Mapping),
  E3 = gb_trees:get(e3, Mapping),
  H2 = gb_trees:get(h2, Mapping),
  R1 = gb_trees:get(r1, Mapping),
  R2 = gb_trees:get(r2, Mapping),
  R3 = gb_trees:get(r3, Mapping),
  R4 = gb_trees:get(r4, Mapping),
  U1 = gb_trees:get(u1, Mapping),
  U2 = gb_trees:get(u2, Mapping),
  Action =
  lists:map(
    fun
      (g2j) ->
        {goto, Jail};
      (Square) when Square =:= cc1; Square =:= cc2; Square =:= cc3 ->
        {draw, community_chest};
      (Square) when Square =:= ch1; Square =:= ch2; Square =:= ch3 ->
        {draw, chance};
      (_Square) ->
        visit
    end,
    Board),
  CommunityChest =
  [
   {goto, Go}, 
   {goto, Jail} | 
   lists:duplicate(14, visit)],
  CommunityChest2 =
  case get_option(shuffle_community_chest, Options, true) of
    true ->
      shuffle(CommunityChest);
    false ->
      CommunityChest
  end,
  Chance =
  [{goto, Go}, 
   {goto, Jail}, 
   {goto, C1}, 
   {goto, E3}, 
   {goto, H2}, 
   {goto, R1}, 
   {next, [R1, R2, R3, R4]}, 
   {next, [R1, R2, R3, R4]}, 
   {next, [U1, U2]}, 
   {back, 3} |
   lists:duplicate(6, visit)],
  Chance2 =
  case get_option(shuffle_chance, Options, true) of
    true ->
      shuffle(Chance);
    false ->
      Chance
  end,
  loop(
    #state{
       trace = get_option(trace, Options, false),
       board_length = length(Board),
       from = none,
       current = 1,
       action = Action,
       visited = 
       gb_trees:from_orddict(
         lists:zip(
           lists:seq(1, length(Board)), 
           lists:duplicate(length(Board), 0))),
       sides_dice = get_option(sides_dice, Options, 6),
       top_community_chest = 1,
       community_chest = CommunityChest2,
       top_chance = 1,
       chance = Chance2,
       doubles = 0,
       jail = Jail
      }
   ).

loop(State) ->
  NewState =
  receive
    {back, N} ->
      trace(State#state.trace, "back: ~p~n", [N]),
      self() ! {goto, next(State#state.current, -N, State#state.board_length)},
      State;
    {draw, What} ->
      trace(State#state.trace, "draw: ~p~n", [What]),
      case What of
        community_chest ->
          self() ! lists:nth(State#state.top_community_chest, State#state.community_chest),
          State#state{top_community_chest = next(State#state.top_community_chest, length(State#state.community_chest))};
        chance ->
          self() ! lists:nth(State#state.top_chance, State#state.chance),
          State#state{top_chance = next(State#state.top_chance, length(State#state.chance))}
      end;
    {get_visited, From} ->
      From ! State#state.visited,
      State;
    {goto, Location} ->
      trace(State#state.trace, "goto: ~p~n", [Location]),
      self() ! lists:nth(Location, State#state.action),
      State#state{current = Location};
    {move, From} ->
      {N1, N2} = roll(State#state.sides_dice),
      trace(State#state.trace, "move: ~p~n", [{N1, N2}]),
      Doubles =
      case N1 =:= N2 of
        true ->
          State#state.doubles + 1;
        false ->
          0
      end,
      case Doubles =:= 3 of
        true ->
          self() ! {goto, State#state.jail},
          State#state{from = From, doubles = 0};
        false ->
          self() ! {goto, next(State#state.current, N1 + N2, State#state.board_length)},
          State#state{from = From, doubles = Doubles}
      end;
    {new_game, From} ->
      From ! ok,
      State#state{
        current = 1, 
        top_community_chest = 1, 
        community_chest = shuffle(State#state.community_chest),
        top_chance = 1,
        chance = shuffle(State#state.chance)};
    {next, Locations} ->
      trace(State#state.trace, "next: ~p~n", [Locations]),
      self() ! {goto, next_from(State#state.current, Locations)},
      State;
    visit ->
      trace(State#state.trace, "visit~n", []),
      Count = gb_trees:get(State#state.current, State#state.visited),
      Visited = gb_trees:update(State#state.current, Count + 1, State#state.visited),
      State#state.from ! ok,
      State#state{from = none, visited = Visited};
    {stop, From} ->
      trace(State#state.trace, "stop~n", []),
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  Games = 1000,
  Iterations = 10000,
  SidesDice = 4,
  Visited = run(Games, Iterations, SidesDice),
  Sorted =
  lists:sort(
    fun({_Square1, Visits1}, {_Square2, Visits2}) ->
        Visits1 >= Visits2
    end,
    gb_trees:to_list(Visited)),
  Top5Visits = lists:sublist(Sorted, 5),
  Top5 = 
  lists:map(
    fun({Square, _Visits}) ->
        Square
    end,
    Top5Visits),
  Top5Percentages =
  lists:map(
    fun({Square, Visits}) ->
        {Square, (Visits * 100) / (Games * Iterations)}
    end,
    Top5Visits),
  io:format("~p~n", [Top5Visits]),
  io:format("~p~n", [Top5Percentages]),
  [T1, T2, T3 | _Tail] = Top5,
  io:format("~2..0B~2..0B~2..0B~n", [T1-1, T2-1, T3-1]),
  ok.

move() ->
  ?MODULE ! {move, self()},
  receive
    Result ->
      Result
  end.

new_game() ->
  ?MODULE ! {new_game, self()},
  receive
    Result ->
      Result
  end.

next(N, Maximum) ->
  next(N, 1, Maximum).

next(N, Increment, Maximum) when Increment < 0 ->
  Next = N + Increment,
  case Next =< 0 of
    true ->
      Maximum + Next;
    false ->
      Next
  end;
next(N, Increment, Maximum) ->
  (N + Increment - 1) rem Maximum + 1.

next_from(N, L) ->
  {Before, After} = lists:splitwith(fun(E) -> E =< N  end, L),
  if 
    After =:= [] ->
      hd(Before);
    true ->
      hd(After)
  end.

roll(Sides) ->
  {random:uniform(Sides), random:uniform(Sides)}.

run(Games, Iterations, SidesDice) ->
  start(
    [
     {sides_dice, SidesDice}, 
     {shuffle_community_chest, true}, 
     {shuffle_chance, true},
     {trace, false}
    ]
   ),
  lists:foreach(
    fun(_) ->
        new_game(),
        run_loop(Iterations)
    end,
    lists:seq(1, Games)),
  Visited = get_visited(),
  ok = stop(),
  Visited.

run_loop(0) ->
  done;
run_loop(Iterations) ->
  ok = move(),
  run_loop(Iterations - 1).

seed() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3).

shuffle(L) ->
  lists:map(
    fun({_Index, E}) ->
        E
    end,
    lists:sort([{random:uniform(length(L)), E} || E <- L])).

start(SidesDice) ->
  register(?MODULE, spawn(?MODULE, init, [SidesDice])).

stop() ->
  ?MODULE ! {stop, self()},
  receive
    Result ->
      Result
  end.

trace(false, _Format, _Parameters) ->
  ok;
trace(true, Format, Parameters) ->
  io:format(Format, Parameters).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_option_test_() ->
  Options = [{option1, 1}, {option2, 2}, {option3, 3}],
  [
   ?_assertEqual(1, get_option(option1, Options, 10)),
   ?_assertEqual(2, get_option(option2, Options, 10)),
   ?_assertEqual(3, get_option(option3, Options, 10)),
   ?_assertEqual(10, get_option(option4, Options, 10)),
   ?_assertEqual(10, get_option(option5, Options, 10))
  ].

next_test_() ->
  [
   ?_assertEqual(1, next(6, 6)),
   ?_assertEqual(2, next(1, 6)),
   ?_assertEqual(3, next(2, 6)),
   ?_assertEqual(4, next(3, 6)),
   ?_assertEqual(5, next(4, 6)),
   ?_assertEqual(6, next(5, 6)),
   ?_assertEqual(3, next(1, 2, 40)),
   ?_assertEqual(1, next(39, 2, 40)),
   ?_assertEqual(2, next(40, 2, 40)),
   ?_assertEqual(40, next(39, 1, 40)),
   ?_assertEqual(36, next(39, -3, 40)),
   ?_assertEqual(38, next(1, -3, 40))
  ].

next_from_test_() ->
  [
   ?_assertEqual(10, next_from(1,  [10, 20, 30, 40])),
   ?_assertEqual(20, next_from(11, [10, 20, 30, 40])),
   ?_assertEqual(30, next_from(21, [10, 20, 30, 40])),
   ?_assertEqual(40, next_from(31, [10, 20, 30, 40])),
   ?_assertEqual(10, next_from(41, [10, 20, 30, 40]))
  ].

roll_test_() ->
  {
   setup,
   fun() ->
       seed()
   end,
   fun(_) ->
       true
   end,
   fun() ->
       {D1, D2} = roll(6),
       [
        ?_assert(D1 >= 1 andalso D1 =< 6),
        ?_assert(D2 >= 1 andalso D2 =< 6)
       ]
   end
}.

shuffle_test_() ->
  {
   setup,
   fun() -> 
       seed()
   end,
   fun(_) ->
       true
   end,
   [
    ?_assertEqual([1,2,3,4,5], lists:sort(shuffle([1,2,3,4,5])))
   ]
  }.

-endif.
