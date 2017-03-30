-module(replacements).

-export(
   [
    find_when/1,
    init/0,
    is_prime/1,
    main/0,
    mark/1,
    patterns/1,
    prime_sequence/1,
    repeating/1,
    replace/3,
    sequence/1,
    start/0,
    stop/0,
    visited/1
   ]
  ).

-record(
   state,
   {
    visited = gb_sets:empty()
   }
  ).

-define(Wildcard, $#).

find_when(Fun) ->
  find_when(101, Fun).

find_when(N, Fun) ->
  Found =
  case is_prime(N) of
    true ->
      find_prime_sequence_when(patterns(integer_to_list(N)), Fun);
    false ->
      false
  end,
  case Found of
    false ->
      find_when(N+2, Fun);
    {true, Seq} ->
      Seq
  end.

find_prime_sequence_when([], _Fun) ->
  false;
find_prime_sequence_when([P|T], Fun) ->
  case visited(P) of
    true ->
      find_prime_sequence_when(T, Fun);
    false ->
      mark(P),
      Seq = prime_sequence(P),
      case Fun(Seq) of
        true ->
          {true, Seq};
        false ->
          find_prime_sequence_when(T, Fun)
      end
  end.

init() ->
  loop(#state{}).

is_prime(N) when is_list(N) ->
  {I,""} = string:to_integer(N),
  is_prime(I);
is_prime(N) when N < 2 ->
  false;
is_prime(2) ->
  true;
is_prime(3) ->
  true;
is_prime(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5,N).

is_prime(I,N) when I*I > N ->
  true;
is_prime(I,N) when N rem I =:= 0 orelse N rem (I+2) =:= 0 ->
  false;
is_prime(I,N) ->
  is_prime(I+6,N).

loop(State) ->
  NewState =
  receive
    {mark, Pattern} ->
      Visited = gb_sets:add_element(Pattern, State#state.visited),
      State#state{visited=Visited};
    {stop, From} ->
      From ! ok,
      exit(normal),
      State;
    {visited, Pattern, From} ->
      From ! gb_sets:is_element(Pattern, State#state.visited),
      State
  end,
  loop(NewState).

main() ->
  start(),
  io:format("~p~n", [find_when(fun(Seq) -> length(Seq) >= 8 end)]),
  ok = stop(),
  ok.

mark(Pattern) ->
  primes ! {mark, Pattern},
  ok.

patterns(NStr) ->
  patterns(NStr, repeating(NStr), []).

patterns(_NStr, [], Acc) ->
  lists:reverse(Acc);
patterns(NStr, [D|T], Acc) ->
  patterns(NStr, T, [replace(NStr, D, ?Wildcard) | Acc]).

prime_sequence(Pattern) ->
  lists:filter(
    fun
      ([$0|_T]) ->
        false;
      (S) ->
        is_prime(S)
    end,
    sequence(Pattern)).

repeating(NStr) ->
  [C|T] = lists:sort(NStr),
  repeating(T, {C,false}, []).

repeating([], empty, Acc) ->
  lists:sort(Acc);
repeating([], {C, Repeats}, Acc) ->
  Acc2 =
  case Repeats of
    true ->
      [C|Acc];
    false ->
      Acc
  end,
  repeating([], empty, Acc2);
repeating([C|T], {C,_}, Acc) ->
  repeating(T, {C,true}, Acc);
repeating([C1|T], {C,Repeats}, Acc) ->
  Acc2 =
  case Repeats of
    true ->
      [C|Acc];
    false ->
      Acc
  end,
  repeating(T, {C1,false}, Acc2).

replace(NStr, OldChr, NewChr) ->
  re:replace(NStr, [OldChr], [NewChr], [global, {return, list}]).

sequence(Pattern) ->
  lists:map(
    fun(N) ->
      replace(Pattern, ?Wildcard, N + $0)
    end,
    lists:seq(0,9)).
              
start() ->
  register(primes, spawn(?MODULE, init, [])).

stop() ->
  primes ! {stop, self()},
  receive
    Result ->
      Result
  end.

visited(Pattern) ->
  primes ! {visited, Pattern, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
  [
   ?_assert(is_prime(2)),
   ?_assert(is_prime("2")),
   ?_assert(is_prime(3)),
   ?_assert(is_prime("3")),
   ?_assertNot(is_prime(4)),
   ?_assertNot(is_prime("4")),
   ?_assert(is_prime(5)),
   ?_assert(is_prime("5")),
   ?_assertNot(is_prime(6)),
   ?_assertNot(is_prime("6")),
   ?_assert(is_prime(7)),
   ?_assert(is_prime("7")),
   ?_assertNot(is_prime(8)),
   ?_assertNot(is_prime("8")),
   ?_assertNot(is_prime(9)),
   ?_assertNot(is_prime("9"))
  ].

patterns_test_() ->
  [
   ?_assertEqual([], patterns("123")),
   ?_assertEqual(["##23"], patterns("1123")),
   ?_assertEqual(["##23#2", "11#31#"], patterns("112312"))
  ].

prime_sequence_test_() ->
  [
   ?_assertEqual(["13", "23", "43", "53", "73", "83"], 
                 prime_sequence(replace("03", $0, ?Wildcard)))
  ].

repeating_test_() ->
  [
   ?_assertEqual("", repeating("123")),
   ?_assertEqual("1", repeating("1231")),
   ?_assertEqual("12", repeating("12312")),
   ?_assertEqual("12", repeating("1123112"))
  ].

replace_test_() ->
  [
   ?_assertEqual("1212", replace("1#1#", $#, $2)),
   ?_assertEqual("1#1#", replace("1212", $2, $#)),
   ?_assertEqual("123", replace("123", $#, $8))
  ].

sequence_test_() ->
  [
   ?_assertEqual(
      ["120", "121", "122", "123", "124",
       "125", "126", "127", "128", "129"], 
      sequence(replace("123", $3, ?Wildcard)))
  ].

visited_test_() ->
  {setup,
   fun() ->
       start(),
       mark("12#"),
       mark("23#"),
       mark("45#")
   end,
   fun(_) ->
       stop()
   end,
   [
    ?_assert(visited("12#")),
    ?_assert(visited("23#")),
    ?_assert(visited("45#")),
    ?_assertNot(visited("#56")),
    ?_assertNot(visited("#2#"))
   ]
  }.

-endif.
