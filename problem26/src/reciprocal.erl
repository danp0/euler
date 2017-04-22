-module(reciprocal).

-export(
   [
    find/1,
    find_cycles/2,
    main/0,
    match/1,
    shift/2,
    squeeze/1,
    to_string/1,
    update_cycle/2
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% For the fractions 1/d where d < 1000, find the value d that
% contains the longest recurring cycle.
%

extend(A, _B, Count, Acc) when A =:= 0 orelse Count =:= 0 ->
  lists:reverse(Acc);
extend(A, B, Count, Acc) when A < B ->
  extend(shift(A,B), B, Count, Acc);
extend(A, B, Count, Acc) ->
  extend(A rem B, B, Count - 1, [A div B | Acc]).

find_cycles(From, To) ->
  Reciprocals = [{N, find(N)} || N <- lists:seq(From, To)],
  WithCycles = lists:filter(
                 fun({_N, {true, _Q, _C}}) ->
                     true;
                    (_) ->
                     false
                 end,
                 Reciprocals),
  lists:sort(
    fun({_N1, {true, _Q1, C1}}, {_N2, {true, _Q2, C2}}) ->
        length(C1) >= length(C2)
    end,
    WithCycles).

find(N) when N >= 2 ->
  find(1, N, dict:new(), []).

find(0, _N, _Cycles, Acc) ->
  {false, lists:reverse(Acc)};
find(A, B, Cycles, Acc) when A < B ->
  {Count, NewCycles} = update_cycle(A, Cycles),
  if
    Count =:= 2 ->
      Extension = extend(A, B, length(Acc), Acc),
      Cycle = match(Extension),
      {true, to_string(Extension), squeeze(Cycle)};
    true ->
      find(shift(A,B), B, NewCycles, Acc)
  end;
find(A, B, Cycles, Acc) ->
  find(A rem B, B, Cycles, [A div B | Acc]).

main() ->
  io:format("reciprocal...~n", []),
  Reciprocals = find_cycles(2,999),
  lists:foreach(
    fun
      ({N, {true, Q, C}}) ->
        io:format("~p:~n~p~n~p~n~p~n", [N, Q, C, length(C)]);
      (_) ->
        ok
    end,
    Reciprocals),
  ok.

match(A) ->
  REs =
  [
    "^(\\d+)\\1$",
    "^(\\d+)\\1\\d$",
    "^(\\d+)\\1\\d\\d$",
    "^\\d+(\\d+)\\1$"
  ],
  S = to_string(A),
  Matches = lists:filtermap(
              fun(RE) ->
                  case re:run(S, RE, [{capture, all_but_first, list}]) of
                    {match, [Cycle]} ->
                      {true, Cycle};
                    nomatch ->
                      false
                  end
              end,
              REs),
  case Matches of
    [] ->
      [];
    [H | _T] ->
      H
  end.

shift(A, B) when A >= B ->
  A;
shift(A, B) ->
  shift(10*A, B).

squeeze([]) ->
  [];
squeeze([H|T]) ->
  case lists:all(fun(E) -> H =:= E end, T) of
    true ->
      [H];
    false ->
      [H|T]
  end.

to_string(L) ->
  lists:flatten(lists:map(fun(E) -> integer_to_list(E) end, L)).

update_cycle(N, Dict) ->
  NewDict = dict:update_counter(N, 1, Dict),
  {dict:fetch(N, NewDict), NewDict}.

%%
%% unit tests
%%
shift_test_() ->
  [
   ?_assertEqual(10, shift(1,3)),
   ?_assertEqual(10, shift(1,10)),
   ?_assertEqual(100, shift(1,11))
  ].

squeeze_test_() ->
  [
   ?_assertEqual([1], squeeze([1])),
   ?_assertEqual([1], squeeze([1,1])),
   ?_assertEqual([1,2], squeeze([1,2]))
  ].

to_string_test_() ->
  [
   ?_assertEqual("1", to_string([1])),
   ?_assertEqual("12", to_string([1,2])),
   ?_assertEqual("123", to_string([1,2,3])),
   ?_assertEqual("1234", to_string([1,2,3,4])),
   ?_assertEqual("12345", to_string([1,2,3,4,5]))
  ].

update_cycle_test_() ->
  {
   setup,
   fun() ->
       lists:foldl(
         fun(N, Dict) ->
             {_, NewDict} = update_cycle(N, Dict),
             NewDict
         end,
         dict:new(),
         [1,2,2,3,3,3])
   end,
   fun(_) -> ok end,
   fun(Dict) ->
       [
        ?_assertEqual(1, dict:fetch(1, Dict)),
        ?_assertEqual(2, dict:fetch(2, Dict)),
        ?_assertEqual(3, dict:fetch(3, Dict))
       ]
   end
  }.
