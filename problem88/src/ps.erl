-module(ps).

-export(
   [
    factor/1,
    factors/1,
    find_minimal/1,
    main/0,
    sequence/1,
    sequence_length/1
   ]
  ).

factor(N) ->
  lists:usort(
    lists:map(
      fun(L) ->
          lists:sort(L)
      end,
      factors(N))).

factors(N) ->
  Factors =
  factors(2, trunc(math:sqrt(N)), N, []),
  [lists:flatten([A | [C]]) || [A, B] <- Factors, C <- [B | factors(B)]].

factors(I, SqrtN, _N, Acc) when I > SqrtN ->
  lists:reverse(Acc);
factors(I, SqrtN, N, Acc) when N rem I =:= 0 ->
  factors(I+1, SqrtN, N, [[I, N div I] | Acc]);
factors(I, SqrtN, N, Acc) ->
  factors(I+1, SqrtN, N, Acc).

find_minimal(Maximum) ->
  lists:foldl(
    fun(N, Mapping) ->
        find_minimal(Maximum, sequence(N), N, Mapping)
    end,
    gb_trees:empty(),
    lists:seq(4, 2*Maximum)).

find_minimal(_Maximum, [], _N, Mapping) ->
  Mapping;
find_minimal(Maximum, [H | T], N, Mapping) ->
  Length = sequence_length(H),
  Minimum =
  case gb_trees:lookup(Length, Mapping) of
    none ->
      N;
    {value, Value} ->
      case N < Value of
        true ->
          N;
        false ->
          Value
      end
  end,
  case Length =< Maximum of
    true ->
      find_minimal(Maximum, T, N, gb_trees:enter(Length, Minimum, Mapping));
    false ->
      find_minimal(Maximum, T, N, Mapping)
  end.

main() ->
  Minimal = find_minimal(12000),
  {K, N} = lists:unzip(gb_trees:to_list(Minimal)),
  io:format("~p~n~p~n", [K, N]),
  io:format("~p~n", [lists:sum(lists:usort(N))]),
  ok.

sequence(N) ->
  sequence(N, factor(N), []).

sequence(_N, [], Acc) ->
  lists:reverse(Acc);
sequence(N, [H|T], Acc) ->
  Sum = lists:sum(H),
  Difference = N - Sum,
  sequence(N, T, 
    [
      if
        Difference =:= 0 ->
          H;
        true ->
          [{1, N - Sum} | H]
      end | Acc
    ]).

sequence_length(Sequence) ->
  lists:foldl(
    fun
    ({_N, P}, Sum) ->
        Sum + P;
    (_N, Sum) ->
        Sum + 1
    end,
    0,
    Sequence).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

factor_test_() ->
  [
   ?_assertEqual([], factor(2)),
   ?_assertEqual([], factor(3)),
   ?_assertEqual([[2,2]], factor(4)),
   ?_assertEqual([], factor(5)),
   ?_assertEqual([[2,3]], factor(6)),
   ?_assertEqual([], factor(7)),
   ?_assertEqual([[2,2,2], [2,4]], factor(8))
  ].

find_minimal_test_() ->
  Minimal = find_minimal(6),
  {_K, N} = lists:unzip(gb_trees:to_list(Minimal)),
  [
   ?_assertEqual(
      [{2, 4}, {3, 6}, {4, 8}, {5, 8}, {6, 12}], 
      gb_trees:to_list(Minimal)),
   ?_assertEqual(30, lists:sum(lists:usort(N)))
  ].

sequence_test_() ->
  [
   ?_assertEqual([[2, 2]], sequence(4)),
   ?_assertEqual([], sequence(5)),
   ?_assertEqual([[{1,1}, 2, 3]], sequence(6)),
   ?_assertEqual([[{1,2}, 2, 2, 2], [{1,2}, 2, 4]], sequence(8)),
   ?_assertEqual([[{1,5}, 2, 2, 3], [{1,4}, 2, 6], [{1,5}, 3, 4]], sequence(12))
  ].

sequence_length_test_() ->
  [
   ?_assertEqual(0, sequence_length([])),
   ?_assertEqual(2, sequence_length([2,2])),
   ?_assertEqual(3, sequence_length([{1,1}, 2, 3])),
   ?_assertEqual(4, sequence_length([{1,2}, 2, 4])),
   ?_assertEqual(5, sequence_length([{1,2}, 2, 2, 2]))
  ].

-endif.

