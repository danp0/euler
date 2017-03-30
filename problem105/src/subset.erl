-module(subset).

-export(
   [
    main/0,
    optimum/1,
    subsets/1,
    read/1
   ]
  ).

-define(SUBSETS, "p105_sets.txt").

main() ->
  io:format("subset...~n", []),
  Subsets = read(?SUBSETS),
  Filtered = lists:filter(fun optimum/1, Subsets),
  Sum = lists:sum(lists:map(fun lists:sum/1, Filtered)),
  io:format("~w~n", [Filtered]),
  io:format("sum: ~p~n", [Sum]),
  ok.

optimum(S) ->
  Sums = [{lists:sum(E), length(E)} || E <- subsets(S)],
  USort =
  lists:usort(
    fun({Sum1, _}, {Sum2, _}) ->
        Sum1 =< Sum2
    end,
    Sums),
  Sort1 =
  lists:sort(
    fun({Sum1, _}, {Sum2, _}) ->
        Sum1 =< Sum2
    end,
    Sums),
  Sort2 =
  lists:sort(
    fun({Sum1, Length1}, {Sum2, Length2}) ->
        case Length1 =:= Length2 of
          true ->
            Sum1 =< Sum2;
          false ->
            Length1 =< Length2
        end
    end,
    Sums),
  length(Sums) =:= length(USort)
  andalso
  Sort1 =:= Sort2.

read(Subsets) ->
  {ok, Binary} = file:read_file(Subsets),
  Lines =
  re:split(Binary, "\n", [{return, list}]),
  lists:map(
    fun(Line) ->
        lists:sort(
          [list_to_integer(E) ||
           E <- re:split(Line, ",", [{return, list}])])
    end,
    Lines).

subsets_unsorted(S) ->
  N = trunc(math:pow(2, length(S))),
  Format = lists:flatten(io_lib:format("~~~b.2.0b", [length(S)])), 
  [
   lists:filtermap(
     fun({E, Bit}) ->
         case [Bit] =:= "1" of
           true ->
             {true, E};
           false ->
             false
         end
     end,
     lists:zip(S, lists:flatten(io_lib:format(Format, [I])))) 
   || 
   I <- lists:seq(1, N-1)
  ].

subsets(S) ->
  Subsets = subsets_unsorted(S),
  lists:sort(
    fun(S1, S2) ->
        if
          length(S1) =:= length(S2) ->
            S1 =< S2;
          true ->
            length(S1) =< length(S2)
        end
    end,
    Subsets).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

optimum_test_() ->
  [
   ?_assert(optimum([1])),
   ?_assert(optimum([1,2])),
   ?_assert(optimum([2,3,4])),
   ?_assert(optimum([3,5,6,7])),
   ?_assert(optimum([6,9,11,12,13])),
   ?_assert(optimum([11,17,20,22,23,24])),
   ?_assert(optimum([11,18,19,20,22,25])),
   ?_assertNot(optimum([1,2,3])),
   ?_assertNot(optimum([3,5,6,9])),
   ?_assertNot(optimum([3,5,6,8]))
  ].

subsets_test_() ->
  [
   ?_assertEqual([], subsets([])),
   ?_assertEqual([[1]], subsets([1])),
   ?_assertEqual([[1], [2], [1,2]], subsets([1,2])),
   ?_assertEqual([[1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]], 
                 subsets([1,2,3])),
   ?_assertEqual(31, length(subsets([1,2,3,4,5])))
  ].

-endif.
