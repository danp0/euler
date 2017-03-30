-module(optimum).

-export(
   [
    main/0,
    nth_optium/1,
    nth_optium_min/1,
    optimum/1,
    subsets/1,
    subsets/2
   ]
  ).

main() ->
  io:format("~p~n", [nth_optium_min(7)]),
  ok.

nth_optium(1) ->
  [1];
nth_optium(N) ->
  Opt = nth_optium(N-1),
  Nth = lists:nth((length(Opt) div 2) + 1, Opt),
  [Nth | lists:map(fun(E) -> E + Nth end, Opt)].

nth_optium_min(N) ->
  Opt = nth_optium(N),
  Elements = 
  lists:usort(
    lists:append([[E+1, E, E-1, E-2] || E <- Opt])),
  lists:sort(
    lists:map(
      fun(L) ->
          {lists:sum(L), L}
      end,
      lists:filter(
        fun(S) ->
            optimum(S)
        end,
        subsets(Elements, N)))).

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

subsets(S, N) ->
  Subsets = subsets_unsorted(S),
  lists:sort(
  lists:filter(
    fun(Subset) ->
        length(Subset) =:= N
    end,
    Subsets)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

nth_optium_test_() ->
  [
   ?_assertEqual([1], nth_optium(1)),
   ?_assertEqual([1,2], nth_optium(2)),
   ?_assertEqual([2,3,4], nth_optium(3)),
   ?_assertEqual([3,5,6,7], nth_optium(4)),
   ?_assertEqual([6,9,11,12,13], nth_optium(5))
  ].

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
   ?_assertEqual(31, length(subsets([1,2,3,4,5]))),
   ?_assertEqual([[1],[2],[3]], subsets([1,2,3], 1)),
   ?_assertEqual([[1,2],[1,3],[2,3]], subsets([1,2,3], 2)),
   ?_assertEqual([[1,2,3]], subsets([1,2,3], 3))
  ].

-endif.
