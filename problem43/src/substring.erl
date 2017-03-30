-module(substring).

-export(
   [
    adjacent/2,
    filter_divisible_by/2,
    find_adjacent/0,
    find_adjacent/2,
    form_sequence/1,
    generate/0,
    is_pandigital/1,
    list_to_num/1,
    main/0,
    num_to_list/1
   ]
  ).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.

adjacent({_A1, B1, C1}, [{A2, B2, _C2} | _T]) ->
  B1 =:= A2 andalso C1 =:= B2.

filter_divisible_by(Triples, N) ->
  lists:map(fun(T) -> [D1,D2,D3] = num_to_list(T), {D1,D2,D3} end,
    lists:filter(fun(T) -> T rem N =:= 0 end, Triples)).

find_adjacent() ->
  Triples = generate(),
  Adjacent =
  lists:foldl(
    fun(N, Adj) ->
        find_adjacent(filter_divisible_by(Triples, N), Adj)
    end,
    [[T] || T <- filter_divisible_by(Triples, 17)],
    [13, 11, 7, 5, 3, 2]),
  [lists:flatten(L) || L <- Adjacent].

find_adjacent(L1, L2) ->
  [[T1, T2] || T1 <- L1, T2 <- L2, adjacent(T1, T2)].

form_sequence(Adjacent) ->
  lists:map(
    fun(L) ->
        [{D1, D2, _D3}|_T] = L,
        Digits = [D1, D2 | [D3 || {_D1, _D2, D3} <- L]],
        case lists:seq(0,9) -- Digits of
          [Missing] ->
            [Missing | Digits];
          _ ->
            Digits
        end
    end,
    Adjacent).

generate() ->
  lists:seq(10,999).

is_pandigital(L) ->
  lists:sort(L) =:= lists:seq(0,9).

list_to_num(L) ->
  list_to_num(L, 0).

list_to_num([], N) ->
  N;
list_to_num([H|T], N) ->
  list_to_num(T, 10*N + H).

main() ->
  Adjacent = form_sequence(find_adjacent()),
  Pandigital = 
  lists:filter(
    fun(D) ->
        is_pandigital(D) 
    end, 
    Adjacent),
  io:format("~w~n", [Pandigital]),
  PandigitalNumbers = lists:map(fun(D) -> list_to_num(D) end, Pandigital),
  io:format("~p~n", [lists:sum(PandigitalNumbers)]),
  ok.

num_to_list(N) ->
  case lists:map(fun(D) -> D - $0 end, integer_to_list(N)) of
    [D1] -> [0,0,D1];
    [D1,D2] -> [0,D1,D2];
    D -> D
  end.

%%
%% unit tests
%%
-ifdef(TEST).

adjacent_test_() ->
  [
   ?_assert(adjacent({7,2,8}, [{2,8,9}])),
   ?_assertNot(adjacent({7,2,9}, [{2,8,9}]))
  ].

filter_divisible_by_test_() ->
  [
   ?_assertEqual([{1,0,0}], 
                 filter_divisible_by([100, 101], 2))
  ].

is_pandigital_test_() ->
  [
   ?_assert(is_pandigital([0,1,2,3,4,5,6,7,8,9])),
   ?_assert(is_pandigital([9,8,7,6,5,4,3,2,1,0])),
   ?_assertNot(is_pandigital([9,9,2,3,4,5,6,7,8,9]))
  ].

list_to_num_test_() ->
  [
   ?_assertEqual(0, list_to_num([0])),
   ?_assertEqual(1, list_to_num([1])),
   ?_assertEqual(123, list_to_num([1,2,3]))
  ].

num_to_list_test_() ->
  [
   ?_assertEqual([0,0,0], num_to_list(0)),
   ?_assertEqual([0,1,2], num_to_list(12)),
   ?_assertEqual([1,2,3], num_to_list(123))
  ].

-endif.

