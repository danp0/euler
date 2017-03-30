-module(permutations).

-export(
   [
    is_equidistant/1,
    main/0,
    partition/1,
    sieve/2,
    to_digits/1,
    to_number/1,
    triplets/1
   ]
  ).

is_equidistant({P1, P2, P3}) ->
  (P2 - P1) =:= (P3 - P2).

main() ->
  Primes = sieve(1000, 9999),
  Permutations = partition(Primes),
  Triplets = triplets(Permutations),
  Equidistant = lists:filter(fun(T) -> is_equidistant(T) end, Triplets),
  io:format("~p~n", [Equidistant]),
  ok.

partition(L) ->
  Permutations = 
  lists:map(
    fun(N) ->
        Digits = to_digits(N),
        {lists:sort(Digits), Digits}
    end,
    L),
  Sorted = lists:sort(Permutations),
  Partitions = partition(tl(Sorted), [hd(Sorted)], []),
  [lists:map(fun({_Order,Prime}) -> to_number(Prime) end, Partition) || Partition <- Partitions].

partition([], [], Acc) ->
  lists:map(fun(L) -> lists:reverse(L) end, lists:reverse(Acc));
partition([], Permutations, Acc) ->
  partition([], [], [Permutations | Acc]);
partition([{Order1,_Prime1}=H1|T1], [{Order2,_Prime2}=H2|T2], Acc) ->
  case Order1 =:= Order2 of
    true ->
      partition(T1, [H1, H2 | T2], Acc);
    false ->
      partition(T1, [H1], [[H2|T2] | Acc])
  end.

sieve(From, To) ->
  sieve(From, lists:seq(2,To), []).

sieve(_From, [], Primes) ->
  lists:reverse(Primes);
sieve(From, [H|T], Primes) ->
  Next =
  case H >= From of
    true ->
      [H|Primes];
    false ->
      Primes
  end,
  sieve(From, [N || N <- T, N rem H =/= 0], Next).

to_digits(0) ->
  [0];
to_digits(N) ->
  to_digits(N, []).

to_digits(0, Digits) ->
  Digits;
to_digits(N, Digits) ->
  to_digits(N div 10, [N rem 10 | Digits]).

to_number(Digits) ->
  to_number(Digits, 0).

to_number([], Number) ->
  Number;
to_number([D|T], Number) ->
  to_number(T, 10 * Number + D).

triplets(L) ->
  triplets(L, []).

triplets([], Acc) ->
  lists:reverse(Acc);
triplets([H|T], Acc) when length(H) < 3 ->
  triplets(T, Acc);
triplets([H|T], Acc) when length(H) =:= 3 ->
  [P1,P2,P3] = H,
  triplets(T, [{P1,P2,P3} | Acc]);
triplets([H|T], Acc) ->
  Triplets = [{P1, P2, P3} || P1 <- H, P2 <- H -- [P1], P3 <- H -- [P1, P2], P1 < P2, P2 < P3],
  triplets(T, lists:append(lists:reverse(Triplets), Acc)).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_equidistant_test_() ->
  [
   ?_assert(is_equidistant({1000, 2000, 3000})),
   ?_assertNot(is_equidistant({1000, 1100, 2000}))
  ].

sieve_test_() ->
  [
   ?_assertEqual([11,13,17,19], sieve(10, 20))
  ].

to_digits_test_() ->
  [
   ?_assertEqual([0], to_digits(0)),
   ?_assertEqual([1], to_digits(1)),
   ?_assertEqual([1,2,3], to_digits(123))
  ].

to_number_test_() ->
  [
   ?_assertEqual(0, to_number([0])),
   ?_assertEqual(1, to_number([1])),
   ?_assertEqual(123, to_number([1,2,3]))
  ].

triplets_test_() ->
  [
   ?_assertEqual([], triplets([[1], [2], [3]])),
   ?_assertEqual([{1,2,3}, {4,5,6}], triplets([[1,2,3],[4,5,6]])),
   ?_assertEqual([{1,2,3}, {1,2,4}, {1,2,5}, {1,3,4}, {1,3,5},
                  {1,4,5}, {2,3,4}, {2,3,5}, {2,4,5}, {3,4,5}, 
                  {7,8,9}], 
                 triplets([[1,2,3,4,5],[6],[7,8,9]]))
  ].

-endif.
