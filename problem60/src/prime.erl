-module(prime).

-export(
   [
    find_prime_pair_sets/1,
    is_prime/1,
    is_prime_pair/2,
    is_prime_pair_set/1,
    main/0,
    pairs_with/2
   ]
  ).

find_prime_pair_sets(Primes) ->
  find_prime_pair_sets([{[P], pairs_with(P, Primes)}||P <- Primes], []).

find_prime_pair_sets([], Acc) ->
  lists:usort(
    fun(E1, E2) -> 
        LE1 = length(E1),
        LE2 = length(E2),
        if
          LE1 =:= LE2 ->
            E1 =< E2;
          true ->
            LE1 =< LE2
        end
    end,
    lists:map(fun(L) -> lists:sort(L) end, 
              lists:filter(fun(L) -> length(L) >= 5 end, Acc)));
find_prime_pair_sets([{Seeds, []}|T], Acc) ->
  find_prime_pair_sets(T, [Seeds|Acc]);
find_prime_pair_sets([{Seeds, [E]}|T], Acc) ->
  find_prime_pair_sets(T, [[E|Seeds]|Acc]);
find_prime_pair_sets([{Seeds, PairsWith}|T], Acc) ->
  find_prime_pair_sets(
    lists:append(
      T,
      [{[P|Seeds], pairs_with(P, PairsWith)} || P <- PairsWith]), Acc).

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I * I =< N ->
  case N rem I =:= 0 orelse N rem (I+2) =:= 0 of
    true ->
      false;
    false ->
      is_prime(I+6, N)
  end;
is_prime(_I, _N) ->
  true.

is_prime_pair(M, N) ->
  MStr = integer_to_list(M),
  NStr = integer_to_list(N),
  is_prime(list_to_integer(lists:append(MStr, NStr))) andalso
  is_prime(list_to_integer(lists:append(NStr, MStr))).

is_prime_pair_set([]) ->
  false;
is_prime_pair_set([_]) ->
  false;
is_prime_pair_set([M, N]) ->
  is_prime_pair(M,N);
is_prime_pair_set([H| T]) ->
  lists:all(
    fun({M,N}) ->
        is_prime_pair(M,N)
    end,
    [{H,I} || I <- T])
  andalso is_prime_pair_set(T).

main() ->
  Primes = lists:filter(fun(N) -> is_prime(N) end, lists:seq(2,9999)),
  PrimePairSets = find_prime_pair_sets(Primes),
  io:format("~p~n", [PrimePairSets]),
  io:format("~p~n", [lists:map(fun(L) -> is_prime_pair_set(L) end, PrimePairSets)]),
  io:format("~p~n", [lists:map(fun(L) -> lists:sum(L) end, PrimePairSets)]),
  ok.

pairs_with(N, Primes) ->
  lists:filter(
    fun(P) ->
        is_prime_pair(N, P)
    end,
    Primes).

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_pair_set_test_() ->
  [
   ?_assert(is_prime_pair_set([3,7,109,673])),
   ?_assertNot(is_prime_pair_set([2,3,5,7]))
  ].

is_prime_test_() ->
  [
   ?_assertNot(is_prime(0)),
   ?_assert(is_prime(2)),
   ?_assert(is_prime(3)),
   ?_assertNot(is_prime(4)),
   ?_assert(is_prime(5)),
   ?_assertNot(is_prime(6)),
   ?_assert(is_prime(7)),
   ?_assertNot(is_prime(8)),
   ?_assertNot(is_prime(9)),
   ?_assertNot(is_prime(10)),
   ?_assert(is_prime(11)),
   ?_assertNot(is_prime(12)),
   ?_assert(is_prime(13)),
   ?_assertNot(is_prime(14)),
   ?_assertNot(is_prime(15)),
   ?_assertNot(is_prime(16)),
   ?_assert(is_prime(17)),
   ?_assertNot(is_prime(18)),
   ?_assert(is_prime(19)),
   ?_assertNot(is_prime(20))
  ].

is_prime_pair_test_() ->
  [
   ?_assert(is_prime_pair(3,7)),
   ?_assert(is_prime_pair(3,109)),
   ?_assert(is_prime_pair(7,109)),
   ?_assert(is_prime_pair(109,673)),
   ?_assertNot(is_prime_pair(10,20))
  ].

-endif.
