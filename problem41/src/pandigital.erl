-module(pandigital).

-export(
   [
    generate/1,
    is_prime/1,
    list_to_int/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

generate([D]) ->
  [[D]];
generate(Digits) ->
  [[D | Tail] || D <- Digits, Tail <- generate(Digits -- [D])].

is_prime(N) when N < 2 ->
  false;
is_prime(2) ->
  true;
is_prime(3) ->
  true;
is_prime(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(N, 5, trunc(math:sqrt(N)) + 1).

is_prime(_N, I, To) when I > To ->
  true;
is_prime(N, I, _To) when N rem I =:= 0 orelse N rem (I + 2) =:= 0 ->
  false;
is_prime(N, I, To) ->
  is_prime(N, I+6, To).

list_to_int(L) ->
  list_to_int(L, 0).

list_to_int([], I) ->
  I;
list_to_int([H|T], I) ->
  list_to_int(T, 10*I + H).

main() ->
  io:format("pandigital prime...~n", []),
  lists:foreach(
    fun(N) ->
      Pds = lists:map(fun(D) -> list_to_int(D) end, generate(lists:seq(N,1,-1))),
      Primes = lists:filter(fun(P) -> is_prime(P) end, Pds),
      if
        length(Primes) =:= 0 ->
          skip;
        true ->
          io:format("~p: ~p~n", [N, hd(Primes)])
      end
    end,
    lists:seq(9,1,-1)),
  ok.

%%
%% unit test
%%
is_prime_test_() ->
  [
   ?_assertNot(is_prime(1)),
   ?_assert(is_prime(2)),
   ?_assert(is_prime(3)),
   ?_assertNot(is_prime(4)),
   ?_assert(is_prime(5)),
   ?_assertNot(is_prime(6)),
   ?_assert(is_prime(7)),
   ?_assertNot(is_prime(8)),
   ?_assertNot(is_prime(9)),
   ?_assert(is_prime(19))
  ].

list_to_int_test_() ->
  [
   ?_assertEqual(0, list_to_int([0])),
   ?_assertEqual(1, list_to_int([1])),
   ?_assertEqual(123, list_to_int([1,2,3]))
  ].
