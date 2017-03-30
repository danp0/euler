-module(truncatable).

-export(
   [
    find_truncatable_primes/1,
    is_prime/1,
    is_truncatable_prime/1,
    main/0,
    truncate_left/1,
    truncate_right/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

find_truncatable_primes(While) ->
  find_truncatable_primes(While, 11, []).

find_truncatable_primes(While, N, Acc) ->
  case is_truncatable_prime(N) of
    true ->
      NewAcc = [N | Acc],
      case While(NewAcc) of
        true ->
          find_truncatable_primes(While, N+2, NewAcc);
        false ->
          lists:reverse(NewAcc)
      end;
    false ->
      find_truncatable_primes(While, N+2, Acc)
  end.

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) when N > 3 ->
  is_prime(N, 5, trunc(math:sqrt(N)) + 1).

is_prime(_N, From, To) when From > To ->
  true;
is_prime(N, From, _To) when N rem From =:= 0; N rem (From + 2) =:= 0 ->
  false;
is_prime(N, From, To) ->
  is_prime(N, From + 6, To).

is_truncatable_prime(N) ->
  is_prime(N) andalso
  is_left_prime(truncate_left(N)) andalso
  is_right_prime(truncate_right(N)).

is_left_prime(N) when N < 10 ->
  is_prime(N);
is_left_prime(N) ->
  case is_prime(N) of
    true ->
      is_left_prime(truncate_left(N));
    false ->
      false
  end.

is_right_prime(N) when N < 10 ->
  is_prime(N);
is_right_prime(N) ->
  case is_prime(N) of
    true ->
      is_right_prime(truncate_right(N));
    false ->
      false
  end.

main() ->
  TPs = find_truncatable_primes(fun(Acc) -> length(Acc) < 11 end),
  io:format("~w~n~p~n", [TPs, lists:sum(TPs)]),
  ok.

truncate_left(N) ->
  truncate_left(N, 1).

truncate_left(N, Place) when 10 * Place > N ->
  N rem Place;
truncate_left(N, Place) ->
  truncate_left(N, 10 * Place).

truncate_right(N) ->
  N div 10.

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
   ?_assertNot(is_prime(10)),
   ?_assert(is_prime(11)),
   ?_assertNot(is_prime(12)),
   ?_assert(is_prime(13)),
   ?_assertNot(is_prime(14)),
   ?_assertNot(is_prime(15)),
   ?_assertNot(is_prime(16)),
   ?_assert(is_prime(17)),
   ?_assertNot(is_prime(18)),
   ?_assert(is_prime(19))
  ].

is_truncatable_prime_test_() ->
  [
   ?_assert(is_truncatable_prime(3797)),
   ?_assertNot(is_truncatable_prime(19))
  ].
truncate_left_test_() ->
  [
   ?_assertEqual(23, truncate_left(123)),
   ?_assertEqual(3, truncate_left(23))
  ].

truncate_right_test_() ->
  [
   ?_assertEqual(12, truncate_right(123))
  ].
