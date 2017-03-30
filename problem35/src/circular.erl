-module(circular).

-export(
   [
    digits_to_number/1,
    find/0,
    find/1,
    is_prime/1,
    is_circular_prime/1,
    main/0,
    number_to_digits/1,
    odd_seq/1,
    rotate/1,
    rotate/2,
    rotations/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

digits_to_number(Digits) ->
  lists:foldl(fun(D, N) -> 10*N + D end, 0, Digits).

find() ->
  lists:merge(
    [
     find(1),
     find(2),
     find(3),
     find(4),
     find(5),
     find(6)
    ]
   ).

find(1) ->
  [[2],[3],[5],[7]];
find(N) ->
  Rotations =
  lists:map(
    fun(S) ->
        [digits_to_number(R) || R <- rotations(S)]
    end, 
    odd_seq(N)),
  lists:filter(fun(R) -> is_circular_prime(R) end, Rotations).

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(N, lists:seq(5, trunc(math:sqrt(N)) + 1, 6)).

is_prime(_N, []) ->
  true;
is_prime(N, [H|_T]) when N rem H == 0; N rem (H + 2) =:= 0 ->
  false;
is_prime(N, [_H|T]) ->
  is_prime(N, T).

is_circular_prime(Rotations) ->
  lists:all(fun(N) -> is_prime(N) end, Rotations).

main() ->
  Seq = find(),
  Primes = lists:usort(lists:flatten(Seq)),
  io:format("~p:~n~p~n", [length(Primes), Primes]),
  ok.

number_to_digits(0) ->
  [0];
number_to_digits(N) ->
  number_to_digits(N, []).

number_to_digits(0, Digits) ->
  Digits;
number_to_digits(N, Digits) ->
  number_to_digits(N div 10, [N rem 10 | Digits]).

odd_seq(N) ->
  odd_seq(N, [1,3,7,9]).

odd_seq(1, Seq) ->
  [[N] || N <- Seq];
odd_seq(N, Seq) ->
  [[E|S] || E <- Seq, S <- odd_seq(N-1, Seq)].

rotate(L) ->
  rotate(1, L).

rotate(N, L) ->
  {L1, L2} = lists:split(N, L),
  lists:append(L2, L1).

rotations(L) ->
  [rotate(N, L) || N <- lists:seq(0,length(L) - 1)].

%%
%% unit tests
%%
digits_to_number_test_() ->
  [
   ?_assertEqual(0, digits_to_number([0])),
   ?_assertEqual(1, digits_to_number([1])),
   ?_assertEqual(10, digits_to_number([1,0])),
   ?_assertEqual(123, digits_to_number([1,2,3]))
  ].

is_circular_prime_test_() ->
  [
   ?_assert(is_circular_prime([2])),
   ?_assert(is_circular_prime([11])),
   ?_assert(is_circular_prime([17,71])),
   ?_assertNot(is_circular_prime([4])),
   ?_assertNot(is_circular_prime([12,21]))
  ].

is_prime_test_() ->
  [
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

number_to_digits_test_() ->
  [
   ?_assertEqual([0], number_to_digits(0)),
   ?_assertEqual([1], number_to_digits(1)),
   ?_assertEqual([1,2,3], number_to_digits(123))
  ].

rotate_test_() ->
  [
   ?_assertEqual([1], rotate([1])),
   ?_assertEqual([1,2,3], rotate([3,1,2])),
   ?_assertEqual([1,2,3], rotate(2,[2,3,1])),
   ?_assertEqual([1,2,3], rotate(0,[1,2,3]))
  ].

rotations_test_() ->
  [
   ?_assertEqual([[1]], rotations([1])),
   ?_assertEqual([[1,2], [2,1]], rotations([1,2])),
   ?_assertEqual([[1,2,3], [2,3,1], [3,1,2]], rotations([1,2,3]))
  ].
