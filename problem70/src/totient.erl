-module(totient).

-export(
   [
    digits/1,
    factor/1,
    find_totient_permutations/1,
    init/0,
    is_permutation/2,
    is_prime/1,
    main/0,
    multiply/1,
    phi/1,
    primes/1,
    start/0,
    stop/0
   ]
  ).

-record(
   state,
   {
    last_prime = 2,
    primes = [2]
   }
  ).

digits(N) ->
  lists:sort(integer_to_list(N)).

factor(1) ->
  [1];
factor(N) ->
  Primes = primes(trunc(math:sqrt(N)) + 1),
  factor(Primes, N, []).

factor([], N, Factors) ->
  Factors2 =
  if
    N > 1 ->
      [N | Factors];
    true ->
      Factors
  end,
  lists:reverse(Factors2);
factor([P|_T], N, Factors) when P * P > N ->
  factor([], N, Factors);
factor([P|T], N, Factors) ->
  if
    N rem P =:= 0 ->
      factor([P|T], N div P, [P | Factors]);
    true ->
      factor(T, N, Factors)
  end.

find_totient_permutations(N) ->
  find_totient_permutations(2, N, []).

find_totient_permutations(I, N, Acc) when I > N ->
  lists:reverse(Acc);
find_totient_permutations(I, N, Acc) ->
  Phi = phi(I),
  NewAcc =
  case is_permutation(I, Phi) of
    true ->
      [{I, Phi, I / Phi} | Acc];
    false ->
      Acc
  end,
  find_totient_permutations(I+1, N, NewAcc).

init() ->
  loop(#state{}).

is_permutation(A, B) ->
  digits(A) =:= digits(B).

is_prime(1) ->
  false;
is_prime(N) when N =:= 2; N =:= 3->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I * I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I + 2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I + 6, N).

loop(State) ->
  NewState =
  receive
    {primes, N, From} ->
      case N > State#state.last_prime of
        true ->
          Primes = 
          lists:append(
            State#state.primes, 
            lists:filter(fun is_prime/1, 
                         lists:seq(State#state.last_prime + 1, N))),
          From ! Primes,
          State#state{last_prime=N, primes=Primes};
        false ->
          From ! lists:takewhile(fun(P) -> P =< N end, State#state.primes),
          State
      end;
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

main() ->
  start(),
  TPs = find_totient_permutations(10000000 - 1),
  io:format("~p~n", [TPs]),
  MinTP =
  lists:foldl(
    fun({_, _, R} = E, {_, _, RMin} = Min) ->
        case R < RMin of
          true ->
            E;
          false ->
            Min
        end
    end,
    hd(TPs),
    tl(TPs)),
  io:format("Min: ~p~n", [MinTP]),
  ok = stop(),
  ok.

multiply(L) ->
  lists:foldl(
    fun(N, Product) ->
        N * Product
    end,
    1,
    L).

phi(N) ->
  Factors = lists:usort(factor(N)),
  (N div multiply(Factors)) * 
  multiply(lists:map(fun(F) -> F - 1 end, Factors)).

primes(N) ->
  primes ! {primes, N, self()},
  receive
    Result ->
      Result
  end.

start() ->
  register(primes, spawn(?MODULE, init, [])).

stop() ->
  primes ! {stop, self()},
  receive
    Result ->
      Result
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

digits_test_() ->
  [
   ?_assertEqual("1", digits(1)),
   ?_assertEqual("12", digits(12)),
   ?_assertEqual("123", digits(123)),
   ?_assertEqual("123", digits(213)),
   ?_assertEqual("123", digits(321)),
   ?_assertEqual("1223", digits(3212))
  ].

factor_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([1], factor(1)),
    ?_assertEqual([2], factor(2)),
    ?_assertEqual([3], factor(3)),
    ?_assertEqual([2,2], factor(4)),
    ?_assertEqual([5], factor(5)),
    ?_assertEqual([2,3], factor(6)),
    ?_assertEqual([7], factor(7)),
    ?_assertEqual([2,2,2], factor(8)),
    ?_assertEqual([3,3], factor(9)),
    ?_assertEqual([2,5], factor(10)),
    ?_assertEqual([11], factor(11))
   ]
  }.

is_permutation_test_() ->
  [
   ?_assert(is_permutation(123, 321)),
   ?_assertNot(is_permutation(12345, 123))
  ].

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
   ?_assert(is_prime(11))
  ].

multiply_test_() ->
  [
   ?_assertEqual(1, multiply([1])),
   ?_assertEqual(120, multiply([1,2,3,4,5]))
  ].

phi_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(1, phi(2)),
    ?_assertEqual(2, phi(3)),
    ?_assertEqual(2, phi(4)),
    ?_assertEqual(4, phi(5)),
    ?_assertEqual(2, phi(6)),
    ?_assertEqual(6, phi(7)),
    ?_assertEqual(4, phi(8)),
    ?_assertEqual(6, phi(9)),
    ?_assertEqual(4, phi(10))
   ]
  }.

primes_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([2], primes(2)),
    ?_assertEqual([2,3,5], primes(6)),
    ?_assertEqual([2,3,5,7,11,13,17,19], primes(20)),
    ?_assertEqual([2,3,5,7], primes(8))
   ]
  }.

-endif.
