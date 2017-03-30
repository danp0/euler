-module(repunit1).

-export(
   [
    a/1,
    fermat_factor/1,
    is_prime/1,
    main/0,
    rdiv/2,
    rfactor/2,
    sieve/1
   ]
  ).

%%
%% r(n) = (10^n - 1) / 9
%% r(n) = 10*r(n-1) + 1
%% r(n) = (10^(n/2) - 1)(10^(n/2) + 1)
%%      = 9r(n/2)(10^(n/2) + 1)
%% [11,17,41,73,101,137,251,271,353,401,449,641,751]
%%
a(N) ->
  case gcd(N, 10) =:= 1 of
    true ->
      a(N, 1 rem N, 1);
    false ->
      0
  end.

a(_N, 0, K) ->
  K;
a(N, R, K) ->
  a(N, (R*10 + 1) rem N, K+1).


ceil(N) when N < 0 ->
  trunc(N);
ceil(N) ->
  T = trunc(N),
  case N - T == 0 of
    true ->
      T;
    false ->
      T + 1
  end.

fermat_factor(N) ->
  fermat_factor([N], []).

fermat_factor([], Ack) ->
  lists:sort(Ack);
fermat_factor([1], []) ->
  [1];
fermat_factor([2], []) ->
  [2];
fermat_factor([N|T], Ack) when N rem 2 =:= 0 ->
  fermat_factor([N div 2|T], [2 | Ack]);
fermat_factor([N|T], Ack) ->
  A = ceil(math:sqrt(N)),
  B = A*A - N,
  F = fermat_factor(A, B, N),
  case F of
    [] ->
      fermat_factor(T, Ack);
    [N] ->
      fermat_factor(T, [N|Ack]);
    [F1, F2] ->
      fermat_factor([F1, F2|T], Ack)
  end.

fermat_factor(A, B, N) ->
  BSqrt = trunc(math:sqrt(B)),
  case B =:= BSqrt*BSqrt of
    true ->
      F1 = A - BSqrt,
      F2 = A + BSqrt,
      lists:filter(fun(F) -> F =/= 1 end, [F1, F2]);
    false ->
      fermat_factor(A+1, (A+1)*(A+1) - N, N)
  end.

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =< 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

main() ->
  io:format("repunit...~n", []),
  Factors = [11,17,41,73,101,137,251,257,271,353,401,449,641,751,1201,1409,1601,3541,4001,4801,5051,9091,10753,15361,16001,19841,21001,21401,24001,25601,27961,37501,40961,43201,60101,62501,69857,76001,76801,160001], %%,162251,453377,524801,544001,670001,952001,976193,980801],
 %% Primes = sieve(1000000),
 %% Factors =
 %% [P || 
 %%  P <- Primes, 
 %%  A <- [a(P)], 
 %%  A =/= 0,
 %%  1000000000 rem A =:= 0
 %%  %lists:filter(fun(E) -> E =/= 2 andalso E =/= 5 end, fermat_factor(A)) =:= []
 %% ],
  io:format("~w~n", [Factors]),
  io:format("length: ~p~n", [length(Factors)]),
  io:format("~p~n", [fermat_factor(1000000000)]),
  io:format("sum: ~p~n", [lists:sum(Factors)]),
  %%io:format("1: ~w~n", [[P || P <- Primes, rdiv(1000000, P) =:= 0]]),
  %%io:format("1: ~w~n", [rfactor(10, 4)]),
  %%io:format("2: ~w~n", [rfactor(100, 4)]),
  %%io:format("3: ~w~n", [rfactor(1000, 4)]),
  %%io:format("4: ~w~n", [rfactor(10000, 5)]),
  %%io:format("5: ~w~n", [rfactor(100000, 6)]),
  %%io:format("6: ~w~n", [rfactor(1000000, 6)]),
  %%io:format("7: ~w~n", [rfactor(10000000, 6)]),
  ok.

rdiv(N, D) ->
  rdiv(N-1, D, 1 rem D).

rdiv(0, _D, Rem) ->
  Rem;
rdiv(N, D, Rem) ->
  rdiv(N-1, D, (10*Rem + 1) rem D).

rfactor(1, 1) ->
  [1];
rfactor(N, Count) ->
  rfactor(N, Count, 3, []).

rfactor(_N, Count, _Div, Ack) when length(Ack) == Count ->
  lists:reverse(Ack);
rfactor(N, Count, Div, Ack) ->
  Next =
  case is_prime(Div) of
    true ->
      case rdiv(N, Div) of
        0 ->
          [Div | Ack];
        _ ->
          Ack
      end;
    false ->
      Ack
  end,
  rfactor(N, Count, Div+2, Next).

sieve(N) ->
  Sieve =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{default, true}, {fixed, true}]))),
  sieve(2, N, Sieve).

sieve(I, N, Sieve) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun(Index, true, Primes) ->
          [Index | Primes];
         (_Index, false, Primes) ->
          Primes
      end,
      [],
      Sieve));
sieve(I, N, Sieve) ->
  Next =
  case array:get(I, Sieve) of
    true ->
      lists:foldl(
        fun(J, S) ->
            array:set(J, false, S)
        end,
        Sieve,
        lists:seq(I*I, N, I));
    false ->
      Sieve
  end,
  sieve(I+1, N, Next).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

fermat_factor_test_() ->
  [
   ?_assertEqual([1], fermat_factor(1)),
   ?_assertEqual([2], fermat_factor(2)),
   ?_assertEqual([3], fermat_factor(3)),
   ?_assertEqual([2,2,5,5], fermat_factor(100)),
   ?_assertEqual([3,37], fermat_factor(111)),
   ?_assertEqual([11, 41, 271, 9091], fermat_factor(1111111111))
  ].

is_prime_test_() ->
  [
   ?_assertEqual(sieve(100), [P || P <- lists:seq(1,100), is_prime(P)])
  ].

refactor_test_() ->
  [
   ?_assertEqual([1],                 rfactor(1, 1)),
   ?_assertEqual([11],                rfactor(2, 1)),
   ?_assertEqual([3, 37],             rfactor(3, 2)),
   ?_assertEqual([11, 101],           rfactor(4, 2)),
   ?_assertEqual([41, 271],           rfactor(5, 2)),
   ?_assertEqual([3, 7, 11, 13, 37],  rfactor(6, 5)),
   ?_assertEqual([239, 4649],         rfactor(7, 2)),
   ?_assertEqual([11, 73, 101, 137],  rfactor(8, 4)),
   ?_assertEqual([3, 37, 333667],     rfactor(9, 3)),
   ?_assertEqual([11, 41, 271, 9091], rfactor(10, 4))
  ].

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.

