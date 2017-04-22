-module(factor).

-export(
   [
    ceil/1,
    factor/1,
    fermat_factor/1,
    gcd/2,
    is_square/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the largest prime factor of the number 600851475143.
%

ceil(X) ->
  T = trunc(X),
  case (X - T) of
    Pos when Pos > 0 ->
      T + 1;
    _ -> T
  end.

factor(N) ->
  FactorsOfN =
  case fermat_factor(N) of
    [1,N] ->
      [N];
    Factors ->
      lists:foldl(
        fun
          (1, Acc) ->
            Acc;
          (2, Acc) ->
            [2 | Acc];
          (E, Acc) ->
            [factor(E), Acc]
        end,
        [],
        Factors)
  end,
  lists:sort(lists:flatten(FactorsOfN)).

fermat_factor(N) when N rem 2 =:= 0 ->
  [2 | fermat_factor(N div 2)];
fermat_factor(N) when N rem 2 =/= 0 ->
  A = ceil(math:sqrt(N)),
  fermat_factor(N, A).

fermat_factor(N, A) ->
  B2 = A * A - N,
  case is_square(B2) of
    false ->
      fermat_factor(N, A+1);
    B ->
      [A-B, A+B]
  end.

gcd(X,0) ->
  X;
gcd(X,Y) when X < Y ->
  gcd(Y,X);
gcd(X,Y) ->
  gcd(Y,X rem Y).

is_square(N) ->
  Sqrt = trunc(math:sqrt(N)),
  if
    Sqrt * Sqrt =:= N ->
      Sqrt;
    true ->
      false
  end.

main() ->
  io:format("~p~n", [lists:max(factor(600851475143))]).

%%
%% unit tests
%%
ceil_test_() ->
  [
   ?_assert(0 =:= ceil(-0.1)),
   ?_assert(2 =:= ceil(2)),
   ?_assert(3 =:= ceil(2.1))
  ].

factor_test_() ->
  [
   ?_assert([1] =:= factor(1)),
   ?_assert([2] =:= factor(2)),
   ?_assert([3] =:= factor(3)),
   ?_assert([2,2] =:= factor(4)),
   ?_assert([5] =:= factor(5))
  ].

fermat_factor_test_() ->
  [
   ?_assert([59, 101] =:= fermat_factor(5959))
  ].

gcd_test_() ->
  [
   ?_assert(3 =:= gcd(3,0)),
   ?_assert(2 =:= gcd(8,2))
  ].

is_square_test_() ->
  [
   ?_assert(not is_square(2)),
   ?_assert(not is_square(3)),
   ?_assert(2 =:= is_square(4))
  ].
