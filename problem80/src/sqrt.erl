-module(sqrt).

-export(
   [
    add/2,
    babylonian/2,
    digits/1,
    digits_in_root/2,
    divide/2,
    gcd/2,
    irrational_roots/2,
    main/0,
    mul/2,
    reduce/1,
    to_decimal/2,
    to_fraction/1
   ]
  ).

add(A,B) when is_integer(A); is_integer(B) ->
  add(to_fraction(A), to_fraction(B));
add({A1, A2}, {B1, B2}) ->
  reduce({A1*B2 + B1*A2, A2*B2}).

babylonian(Iterations, S) ->
  babylonian(Iterations, S, trunc(math:sqrt(S))).

babylonian(0, _S, Xn) ->
  Xn;
babylonian(Iterations, S, Xn) ->
  babylonian(Iterations - 1, 
             S, 
             mul({1,2}, add(Xn, divide(S, Xn)))).

digits(N) ->
  lists:map(fun(D) -> D - $0 end, integer_to_list(N)).

digits_in_root(N, Root) ->
  {Integer, Fraction} = to_decimal(N, Root),
  Digits = lists:flatten(Integer, Fraction),
  lists:sublist(Digits, N).

divide(A,B) when is_integer(A); is_integer(B) ->
  divide(to_fraction(A), to_fraction(B));
divide({A1, A2}, {B1, B2}) ->
  mul({A1, A2}, {B2, B1}).

gcd(A,A) ->
  A;
gcd(A,B) ->
  case A > B of
    true ->
      gcd(A-B, B);
    false ->
      gcd(A, B-A)
  end.

irrational_roots(From, To) ->
  lists:filter(
    fun({_A,B}) ->
        B =/= 1
    end,
    [babylonian(8, I) || I <- lists:seq(From, To)]).

main() ->
  Sum =
  lists:sum(
    lists:map(
      fun(Sqrt) ->
          lists:sum(digits_in_root(100, Sqrt))
      end,
      irrational_roots(1,100))),
  io:format("sum: ~p~n", [Sum]),
  ok.

mul(A,B) when is_integer(A); is_integer(B) ->
  mul(to_fraction(A), to_fraction(B));
mul({A1,A2}, {B1,B2}) ->
  reduce({A1*B1, A2*B2}).

reduce(F) when is_integer(F) ->
  F;
reduce({A,B}) ->
  Gcd = gcd(A,B),
  {A div Gcd, B div Gcd}.

to_decimal(_Precision, A) when is_integer(A) ->
  {digits(A), [0]};
to_decimal(Precision, {A,B}) ->
  {digits(A div B), to_decimal(Precision, {A rem B, B}, [])}.

to_decimal(Precision, {A,_B}, Acc) when Precision =:= 0; A =:= 0 ->
  lists:reverse(Acc);
to_decimal(Precision, {A, B}, Acc) when A < B ->
  A10 = 10*A,
  case A10 < B of
    true ->
      to_decimal(Precision - 1, {A10, B}, [0|Acc]);
    false ->
      to_decimal(Precision, {10*A, B}, Acc)
  end;
to_decimal(Precision, {A, B}, Acc) ->
  to_decimal(Precision - 1, {A rem B, B}, [A div B | Acc]).

to_fraction(A) when is_integer(A) ->
  {A,1};
to_fraction(A) when is_tuple(A) ->
  A.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

add_test_() ->
  [
   ?_assertEqual({3,2}, add(1, {1,2})),
   ?_assertEqual({3,2}, add({1,2}, 1)),
   ?_assertEqual({1,1}, add({1,2}, {1,2})),
   ?_assertEqual({7,6}, add({2,3}, {1,2})),
   ?_assertEqual({7,6}, add({1,2}, {2,3}))
  ].

babylonian_test_() ->
  [
   ?_assertEqual({1,1}, babylonian(8,1)),
   ?_assertEqual({577,408}, babylonian(3,2)),
   ?_assertEqual({97,56}, babylonian(3,3)),
   ?_assertEqual({2,1}, babylonian(8,4)),
   ?_assertEqual({3,1}, babylonian(8,9))
  ].

digits_in_root_test_() ->
  Digits = digits_in_root(100, babylonian(8, 2)),
  [
   ?_assertEqual(100, length(Digits)),
   ?_assertEqual(475, lists:sum(Digits))
  ].

divide_test_() ->
  [
    ?_assertEqual({1,2}, divide(1, 2)),
    ?_assertEqual({3,2}, divide({1,2}, {1,3}))
  ].

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2,8)),
   ?_assertEqual(2, gcd(8,2)),
   ?_assertEqual(3, gcd(24, 27))
  ].

mul_test_() ->
  [
   ?_assertEqual({1,1}, mul(2, {1,2})),
   ?_assertEqual({1,1}, mul({1,2}, 2)),
   ?_assertEqual({2,9}, mul({1,3}, {2,3})),
   ?_assertEqual({2,9}, mul({2,3}, {1,3})),
   ?_assertEqual({3,16}, mul({1,16}, 3))
  ].

reduce_test_() ->
  [
   ?_assertEqual({1,2}, reduce({8,16})),
   ?_assertEqual({1,3}, reduce({3,9})),
   ?_assertEqual(3, reduce(3))
  ].

to_decimal_test_() ->
  [
   ?_assertEqual({[0], [2,5]}, to_decimal(10, {1,4})),
   ?_assertEqual({[0], [3,3,3,3,3,3,3,3,3,3]}, to_decimal(10, {1,3})),
   ?_assertEqual({[0], [5]}, to_decimal(10, {1,2})),
   ?_assertEqual({[0], [6,6,6,6,6,6,6,6,6,6]}, to_decimal(10, {2,3})),
   ?_assertEqual({[1], []}, to_decimal(10, {1,1})),
   ?_assertEqual({[1], [5]}, to_decimal(10, {3,2})),
   ?_assertEqual({[1,0], [5]}, to_decimal(10, {21,2}))
  ].

to_fraction_test_() ->
  [
   ?_assertEqual({2,1}, to_fraction(2)),
   ?_assertEqual({1,2}, to_fraction({1,2}))
  ].

-endif.
