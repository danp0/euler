-module(pandigital).

-export(
   [
    digits_to_number/1,
    find_pandigital_products/0,
    find_pandigital_products/2,
    is_pandigital_product/2,
    main/0,
    make_combinations/2,
    make_pairs/3,
    number_to_digits/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the sum of all products whose multiplicand/multiplier/product
% identity can be written as a 1 - 9 pandigital number.
%

digits_to_number(Digits) ->
  digits_to_number(Digits, 0).

digits_to_number([], Number) ->
  Number;
digits_to_number([D|T], Number) ->
  digits_to_number(T, D + 10 * Number).

find_pandigital_products() ->
  lists:merge(
    [
      find_pandigital_products(1,4),
      find_pandigital_products(2,3)
    ]).

find_pandigital_products(M,N) ->
  Digits = [1,2,3,4,5,6,7,8,9],
  lists:filter(
    fun(E) -> E =/= false end,
    [is_pandigital_product(P1,P2) || {P1,P2} <- make_pairs(M,N,Digits)]). 

is_pandigital_product(A,B) when is_list(A) andalso is_list(B) ->
  is_pandigital_product(digits_to_number(A), digits_to_number(B));
is_pandigital_product(A,B) ->
  C = A * B,
  Digits = lists:merge(
             [number_to_digits(A), 
              number_to_digits(B), 
              number_to_digits(C)]),
  case [1,2,3,4,5,6,7,8,9] =:= lists:sort(Digits) of
    false ->
      false;
    true ->
      {A, B, C}
  end.

main() ->
  PanDigitalProducts = find_pandigital_products(),
  io:format("pandigital products:~n~p~n", [PanDigitalProducts]),
  io:format("sum: ~p~n",
            [lists:sum(
               lists:usort(
                 lists:map(fun({_A, _B, C}) -> C end, PanDigitalProducts)))]),
  ok.

make_combinations(1, Set) ->
  [[E] || E <- Set];
make_combinations(N, Set) ->
  [ [E1|S1] || E1 <- Set, S1 <- make_combinations(N-1, Set -- [E1]) ].

make_pairs(M, 1, Set) ->
  [{S1, [E2]} || S1 <- make_combinations(M, Set), E2 <- Set -- S1];
make_pairs(M, N, Set) when M < N ->
  make_pairs(N, M, Set);
make_pairs(M, N, Set) ->
  [ {[E1|S1], [E2|S2]} || 
    E1 <- Set,
    E2 <- Set -- [E1], 
    {S1, S2} <- make_pairs(M-1, N-1, Set -- [E1,E2]) ].

number_to_digits(N) ->
  number_to_digits(N, []).

number_to_digits(0, []) ->
  [0];
number_to_digits(0, Digits) ->
  Digits;
number_to_digits(N, Digits) ->
  number_to_digits(N div 10, [N rem 10 | Digits]).

%%
%% unit testing
%%
digits_to_number_test_() ->
  [
   ?_assertEqual(0, digits_to_number([0])),
   ?_assertEqual(10, digits_to_number([1,0])),
   ?_assertEqual(1, digits_to_number([1])),
   ?_assertEqual(12, digits_to_number([1,2])),
   ?_assertEqual(123, digits_to_number([1,2,3]))
  ].

is_pandigital_product_test_() ->
  [
   ?_assertNot(is_pandigital_product(2, 3)),
   ?_assertEqual({39, 186, 7254}, is_pandigital_product([3,9], [1,8,6]))
  ].

number_to_digits_test_() ->
  [
   ?_assertEqual([0], number_to_digits(0)),
   ?_assertEqual([1,0], number_to_digits(10)),
   ?_assertEqual([1], number_to_digits(1)),
   ?_assertEqual([1,2], number_to_digits(12)),
   ?_assertEqual([1,2,3], number_to_digits(123))
  ].

