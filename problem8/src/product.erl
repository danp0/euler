-module(product).

-export(
   [
    largest_product/2,
    main/0,
    partition/2,
    product/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Find the thirteen adjacent digits in the 1000-digit number
% that have the greatest product. Find the product.
%

largest_product(L, N) ->
  GreaterThan = fun(E1, E2) -> E1 >= E2 end,
  SortedElements =
  lists:sort(GreaterThan,
    lists:map(
      fun(E) -> product(E) end,
      partition(L, N))),
  case SortedElements of
    [] ->
      0;
    [H|_T] ->
      H
  end.

main() ->
  Number = 
  "73167176531330624919225119674426574742355349194934" ++
  "96983520312774506326239578318016984801869478851843" ++
  "85861560789112949495459501737958331952853208805511" ++
  "12540698747158523863050715693290963295227443043557" ++
  "66896648950445244523161731856403098711121722383113" ++
  "62229893423380308135336276614282806444486645238749" ++
  "30358907296290491560440772390713810515859307960866" ++
  "70172427121883998797908792274921901699720888093776" ++
  "65727333001053367881220235421809751254540594752243" ++
  "52584907711670556013604839586446706324415722155397" ++
  "53697817977846174064955149290862569321978468622482" ++
  "83972241375657056057490261407972968652414535100474" ++
  "82166370484403199890008895243450658541227588666881" ++
  "16427171479924442928230863465674813919123162824586" ++
  "17866458359124566529476545682848912883142607690042" ++
  "24219022671055626321111109370544217506941658960408" ++
  "07198403850962455444362981230987879927244284909188" ++
  "84580156166097919133875499200524063689912560717606" ++
  "05886116467109405077541002256983155200055935729725" ++
  "71636269561882670428252483600823257530420752963450",
  io:format("~p~n", [Number]),
  io:format(" 4-digit: ~p~n", [largest_product(Number, 4)]),
  io:format("13-digit: ~p~n", [largest_product(Number, 13)]).

partition(L, N) ->
  lists:reverse(partition(L, N, [])).

partition([], _N, Acc) ->
  Acc;
partition(L, N, Acc) ->
  S = lists:sublist(L, N),
  case length(S) of
    N ->
      [_|T] = L,
      partition(T, N, [S|Acc]);
    _ ->
      Acc
  end.

product(Digits) ->
  lists:foldl(
    fun(E, Product) ->
        {V, ""} = string:to_integer([E]),
        V * Product
    end,
    1,
    Digits).

%%
%% tests
%%
largest_product_test_() ->
  [
   ?_assert(20 =:= largest_product("12345", 2)),
   ?_assert(60 =:= largest_product("12345", 3))
  ].

partition_test_() ->
  [
   ?_assert([] =:= partition("", 1)),
   ?_assert(["1", "2", "3"] =:= partition("123", 1)),
   ?_assert(["12", "23"] =:= partition("123", 2)),
   ?_assert(["123"] =:= partition("123", 3)),
   ?_assert(["12", "23", "34", "45"] =:= partition("12345", 2))
  ].

product_test_() ->
  [
   ?_assert(0 =:= product("10")),
   ?_assert(2 =:= product("12")),
   ?_assert(6 =:= product("123")),
   ?_assert(24 =:= product("1234")),
   ?_assert(120 =:= product("12345"))
  ].
