-module(roman).

-export(
   [
    main/0,
    read/1,
    to_decimal/1,
    to_roman/1
   ]
  ).

%%
%% I  1
%% IV 4
%% V  5
%% IX 9
%% X  10
%% XL 40
%% L  50
%% XC 90
%% C  100
%% CD 400
%% D  500
%% CM 900
%% M  1000
%%
%% I.   Numerals must be arranged in descending order of size.
%% II.  M, C, and X cannot be equalled or exceeded by smaller denominations.
%% III. D, L, and V can each only appear once.
%% 
%% i.   Only one I, X, and C can be used as the leading numeral in part of a 
%%      subtractive pair.
%% ii.  I can only be placed before V and X.
%% iii. X can only be placed before L and C.
%% iv.  C can only be placed before D and M.
%%

-define(ROMAN_NUMERALS, "p089_roman.txt").

main() ->
  RN1 = read(?ROMAN_NUMERALS),
  RN2 = lists:map(fun(R) -> to_roman(to_decimal(R)) end, RN1),
  L1 = length(lists:flatten(RN1)),
  L2 = length(lists:flatten(RN2)),
  io:format("~p - ~p = ~p~n", [L1, L2, L1 - L2]),
  ok.

read(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  re:split(Binary, "\n", [{return, list}, trim]).

to_decimal(Roman) ->
  Match =
  re:run(
    Roman, 
    "(IV|IX|XL|XC|CD|CM|I|V|X|L|C|D|M)", 
    [global, {capture, all_but_first, list}]),
  Decimal =
  case Match of
    {match, Captured} ->
      lists:map(
        fun
          (["I"])  -> 1;
          (["IV"]) -> 4;
          (["V"])  -> 5;
          (["IX"]) -> 9;
          (["X"])  -> 10;
          (["XL"]) -> 40;
          (["L"])  -> 50;
          (["XC"]) -> 90;
          (["C"])  -> 100;
          (["CD"]) -> 400;
          (["D"])  -> 500;
          (["CM"]) -> 900;
          (["M"])  -> 1000
        end,
        Captured);
    _ ->
      nomatch
  end,
  lists:sum(Decimal).

to_roman(Decimal) ->
  to_roman(
    Decimal,
    [
     {"M", 1000}, 
     {"CM", 900}, 
     {"D",  500}, 
     {"CD", 400}, 
     {"C",  100}, 
     {"XC",  90}, 
     {"L",   50}, 
     {"XL",  40}, 
     {"X",   10}, 
     {"IX",   9}, 
     {"V",    5}, 
     {"IV",   4}, 
     {"I",    1}
    ],
    []).

to_roman(_Decimal, [], Acc) ->
  lists:flatten(lists:reverse(Acc));
to_roman(Decimal, [{Number, Value} | T], Acc) ->
  Count = Decimal div Value,
  if
    Count > 0 ->
      to_roman(Decimal - Count * Value, T, [lists:duplicate(Count, Number) | Acc]);
    Count =< 0 ->
      to_roman(Decimal, T, Acc)
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

to_decimal_test_() ->
  [
   ?_assertEqual(1, to_decimal("I")),
   ?_assertEqual(2, to_decimal("II")),
   ?_assertEqual(3, to_decimal("III")),
   ?_assertEqual(4, to_decimal("IV")),
   ?_assertEqual(5, to_decimal("V")),
   ?_assertEqual(6, to_decimal("VI")),
   ?_assertEqual(7, to_decimal("VII")),
   ?_assertEqual(8, to_decimal("VIII")),
   ?_assertEqual(9, to_decimal("IX")),
   ?_assertEqual(10, to_decimal("X")),
   ?_assertEqual(40, to_decimal("XL")),
   ?_assertEqual(50, to_decimal("L")),
   ?_assertEqual(90, to_decimal("XC")),
   ?_assertEqual(100, to_decimal("C")),
   ?_assertEqual(400, to_decimal("CD")),
   ?_assertEqual(500, to_decimal("D")),
   ?_assertEqual(900, to_decimal("CM")),
   ?_assertEqual(1000, to_decimal("M")),
   ?_assertEqual(1900, to_decimal("MCM")),
   ?_assertEqual(1999, to_decimal("MCMXCIX"))
  ].

to_roman_test_() ->
  [
   ?_assertEqual(
      ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"], 
      [to_roman(I) || I <- lists:seq(1,10)]),
   ?_assertEqual(
      ["XL", "L", "XC", "C", "CD", "D", "CM", "M"],
      [to_roman(I) || 
       I <- [40, 50, 90, 100, 400, 500, 900, 1000]]),
   ?_assertEqual("MCM", to_roman(1900)),
   ?_assertEqual("MCMXCIX", to_roman(1999))
  ].

-endif.
