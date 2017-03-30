-module(pandigital).

-export(
   [
    digits/1,
    find/0,
    find/2,
    generate/0,
    generate/2,
    is_pandigital_multiple/2,
    len/1,
    main/0,
    number/1,
    prefix/2
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

digits(0) ->
  [0];
digits(N) ->
  digits(N, []).

digits(0, Digits) ->
  Digits;
digits(N, Digits) ->
  digits(N div 10, [N rem 10 | Digits]).

find() ->
  Pandigital = lists:map(fun(D) -> number(D) end, generate()),
  lists:sort(
    lists:flatten(
      [find(Pandigital, I) || I <- lists:seq(1,4)]
     )
   ).

find(PandigitalNumbers, PrefixLength) ->
  lists:filter(
    fun(P) ->
        Prefix = prefix(PrefixLength, P),
        is_pandigital_multiple(P, Prefix)
    end,
    PandigitalNumbers).

generate() ->
  generate(9, lists:seq(9,1,-1)).

generate(1, Digits) ->
  [[E] || E <- Digits];
generate(N, Digits) ->
  [[E | P] || E <- Digits, P <- generate(N-1, Digits -- [E])].

is_pandigital_multiple(N, Prefix) ->
  is_pandigital_multiple(digits(N), 1, Prefix, []).

is_pandigital_multiple(Digits, _N, _Prefix, Acc) when length(Digits) =:= length(Acc) ->
  Digits =:= Acc;
is_pandigital_multiple(Digits, _N, _Prefix, Acc) when length(Digits) < length(Acc) ->
  false;
is_pandigital_multiple(Digits, N, Prefix, Acc) ->
  is_pandigital_multiple(Digits, N+1, Prefix, lists:append(Acc, digits(N*Prefix))).

len(N) ->
  length(digits(N)).

main() ->
  io:format("~p~n", [find()]),
  ok.

number(Digits) ->
  number(Digits, 0).

number([], N) ->
  N;
number([H|T], N) ->
  number(T, 10*N + H).

prefix(Len, N) when N > 0 ->
  shift(len(N) - Len, N).

shift(0, N) ->
  N;
shift(Count, N) ->
  shift(Count - 1, N div 10).

%%
%% unit tests
%%
digits_test_() ->
  [
   ?_assertEqual([0], digits(0)),
   ?_assertEqual([1], digits(1)),
   ?_assertEqual([1,2,3], digits(123))
  ].

is_pandigital_multiple_test_() ->
  [
   ?_assert(is_pandigital_multiple(192384576, 192)),
   ?_assert(is_pandigital_multiple(918273645, 9))
  ].

len_test_() ->
  [
   ?_assertEqual(1, len(0)),
   ?_assertEqual(2, len(10)),
   ?_assertEqual(3, len(123))
  ].

number_test_() ->
  [
   ?_assertEqual(0, number(digits(0))),
   ?_assertEqual(1, number(digits(1))),
   ?_assertEqual(123, number(digits(123)))
  ].

prefix_test_() ->
  [
   ?_assertEqual(1, prefix(1, 123456789)),
   ?_assertEqual(12, prefix(2, 123456789)),
   ?_assertEqual(123, prefix(3, 123456789)),
   ?_assertEqual(1234, prefix(4, 123456789)),
   ?_assertEqual(12345, prefix(5, 123456789))
  ].

