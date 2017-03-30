-module(champernowne).

-export(
   [
    digits/1,
    generate/1,
    main/0,
    nth/2,
    pow/2,
    range_of/2,
    ranges/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%%
%% positions:
%%
%% 1               9 10,11 12,13 14,15 16,17 18,19 20,21 22,23 24,25 26,27 28,29 30,31     
%% 1 2 3 4 5 6 7 8 9 10    11    12    13    14    15    16    17    18    19    20
%%
%% 188,189 190,191,192 193,194,195
%% 99      100         101
%%
%%          1   10     100     1000     10000   100000
%%          ---------------------------------------------
%% digits   9 + 2*90 + 3*900 + 4*9000 + 5*90000 6*900000
%%
%% ranges:
%% 9                               = 1..9
%% 9+180                           = 10..189
%% 9+180+2700                      = 190..2889
%% 9+180+2700+36000                = 2890..38889
%% 9+180+2700+36000+450000         = 38890..488889
%% 9+180+2700+36000+450000+5400000 = 488890..5888889
%%

digits(0) ->
  [0];
digits(N) ->
  digits(N, []).

digits(0, Digits) ->
  Digits;
digits(N, Digits) ->
  digits(N div 10, [N rem 10|Digits]).

generate(Length) ->
  generate(Length, 1, [], []).

generate(Length, _N, _Buffer, Digits) when Length =:= length(Digits) ->
  lists:reverse(Digits);
generate(Length, N, [], Digits) ->
  generate(Length, N+1, digits(N), Digits);
generate(Length, N, [H|T], Digits) ->
  generate(Length, N, T, [H|Digits]).

main() ->
  io:format("champernowne...~n", []),
  Ranges = ranges(6),
  Powers = [pow(10, N) || N <- lists:seq(0,6)],
  Digits = [nth(N, Ranges) || N <- Powers],
  Product = lists:foldl(fun(N, P) -> N * P end, 1, Digits),
  io:format("Ranges: ~w~nPowers: ~w~n Digits: ~p~n Product: ~p~n", 
            [Ranges, Powers, Digits, Product]),
  ok.

nth(N, Ranges) ->
  {Seq, {From, _To}} = range_of(N, Ranges),
  Start = pow(10, Seq - 1),
  Nth = digits(Start + ((N - From) div Seq)), 
  Digit = 1 + ((N - From) rem Seq),
  lists:nth(Digit, Nth).

pow(_X, 0) ->
  1;
pow(X, Y) ->
  X * pow(X, Y-1).

range_of(N, Ranges) ->
  case lists:dropwhile(fun({_Nth, {_From, To}}) -> N > To end, Ranges) of
    [] ->
      out_of_range;
    [H|_T] ->
      H
  end.

ranges(N) ->
  Widths = 
  [X * 9 * pow(10, X - 1) || X <- lists:seq(1,N)],
  UpperRanges =
  lists:reverse(
    lists:foldl(
      fun(X, Sums) ->
          {L, _} = lists:split(X, Widths),
          [lists:sum(L) | Sums]
      end,
      [],
      lists:seq(1,N))),
  {LowerRanges, _} = lists:split(N-1, lists:map(fun(X) -> X+1 end, UpperRanges)),
  lists:zip(lists:seq(1,N), lists:zip([1|LowerRanges], UpperRanges)).

%%
%% unit test
%%
digits_test_() ->
  [
   ?_assertEqual([0], digits(0)),
   ?_assertEqual([1], digits(1)),
   ?_assertEqual([1,2,3], digits(123))
  ].

generate_test_() ->
  [
   ?_assertEqual([1], generate(1)),
   ?_assertEqual([1,2], generate(2)),
   ?_assertEqual([1,2,3,4,5,6,7,8,9], generate(9)),
   ?_assertEqual([1,2,3,4,5,6,7,8,9,1], generate(10)),
   ?_assertEqual([1,2,3,4,5,6,7,8,9,1,0], generate(11))
  ].

nth_test_() ->
  Ranges = ranges(6),
  [
   ?_assertEqual([nth(N, Ranges) || N <- lists:seq(1,1)], generate(1)),
   ?_assertEqual([nth(N, Ranges) || N <- lists:seq(1,10000)], generate(10000))
  ].

pow_test_() ->
  [
   ?_assertEqual(1, pow(10,0)),
   ?_assertEqual(10, pow(10,1)),
   ?_assertEqual(100, pow(10,2))
  ].

range_of_test_() ->
  Ranges = ranges(6),
  [
   ?_assertEqual({1,{1,9}}, range_of(1, Ranges)),
   ?_assertEqual({1,{1,9}}, range_of(2, Ranges)),
   ?_assertEqual({1,{1,9}}, range_of(3, Ranges)),
   ?_assertEqual({2,{10,189}}, range_of(10, Ranges)),
   ?_assertEqual({2,{10,189}}, range_of(11, Ranges)),
   ?_assertEqual({2,{10,189}}, range_of(12, Ranges)),
   ?_assertEqual({6,{488890,5888889}}, range_of(488890, Ranges)),
   ?_assertEqual({6,{488890,5888889}}, range_of(488891, Ranges)),
   ?_assertEqual({6,{488890,5888889}}, range_of(488892, Ranges)),
   ?_assertEqual(out_of_range, range_of(5888890, Ranges))
  ].

ranges_test_() ->
  [
   ?_assertEqual([{1, {1,9}}], ranges(1)),
   ?_assertEqual(
      [{1,{1,9}},
       {2,{10,189}},
       {3,{190,2889}},
       {4,{2890,38889}},
       {5,{38890,488889}},
       {6,{488890,5888889}}], ranges(6))
  ].
