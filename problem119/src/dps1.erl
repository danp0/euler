
-module(dps1).

-export(
   [
    a/1,
    is_power/2,
    main/0,
    power/2,
    power_sums/2,
    sum_digits/1
   ]
  ).

%% 
%% digit power sum...
%%  1,  81,        9,  2 (1+8)
%%  2,  512,       8,  3 (1+2+5)
%%  3,  2401,      7,  4 (1+2+4)
%%  4,  4913,      17, 3 (1+3+4+9)
%%  5,  5832,      18, 3 (2+3+5+8)
%%  6,  17576,     26, 3 (1+5+6+7+7)
%%  7,  19683,     27, 3 (1+3+6+8+9)
%%  8,  234256,    22, 4 (2+2+3+4+5+6)
%%  9,  390625,    25, 4
%%  10, 614656,    28, 4
%%  11, 1679616,   36, 4
%%  12, 17210368,  28, 5
%%  13, 34012224,  18, 6
%%  14, 52521875,  35, 5
%%  15, 60466176,  36, 5
%%  16, 205962976, 46, 5
%%  17, 612220032, 18, 7

%% digit power sum...
%% [{81,9,2,9},
%%  {512,8,3,8},
%%  {2401,7,4,7},
%%  {4913,17,3,17},
%%  {5832,18,3,18},
%%  {17576,26,3,26},
%%  {19683,27,3,27},
%%  {234256,22,4,22},
%%  {390625,25,4,25},
%%  {614656,28,4,28},
%%  {1679616,36,4,36},
%%  {17210368,28,5,28},
%%  {34012224,18,6,18},
%%  {52521875,35,5,35},
%%  {60466176,36,5,36},
%%  {205962976,46,5,46},
%%  {612220032,18,7,18},
%%  {8303765625,45,6,45},
%%  {10460353203,27,7,27},
%%  {24794911296,54,6,54},
%%  {27512614111,31,7,31},
%%  {52523350144,34,7,34},
%%  {68719476736,64,6,64},
%%  {271818611107,43,7,43},
%%  {1174711139837,53,7,53},
%%  {2207984167552,58,7,58},
%%  {6722988818432,68,7,68},
%%  {20047612231936,46,8,46},
%%  {72301961339136,54,8,54},
%%  {248155780267521,63,8,63},
%%  {3904305912313344,54,9,54},
%%  {45848500718449031,71,9,71},
%%  {81920000000000000,20,13,20},
%%  {150094635296999121,81,9,81},
%%  {13744803133596058624,82,10,82},
%%  {19687440434072265625,85,10,85},
%%  {53861511409489970176,94,10,94},
%%  {73742412689492826049,97,10,97},
%%  {671088640000000000000,40,13,40},
%%  {8007313507497959524352,98,11,98},
%%  {14076019706120526112710656,86,13,86},
%%  {2670419511272061205254504361,91,14,91},
%%  {225179981368524800000000000000000,80,17,80},
%%  {1441151880758558720000000000000000000,80,19,80},
%%  {13508517176729920890000000000000000000,90,19,90},
%%  {1215766545905692880100000000000000000000,90,20,90},
%%  {109418989131512359209000000000000000000000,90,21,90},
%%  {9847709021836112328810000000000000000000000,90,22,90},
%%  {5233476330273605372135115210000000000000000000000000000,90,28,90}]
%% 
a(N) ->
  a(1, N, 10, []).

a(I, N, _Number, Ack) when I > N ->
  lists:reverse(Ack);
a(I, N, Number, Ack) ->
  Sum = sum_digits(Number),
  case is_power(Sum, Number) of
    false ->
      a(I, N, Number + 1, Ack);
    Power ->
      a(I+1, N, Number + 1, [{I, Number, Sum, Power}|Ack])
  end.

is_power(A, B) when B < A; A =:= 1 ->
  false;
is_power(A, B) ->
  is_power(A, B, 0).

is_power(_A, 1, Power) ->
  Power;
is_power(A, B, Power) ->
  case B rem A =:= 0 of
    true ->
      is_power(A, B div A, Power + 1);
    false ->
      false
  end.

main() ->
  io:format("digit power sum...~n", []),
  %io:format("~p~n", [a(17)]),
  %lists:foreach(
  %  fun(A) ->
  %      lists:foreach(
  %        fun(N) ->
  %            Power = power(A,N),
  %            Sum = sum_digits(Power),
  %            case Sum =:= A andalso Power >= 10 of
  %              true ->
  %                io:format("~p**~p: ~p: ~p~n", [A, N, Power, Sum]);
  %              false ->
  %                false
  %            end
  %        end,
  %        lists:seq(1,50))
  %  end,
  %  lists:seq(2,100)),
  PS = power_sums(200, 200),
  io:format("~p~n", [PS]),
  io:format("~p~n", [lists:nth(30, PS)]),
  ok.

power(_A,0) ->
  1;
power(A,1) ->
  A;
power(A,N) when N rem 2 =:= 1 ->
  A * power(A, N-1);
power(A,N) ->
  P2 = power(A, N div 2),
  P2 * P2.

power_sums(AMax, NMax) ->
  lists:sort(
    [
     {Power, A, N, Sum} ||
     A <- lists:seq(2, AMax),
     N <- lists:seq(1, NMax),
     Power <- [power(A,N)],
     Sum <- [sum_digits(Power)],
     Power >= 10,
     Sum =:= A
    ]).

sum_digits(N) when is_integer(N) ->
  sum_digits(integer_to_list(N));
sum_digits(N) when is_list(N) ->
  lists:sum(lists:map(fun(D) -> list_to_integer([D]) end, N)).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_power_test_() ->
  [
   ?_assertNot(is_power(4, 2)),
   ?_assertNot(is_power(3, 5)),
   ?_assertNot(is_power(3, 8)),
   ?_assertEqual(2, is_power(2, 4)),
   ?_assertEqual(3, is_power(2, 8)),
   ?_assertEqual(4, is_power(2, 16)),
   ?_assertEqual(5, is_power(2, 32)),
   ?_assertEqual(6, is_power(2, 64))
  ].

power_test_() ->
  [
   ?_assertEqual([1,2,4,8,16,32,64,128,256], 
                 [power(2,I) || I <- lists:seq(0,8)])
  ].

sum_digits_test_() ->
  [
   ?_assertEqual(
      [0,1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9,10,2],
      [sum_digits(I) || I <- lists:seq(0, 20)]),
   ?_assertEqual(45, sum_digits("123456789"))
  ].

-endif.
