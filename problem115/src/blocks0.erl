-module(blocks0).

-export(
   [
    c/2,
    f/2,
    f1/2,
    main/0,
    pow/2,
    to_binary/2
   ]
  ).

%% b1 = 7             1 0      C(6,0)                   = 1
%% r1 = 7               1      C(6,0)                   = 1
%% b1 + r2 = 7        2 01     C(7-2-1, 2-1) = C(4,1)   = 4
%% r1 + b2 = 7          10     C(7-2-1, 2-1) = C(4,1)   = 4
%% b1 + r2 + b3 = 7   3 010    C(7-2-1, 3-1) = C(4,2)   = 6
%% r1 + b2 + r3 = 7     101    C(7-2-2-1, 3-1) = C(2,2) = 1
%%                    4 0101
%%                      1010
%%                    5 01010
%%                      10101
%% 
%% C(7,0) = 1
%% C(7,0) = 1
%% C(5,1) = 5
%% C(5,1) = 5
%% C(5,2) = 10
%% C(3,2) = 3
%% C(3,3) = 1          0101
%% C(3,3) = 1          1010
%%
%% C(N-1,0) + C(N-M+1-1,0) +               0 1
%% C(N-M+1-1,1) + C(N-M+1-1,1) +           01 10
%% C(N-M+1-1,2) + C(N-M+1-M+1-1,2) +       010 101
%% C(N-M+1-M+1-1,3) + C(N-M+1-M+1-1,3)     0101 1010
%% C(N-M+1-M+1-1,4) + C(N-M+1-M+1-M+1-1,4) 01010 10101
%%
%% C(N-1,0) + C(N-M,0) +     0 1
%% C(N-M,1) + C(N-M,1) +     01 10
%% C(N-M,2) + C(N-2M+1,2) +  010 101
%% C(N-2M+1,3) + C(N-2M+1,3) 0101 1010
%% C(N-2M+1,4) + C(N-3M+2,4) 01010 10101
%%
%% bi > 0 and ri > M-1
%% c(N - a1 - a2 ... - ai - 1, i - 1)
%%
%% M = 3
%% 
%% blocks...
%% 1: 1
%% 2: 1
%% 3: 2
%% 4: 4
%% 5: 7
%% 6: 11
%% 7: 17
%% 8: 27
%% 9: 44
%% 10: 72
%% 11: 117
%% 12: 189
%% 13: 305
%% 14: 493
%% 15: 798
%% 16: 1292
%% 17: 2091
%% 18: 3383
%% 19: 5473
%% 20: 8855
%% 
%% 
%% M = 4
%% 
%% blocks...
%% 1: 1
%% 2: 1
%% 3: 1
%% 4: 2
%% 5: 4      1 +  2 + 1
%% 6: 7      2 +  4 + 1
%% 7: 11     4 +  7
%% 8: 16     7 + 11 - 2
%% 9: 23    11 + 16 - 4
%% 10: 34   16 + 23 - 5
%% 11: 52   23 + 34 - 5
%% 12: 81   34 + 52 - 5
%% 13: 126  52 + 81 - 7
%% 14: 194  81 + 126 - 13
%% 15: 296
%% 16: 450
%% 17: 685
%% 18: 1046
%% 19: 1601
%% 20: 2452
%% 
%% M = 5
%%
%% blocks...
%% 1: 1
%% 2: 1
%% 3: 1
%% 4: 1
%% 5: 2
%% 6: 4
%% 7: 7
%% 8: 11
%% 9: 16
%% 10: 22
%% 11: 30
%% 12: 42
%% 13: 61
%% 14: 91
%% 15: 137
%% 16: 205
%% 17: 303
%% 18: 443
%% 19: 644
%% 20: 936

c(_N,0) ->
  1;
c(N,N) ->
  1;
c(N,K) when N < K ->
  0;
c(N,K) ->
  c(N-1,K-1) + c(N-1,K).

%% M: minimum block width
%% N: row length
f(M, N) ->
  f(M, N, 1, 0).

f(M, N, I, Ack) ->
  C1 = c(N - (I div 2)*(M-1)  - 1, I-1),
  C2 = c(N - ((I div 2) + (I rem 2))*(M-1) - 1, I-1),
  case C1 + C2 of
    0 ->
      Ack;
    Sum ->
      f(M, N, I+1, Ack + Sum)
  end.

%% M: minimum block width
%% N: row length
f1(M, N) ->
  f1(0, pow(2, N) - 1, M, N, 0).

f1(From, To, _M, _N, Total) when From > To ->
  Total;
f1(From, To, M, N, Total) ->
  B = get_blocks(to_binary(From, N)),
  Ok =
  case (B =:= [] orelse length(hd(B)) >= M) of
    true ->
      1;
    false ->
      0
  end,
  f1(From+1, To, M, N, Total + Ok).

get_blocks(Binary) ->
  Blocks =
  lists:sort(re:split(Binary, "1+", [{return, list}, trim])),
  case Blocks of
    [[]|T] ->
      T;
    Blocks ->
      Blocks
  end.

main() ->
  io:format("blocks...~n", []),
  lists:foreach(
    fun(N) ->
        io:format("~p: ~p ~p~n", [N, f(5, N), f1(5, N)])
    end,
    lists:seq(1, 10)),
  io:format("~p~n", [f(10,57)]),
  ok.

pow(_A, 0) ->
  1;
pow(A, 1) ->
  A;
pow(A, B) when B rem 2 =:= 1 ->
  A * pow(A, B-1);
pow(A, B) ->
  Power = pow(A, B div 2),
  Power * Power.

to_binary(I, Width) ->
  lists:flatten(io_lib:format("~*.2.0B", [Width, I])).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

c_test_() ->
  [
   ?_assertEqual(0, c(1,2)),
   ?_assertEqual(1, c(1,0)),
   ?_assertEqual(1, c(1,1)),
   ?_assertEqual(3, c(3,2)),
   ?_assertEqual(15, c(6,2)),
   ?_assertEqual(20, c(6,3))
  ].

pow_test_() ->
  [
   ?_assertEqual(
      [1, 2, 4, 8, 16, 32, 64, 128, 256],
      [pow(2,N) || N <- lists:seq(0,8)])
  ].

to_binary_test_() ->
  [
   ?_assertEqual(
      ["000", "001", "010", "011", "100", "101", "110", "111"],
      [to_binary(I, 3) || I <- lists:seq(0,7)])
  ].

-endif.
