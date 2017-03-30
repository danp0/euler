-module(exponentiation1).

-export(
   [
    factor/1,
    is_prime/1,
    m/1,
    m1/1,
    m2/1,
    m3/1,
    main/0,
    powers/1
   ]
  ).

%%
%% m(1)  = 0  n = n
%%
%% m(2)  = 1  n x n = n^2
%%
%% m(3)  = 2  n x n, n^2 x n = n^3
%%
%% m(4)  = 2  n x n = n^2, n^2 x n^2 = n^4 
%%
%% m(5)  = 3  n x n = n^2, n^2 x n^2 = n^4, n^4 x n = n^5
%% m(6)  = 3  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^2 = n^6
%% m(7)  = 4  n x n = n^2, n^2 x n = n^3, n^3 x n^3 = n^6, n^6 x n^1 = n^7

%% m(8)  = 3  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8

%% m(9)  = 4  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8, n^8 x n = n^9
%% m(10) = 4  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8, n^8 x n^2 = n^10
%% m(11) = 5  n x n = n^2, n^2 x n = n^3, n^3 x n^3 = n^6, n^6 x n^3 = n^9, n^9 x n^2 = n^11
%% m(12) = 4  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8, n^8 x n^4 = n^12
%% m(13) = 5  n x n = n^2, n^2 x n = n^3, n^3 x n^3 = n^6, n^6 x n^3 = n^9, n^9 x n^3 = n^12
%% m(14) = 5  n x n = n^2, n^2 x n = n^3, n^3 x n^3 = n^6, n^6 x n^6 = n^12, n^12 x n^2 = n^14
%% m(15) = 5  n x n = n^2, n^2 x n = n^3, n^3 x n^3 = n^6, n^6 x n^6 = n^12, n^12 x n^3 = n^15

%% m(16) = 4  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8, n^8 x n^8 = n^16
%% m(17) = 5  n x n = n^2, n^2 x n^2 = n^4, n^4 x n^4 = n^8, n^8 x n^8 = n^16, n^16 x n = n^17
%% m(18) = 5  2, 4, 8, 16, 17
%% m(19) = 6  2, 3, 6, 12, 18, 19
%% m(20) = 5  2, 4, 8, 16, 20
%% m(21) = 6  2, 4, 8, 16, 20, 21
%% m(22) = 6  2, 4, 8, 16, 20, 22
%% m(23) = 7  2, 4, 8, 16, 20, 22, 23
%% m(24) = 5  2, 4, 8, 16, 24
%% m(25) = 6  2, 4, 5, 10, 20, 25
%% m(26) = 6  2, 4, 8, 16, 24, 26
%% m(27) = 6  2, 3, 6, 12, 24, 27
%%
%% m(77) = 9  2, 4, 8, 16, 32, 64, 72, 76, 77
%%            2, 3, 6, 9, 11, 22, 44, 66, 77
%%
%% m(100) = 8  2, 4, 5, 10, 20, 40, 80, 100
%%             2, 4, 8, 16, 32, 64, 96, 100
%%
%% m(nxm) = m(n) + m(m)
%%
%% [1, 2, 4, 8, 16, 32, 64, 128]
%% [1, 2, 3, 6, 12, 24, 48,  96, 192]
%% [1, 2, 4, 5, 10, 20, 40,  80, 160]
%% [1, 2, 4, 8,  9, 18, 36,  72, 144]
%% [1, 2, 4, 8, 16, 17, 34,  64, 128]
%% [1, 2, 4, 8, 16, 32, 33,  66, 172]
%% [1, 2, 4, 8, 16, 32, 64,  65, 130]
%% [1, 2, 3, 6,  7, 14, 28,  56, 112]
%% [1, 2, 3, 6, 12, 13, 26,  52, 104]
%% 

factor(N) ->
  Primes = [I || I <- lists:seq(2, trunc(math:sqrt(N)) + 1), is_prime(I)],
  factor(N, Primes, []).

factor(N, [], Ack) when N =:= 1 ->
  lists:reverse(Ack);
factor(N, [], Ack) ->
  factor(1, [], [N | Ack]);
factor(N, [P|T] = Primes, Ack) ->
  case N rem P =:= 0 of
    true ->
      factor(N div P, Primes, [P|Ack]);
    false ->
      factor(N, T, Ack)
  end.

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I * I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

m(1) ->
  0;
m(2) ->
  1;
m(3) ->
  2;
m(N) ->
  case factor(N) of
    [N] ->
      1 + m(N-1);
    Factors ->
      lists:sum([m(F) || F <- Factors])
  end.

m1(1) ->
  0;
m1(N) when N rem 2 =:= 1 ->
  1 + m1(N-1);
m1(N) ->
  1 + m1(N div 2).

m2(N) ->
  m2(1, N, [1], 0).

m2(Exp, N, _Exponents, Ack) when Exp =:= N ->
  Ack;
m2(Exp, N, Exponents, Ack) when 2*Exp > N ->
  case length([ N || E <- Exponents, Exp+E =:= N]) =:= 0 of
   false ->
      m2(N, N, Exponents, Ack + 1);
   true -> 
      {Exp, N, Exponents, Ack+1}
  end;
m2(Exp, N, Exponents, Ack) ->
  Exp2 = 2 * Exp,
  m2(Exp2, N, [Exp2 | Exponents], Ack + 1). 

m3(N) ->
  %%  Powers = powers(200),
  Powers =
  [
   [1, 2, 4, 8, 16, 32, 64, 128],
   [1, 2, 3, 6, 12, 24, 48,  96, 192],
   [1, 2, 4, 5, 10, 20, 40,  80, 160],
   [1, 2, 4, 8,  9, 18, 36,  72, 144],
   [1, 2, 4, 8, 16, 17, 34,  68, 136],
   [1, 2, 4, 8, 16, 32, 33,  66, 172],
   [1, 2, 4, 8, 16, 32, 64,  65, 130],
   [1, 2, 3, 6,  7, 14, 28,  56, 112],
   [1, 2, 3, 6, 12, 13, 26,  52, 104],
   [1, 2, 3, 6, 12, 24, 25,  50, 100, 200],
   [1, 2, 3, 6, 12, 24, 48,  49, 198],
   [1, 2, 3, 5,  6, 12, 24,  48,  96, 192],
   [1, 2, 3, 5, 10, 11, 22,  44,  88, 176],
   [1, 2, 3, 5, 10, 20, 21,  42,  84, 168],
   [1, 2, 4, 5, 10, 20, 40,  41,  82, 164],
   [1, 2, 3, 5, 10, 20, 40,  80,  81, 162],
   [1, 2, 4, 8,  9, 10, 20,  40,  80, 160],
   [1, 2, 4, 8,  9, 18, 19,  38,  76, 152],
   [1, 2, 4, 8,  9, 18, 36,  37,  74, 148],
   [1, 2, 4, 8,  9, 18, 36,  72,  73, 146],
   [1, 2, 4, 8, 16, 17, 18,  36,  72, 144],
   [1, 2, 4, 8, 16, 17, 34,  35,  70, 140],
   [1, 2, 4, 8, 16, 17, 34,  68,  69, 138],
   [1, 2, 4, 8, 16, 17, 34,  68, 136, 137],
   [1, 2, 4, 8, 16, 32, 33,  34,  35,  70, 140],
   [1, 2, 4, 8, 16, 32, 33,  66,  67, 134],
   [1, 2, 4, 8, 16, 32, 33,  66, 132, 133],
   [1, 2, 3, 5, 10, 20, 23,  46,  92, 184],
   [1, 2, 3, 6, 12, 13, 26,  29,  58, 116],
   [1, 2, 3, 6,  7, 14, 28,  31,  62, 124],
   [1, 2, 4, 8, 16, 32, 33,  37,  74, 148],
   [1, 2, 3, 6, 12, 13, 26,  39,  78, 156],
   [1, 2, 3, 5, 10, 20, 40,  43,  86, 192],
   [1, 2, 3, 5, 10, 20, 21,  42,  47,  94, 188],
   [1, 2, 4, 5, 10, 20, 40,  41,  51, 102],
   [1, 2, 4, 8, 16, 17, 34,  51, 102],
   [1, 2, 3, 6, 12, 24, 25,  50,  53, 106],
   [1, 2, 3, 5, 10, 20, 21,  42,  47,  57, 114],
   [1, 2, 3, 6, 12, 13, 26,  29,  58,  59, 118],
   [1, 2, 4, 5, 10, 20, 40,  41,  61, 122]
  ],
  m3(N, Powers, 200).

m3(_N, [], Ack) ->
  Ack;
m3(N, [H | T], Ack) ->
  L = lists:takewhile(fun(E) -> E =< N end, H), 
  m3(
    N, 
    T, 
    min(Ack, m3_1(N, L, length(L) - 2))).

m3_1(0, _L, Ack) ->
  Ack;
m3_1(N, L, _Ack) when N < 0; length(L) =:= 0 ->
  200;
m3_1(N, [H|T], Ack) ->
  min(m3_1(N - H, T, Ack+1), m3_1(N, T, Ack)).

main() ->
  io:format("exponentation...~n", []),
  io:format("~p~n", [lists:sum([m(I)  || I <- lists:seq(1,200)])]),
  io:format("~p~n", [lists:sum([m1(I) || I <- lists:seq(1,200)])]),
  io:format("~p~n", [lists:sum([m3(I) || I <- lists:seq(1,200)])]),
  %io:format("~p~n", [[m2(I) || I <- lists:seq(1,200)]]),
  %io:format("~p~n", [powers(200)]),
  ok.

powers(Upto) ->
  powers(Upto, [1], []).

powers(Upto, [H|_T], Ack) when H > Upto ->
  lists:reverse(Ack);
powers(Upto, [H|_T] = Seq, Ack) ->
  powers(Upto, [2*H | Seq], [powers1(H+1, Upto, Seq) | Ack]).

powers1(H, Upto, Seq) when H > Upto ->
  lists:reverse(Seq);
powers1(H, Upto, Seq) ->
  powers1(2*H, Upto, [H | Seq]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

factor_test_() ->
  [
   ?_assertEqual([2], factor(2)),
   ?_assertEqual([3], factor(3)),
   ?_assertEqual([2,2], factor(4)),
   ?_assertEqual([5], factor(5)),
   ?_assertEqual([2,3], factor(6)),
   ?_assertEqual([7], factor(7)),
   ?_assertEqual([2,2,2], factor(8)),
   ?_assertEqual([2,2,5], factor(20))
  ].

is_prime_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19],
                 [I || I <- lists:seq(0, 20), is_prime(I)])
  ].

m_test_() ->
  [
   ?_assertEqual(
      [0,1,2,2,3,3,4,3,4,4,5,4,5,5,5,4,5,5,6,5,6,6,7,5,6,6,6,9,8],
      [m(I) || I <- lists:seq(1,27) ++ [77, 100]])
  ].

-endif.
