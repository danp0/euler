-module(psr).

-export(
   [
    is_prime/1,
    main/0,
    pow/2,
    remainder/2,
    remainder_1/2,
    r_gt/2
   ]
  ).

%%
%% prime square remainders...
%%
%% n   pn  r
%%
%% 1:   2  0
%% 2:   3  2
%% 3:   5  5
%% 4:   7  2
%% 5:  11  110 = 2*5*11
%% 6:  13  2
%% 7:  17  238 = 2*7*17
%% 8:  19  2
%% 9:  23  414 = 2*9*23
%% 10: 29  2
%% 11: 31  682 = 2*11*31
%% 12: 37  2
%% 13: 41  1066 = 2*13*41
%% 14: 43  2
%% 15: 47  1410 = 2*15*47 
%% 16: 53  2
%% 17: 59  2006 = 2*17*59
%% 18: 61  2
%% 19: 67  2546 = 2*19*67
%%

is_prime(N) when N =< 1 ->
  false;
is_prime(N) when N =< 3 ->
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

main() ->
  io:format("~p~n", [r_gt(10000000000, fun remainder/2)]),
  ok.

next_prime(2) ->
  np(3);
next_prime(P) ->
  np(P+2).

np(P) ->
  case is_prime(P) of
    true ->
      P;
    false ->
      np(P+2)
  end.

pow(_A, 0) ->
  1;
pow(A, 1) ->
  A;
pow(A, N) when N rem 2 =:= 1 ->
  A * pow(A, N-1);
pow(A, N) ->
  A2 = pow(A, N div 2),
  A2 * A2.

remainder(1, 2) ->
  0;
remainder(3, 5) ->
  5;
remainder(I, _P) when I rem 2 =:= 0 ->
  2;
remainder(I, P) ->
  2 * I * P.

remainder_1(I, P) ->
  (pow(P-1, I) + pow(P+1, I)) rem (P*P).

r_gt(Boundary, Remainder) ->
  r_gt(1, 2, Boundary, Remainder).

r_gt(I, P, Boundary, Remainder) ->
  case Remainder(I, P) > Boundary of
    true ->
      I;
    false ->
      r_gt(I+1, next_prime(P), Boundary, Remainder)
  end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
  [
   ?_assertEqual(
      [2,3,5,7,11,13,17,19],
      [I || I <- lists:seq(1,20), is_prime(I)])
  ].

pow_test_() ->
  [
   ?_assertEqual(
      [1,2,4,8,16,32,64,128,256,512,1024],
      [pow(2,I) || I <- lists:seq(0,10)])
  ].

r_gt_test_() ->
  [
   ?_assertEqual(r_gt(1,      fun remainder_1/2), 
                 r_gt(1,      fun remainder/2)),
   ?_assertEqual(r_gt(100,    fun remainder_1/2), 
                 r_gt(100,    fun remainder/2)),
   ?_assertEqual(r_gt(1000,   fun remainder_1/2), 
                 r_gt(1000,   fun remainder/2)),
   ?_assertEqual(r_gt(10000,  fun remainder_1/2), 
                 r_gt(10000,  fun remainder/2)),
   ?_assertEqual(r_gt(100000, fun remainder_1/2), 
                 r_gt(100000, fun remainder/2))
  ].

-endif.
