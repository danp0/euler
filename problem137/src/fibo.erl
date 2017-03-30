-module(fibo).

-export(
   [
    isqrt/1,
    main/0,
    x/1,
    xfibo/1
   ]
  ).

%%
%% Af(x) = x/(1-x-x^2)
%% n = x/(1-x-x^2)
%% n - nx - nx^2 = x
%% nx^2 + (n+1)x - n = 0
%%
%% a = n
%% b = n+1
%% c = -n
%%
%% x = -(n+1) +- ((n+1)^2 + 4n^2)^1/2 / 2n
%% x = -(n+1) +- (n^2 + 2n + 1 + 4n^2)^1/2 / 2n
%% x = -(n+1) +- (5n^2 + 2n + 1)^1/2 / 2n
%%
%% n   x
%% 1   -2 + 2(2)^1/2 / 2 = 2^1/2 - 1
%% 2   -3 + (20 + 4 + 1)^1/2 / 4 = -3 +- 5 / 4 = 1/2
%% 3   -4 + (45 + 6 + 1)^1/2 / 6 = -4 + 2(13)^1/2 / 6 = 13^1/2 - 2 / 3
%%
%% 1,1,2,3,5,8,13,21,34,55,89
%% x: [
%% 2,       2
%% 15,      3,5
%% 104,     8,13
%% 714,     21,34 
%% 4895,    55,89
%% 33552,
%% 229970,
%% 1576239,
%% 10803704,
%% 74049690
%% ]

isqrt(X) ->
  Sqrt = math:sqrt(X),
  ISqrt = trunc(Sqrt),
  case X =:= ISqrt*ISqrt of
    true ->
      ISqrt;
    false ->
      false
  end.

main() ->
  io:format("x: ~p~n", [xfibo(15)]),
  ok.

x(Upto) ->
  x(Upto, 1, []).

x(0, _N, Ack) ->
  lists:reverse(Ack);
x(Upto, N, Ack) ->
  case isqrt(5*N*N + 2*N + 1) of
    false ->
      x(Upto, N+1, Ack);
    _Sqrt ->
      x(Upto - 1, N+1, [N | Ack])
  end.

xfibo(Upto) ->
  xfibo(Upto, 1,2, [2]).

xfibo(1, _F1, _F2, Ack) ->
  lists:reverse(Ack);
xfibo(Upto, F1, F2, Ack) ->
  F3 = F1 + F2,
  F4 = F2 + F3,
  xfibo(Upto - 1, F3, F4, [F3*F4|Ack]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

xfibo_test_() ->
  [
   ?_assertEqual(x(5), xfibo(5))
  ].

-endif.
