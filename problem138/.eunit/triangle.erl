-module(triangle).

-export(
   [
    gcd/2,
    main/0
   ]
  ).

%%
%%   |\
%%   | \
%%  h|  \l
%%   |   \
%%   |____\
%%      b
%%
%% h = 2b+1
%% h = 2b-1
%%
%% l^2 = (2b+1)^2 + b^2 = 5b^2 + 4b + 1
%% l^2 = (2b-1)^2 + b^2 = 5b^2 - 4b + 1
%%
%% a = k(m^2 - n^2) 
%% b = k(2mn) 
%% c = k(m^2 + n^2)
%% m,n,k are positive integers
%% m and n are coprime
%% m - n is odd
%%
%% 2mn = 2m^2 - 2n^2 + 1
%% 2mn = 2m^2 - 2n^2 - 1
%%

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

main() ->
  io:format("triangle...~n", []),
  ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1,0)),
   ?_assertEqual(1, gcd(2,1)),
   ?_assertEqual(3, gcd(9,3)),
   ?_assertEqual(8, gcd(16,24)),
   ?_assertEqual(9, gcd(54,63))
  ].

-endif.
