-module(triangle).

-export(
   [
    gcd/2,
    main/0,
    sit/1
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
%% m > n
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
  Sit = sit(12),
  io:format("sit: ~p~n~p~n", [Sit, lists:sum(Sit)]),
  ok.

sit(Count) ->
  sit(Count, 2, 1, []).

sit(0, _M, _N, Ack) ->
  lists:reverse(Ack);
sit(Count, M, N, Ack) when M =:= N ->
  sit(Count, M+1, 1, Ack);
sit(Count, M, N, Ack) ->
  case (M-N) rem 2 =:= 1 andalso gcd(M, N) =:= 1 of
    true ->
      M2 = M*M,
      N2 = N*N,
      [A,B,C] = lists:sort([M2 - N2, 2*M*N, M2 + N2]),
      case abs(2*A-B) =:= 1 of
        true ->
          sit(Count-1, M+1, M, [C | Ack]);
        false ->
          sit(Count, M+1, N, Ack)
      end;
    false ->
      sit(Count, M+1, N, Ack)
  end.

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
