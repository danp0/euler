-module(sr).

-export(
   [
    main/0,
    power/2,
    remainder/1,
    remainder/2
   ]
  ).

main() ->
  Max =
  [lists:max(remainder(A)) || A <- lists:seq(3,1000)],
  io:format("~p~n", [lists:sum(Max)]),
  ok.

power(_A, 0) ->
  1;
power(A, 1) ->
  A;
power(A, N) when N rem 2 =:= 1 ->
  A * power(A, N-1);
power(A, N) ->
  A2 = power(A, N div 2),
  A2 * A2.

remainder(A) ->
  remainder(A, 2, [remainder(A,1)]).

remainder(A, N, Ack) ->
  {L1, L2} = lists:split(length(Ack) div 2, Ack),
  case L1 =:= L2 of
    true ->
      lists:reverse(L1);
    false ->
      remainder(A, N+1, [remainder(A, N) | Ack])
  end.

remainder(A, N) ->
  Power = power(A-1, N) + power(A+1, N),
  Power rem (A*A).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

power_test_() ->
  [
   ?_assertEqual(
      [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024], 
      [power(2, N) || N <- lists:seq(0, 10)])
  ].

-endif.

