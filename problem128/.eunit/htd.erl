
-module(htd).

-export(
   [
    is_prime/1,
    main/0,
    pd/2,
    pd3/1,
    ring_around_first/1,
    ring_around_last/1,
    ring_initial/1,
    ring_length/1,
    ring_max/1
   ]
  ).

is_prime(1) ->
  false;
is_prime(N) when N =< 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N).

is_prime(I, N) when I*I > N ->
  true;
is_prime(I, N) when N rem I =:= 0; N rem (I+2) =:= 0 ->
  false;
is_prime(I, N) ->
  is_prime(I+6, N).

main() ->
  PD3 = 
  hd(lists:reverse([N || {N,_} <- pd3(2000)])),
  io:format("~p~n", [PD3]),
  ok.

pd(N, Around) ->
  length(
    [P ||
     P <-
     lists:map(
       fun(A) ->
           abs(N - A)
       end,
       Around),
     is_prime(P)]).

pd3(Nth) ->
  pd3(2, Nth, [{1, ring_around_first(1)}], []).

pd3(_Ring, Nth, _RingAround, Ack) when Nth =:= length(Ack) ->
  lists:reverse(Ack);
pd3(Ring, Nth, [], Ack) ->
  First = ring_initial(Ring),
  Last = ring_max(Ring),
  pd3(Ring+1, 
      Nth, 
      [
       {First, ring_around_first(Ring)}, 
       {Last, ring_around_last(Ring)}
      ],
      Ack);
pd3(Ring, Nth, [{I, Around} | T], Ack) ->
  case pd(I, Around) =:= 3 of
    true ->
      pd3(Ring, Nth, T, [{I, Around} | Ack]);
    false ->
      pd3(Ring, Nth, T, Ack)
  end.

ring_around_first(1) ->
  lists:seq(2,7);
ring_around_first(Ring) when Ring >= 2 ->
  lists:sort(
    [ring_initial(Ring - 1),
     ring_initial(Ring) + 1,
     ring_max(Ring),
     ring_initial(Ring + 1),
     ring_initial(Ring + 1) + 1,
     ring_max(Ring+1)]).

ring_around_last(2) ->
  Ring3Max = ring_max(3),
  lists:sort(
    [1,
     ring_initial(2), 
     ring_max(2) - 1, 
     Ring3Max, 
     Ring3Max - 1, 
     Ring3Max - 2]);
ring_around_last(Ring) when Ring >= 3 ->
  RingMax = ring_max(Ring + 1),
  lists:sort(
    [ring_initial(Ring - 1), 
     ring_max(Ring - 1), 
     ring_initial(Ring), 
     ring_max(Ring) - 1, 
     RingMax, 
     RingMax - 1]).

ring_initial(1) ->
  1;
ring_initial(Ring) ->
  3 * (Ring-1) * (Ring-2) + 2.

ring_length(1) ->
  1;
ring_length(Ring) ->
  6 * (Ring-1).

ring_max(Ring) ->
  ring_initial(Ring) + ring_length(Ring) - 1.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
  [
   ?_assertEqual(
      [2,3,5,7,11,13,17,19,23,29], 
      [I || I <- lists:seq(1,30), is_prime(I)])
  ].

pd_test_() ->
  [
   ?_assertEqual(3, pd(1, [2,3,4,5,6,7])),
   ?_assertEqual(3, pd(8, [2,9,19,20,21,37])),
   ?_assertEqual(2, pd(17, [6,7,16,18,33,34]))
  ].

pd3_test_() ->
  PD = [N || {N,_} <- pd3(10)],
  [
   ?_assertEqual([1,2,8,19,20,37,61,128,217,271], PD)
  ].

ring_around_first_test_() ->
  [
   ?_assertEqual([2,3,4,5,6,7], ring_around_first(1)),
   ?_assertEqual([1,3,7,8,9,19], ring_around_first(2)),
   ?_assertEqual([2,9,19,20,21,37], ring_around_first(3)),
   ?_assertEqual([8,21,37,38,39,61], ring_around_first(4)),
   ?_assertEqual([20,39,61,62,63,91], ring_around_first(5))
  ].

ring_around_last_test_() ->
  [
   ?_assertEqual([1,2,6,17,18,19], ring_around_last(2)),
   ?_assertEqual([2,7,8,18,36,37], ring_around_last(3)),
   ?_assertEqual([8,19,20,36,60,61], ring_around_last(4)),
   ?_assertEqual([20,37,38,60,90,91], ring_around_last(5)),
   ?_assertEqual([38,61,62,90,126,127], ring_around_last(6))
  ].

ring_initial_test_() ->
  [
   ?_assertEqual([1,2,8,20,38,62,92], [ring_initial(I) || I <- lists:seq(1,7)])
  ].

ring_length_test_() ->
  [
   ?_assertEqual([1,6,12,18,24,30,36], [ring_length(I) || I <- lists:seq(1,7)])
  ].

ring_max_test_() ->
  [
   ?_assertEqual([1,7,19,37,61,91,127], [ring_max(I) || I <- lists:seq(1,7)])
  ].

-endif.
