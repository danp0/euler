-module(pair).

-export(
   [
    find/1,
    magnitude/1,
    main/0,
    sieve/1
   ]
  ).

%%
%% p1, p2
%% n*10^m + p1 rem p2 == 0
%%
%% p1 = 19, p2 = 23
%%
%% 12*100 + 19 rem 23 == 0
%% 
%% 1219 = 23*53
%%

find(Upto) ->
  Primes =
  lists:dropwhile(fun(E) -> E < 5 end, sieve(2*Upto)),
  find(Upto, Primes, []).

find(_Upto, [], Ack) ->
  lists:reverse(Ack);
find(Upto, [_P], Ack) ->
  find(Upto, [], Ack);
find(Upto, [P1, P2 | T], Ack) ->
  case P1 > Upto of
    true ->
      find(Upto, [], Ack);
    false ->
      find(Upto, [P2 | T], [find(1, P1, P2, magnitude(P1)) | Ack])
  end.

find(N, P1, P2, Magnitude) ->
  S = N * Magnitude + P1,
  case (S rem P2) =:= 0 of
    true ->
      S;
    false ->
      find(N+1, P1, P2, Magnitude)
  end.

magnitude(N) ->
  magnitude(N, 10).

magnitude(N, Ack) when N div Ack =:= 0 ->
  Ack;
magnitude(N, Ack) ->
  magnitude(N, 10*Ack).

main() ->
  io:format("pair...~n", []),
  S = find(1000000),
  io:format("~p~n", [S]),
  io:format("Sum: ~p~n", [lists:sum(S)]),
  ok.

sieve(N) ->
  Primes =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{default, true}, {fixed, true}]))),
  sieve(2, N, Primes).

sieve(I, N, Primes) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun(P, true, Acc) ->
          [P | Acc];
         (_P, false, Acc) ->
          Acc
      end,
      [],
      Primes));
sieve(I, N, Primes) ->
  Next =
  case array:get(I, Primes) of
    true ->
      lists:foldl(
        fun(J, P) ->
            array:set(J, false, P)
        end,
        Primes,
        lists:seq(I*I, N, I));
    false ->
      Primes
  end,
  sieve(I+1, N, Next).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

magnitude_test_() ->
  [
   ?_assertEqual(10, magnitude(0)),
   ?_assertEqual(10, magnitude(1)),
   ?_assertEqual(10, magnitude(9)),
   ?_assertEqual(100, magnitude(10)),
   ?_assertEqual(100, magnitude(99))
  ].

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
