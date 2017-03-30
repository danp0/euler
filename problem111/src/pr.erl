-module(pr).

-export(
   [
    find/1,
    is_prime/1,
    main/0,
    primes/2,
    runs/3
   ]
  ).

find(N) ->
  [
   {D, M, length(Primes), lists:sum(Primes)}
   ||
   {D, {M, Primes}} <- [{D, primes(N,D)} || D <- lists:seq(0,9)]
  ].

is_prime(N) when N < 2 ->
  false;
is_prime(N) when N =:= 2; N =:= 3 ->
  true;
is_prime(N) when N rem 2 =:= 0; N rem 3 =:= 0 ->
  false;
is_prime(N) ->
  is_prime(5, N, trunc(math:sqrt(N)) + 1).

is_prime(I, _N, SqrtN) when I > SqrtN ->
  true;
is_prime(I, N, _SqrtN) when N rem I =:= 0 ->
  false;
is_prime(I, N, SqrtN) ->
  is_prime(I + 1, N, SqrtN).

main()->
  Primes = find(10),
  Sum = lists:sum([S || {_,_,_,S} <- Primes]),
  io:format("~p~n~p~n", [Primes, Sum]),
  ok.

primes(N, D) ->
  primes(N, N-1, D).

primes(N, 0, D) ->
  {0, lists:filter(fun is_prime/1, runs(N, 0, D))};
primes(N, R, D) ->
  Primes =
  lists:filter(fun is_prime/1, runs(N, R, D)),
  case Primes of
    [] ->
      primes(N, R-1, D);
    _ ->
      {R, Primes}
  end.

runs(N, R, 0) when R =:= N-2 ->
  Repeat = lists:duplicate(R, $0),
  [
   list_to_integer(lists:flatten([[Head], Repeat, [Tail]]))
   || 
   Head <- "123456789", Tail <- "13579"
  ];
runs(N, R, D) when R =:= N-1 ->
  Repeat = lists:duplicate(R, hd(integer_to_list(D))),
  lists:usort(
    [
     list_to_integer(E)
     ||
     E <-
     [
      lists:flatten([Head, Middle, Tail])
      ||
      {Head, Tail} <- [lists:split(I, Repeat) || I <- lists:seq(0,R)],
      Middle <- "0123456789"
     ],
     hd(E) =/= $0
    ]
   );
runs(N, R, D) when R =:= N-2 ->
  Repeat = lists:duplicate(R, hd(integer_to_list(D))),
  lists:usort(
    [
     list_to_integer(E)
     ||
     E <-
     [
      lists:flatten([Head1, Middle1, Tail1, Middle2, Tail])
      ||
      {Head1, Tail1, Tail} <-
      [
       {Head1, Tail1, Tail} ||
       {Head, Tail} <- [lists:split(I, Repeat) || I <- lists:seq(0,R)],
       {Head1, Tail1} <- [lists:split(I, Head) || I <- lists:seq(0,length(Head))]
      ],
      Middle1 <- "0123456789",
      Middle2 <- "0123456789"
     ],
     hd(E) =/= $0
    ]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

find_test_() ->
  [
   ?_assertEqual(
      [
       {0, 2, 13, 67061},
       {1, 3,  9, 22275},
       {2, 3,  1,  2221},
       {3, 3, 12, 46214},
       {4, 3,  2,  8888},
       {5, 3,  1,  5557},
       {6, 3,  1,  6661},
       {7, 3,  9, 57863},
       {8, 3,  1,  8887},
       {9, 3,  7, 48073}
      ], 
      find(4))
  ].

is_prime_test_() ->
  ?_assertEqual([2,3,5,7,11,13,17,19], 
                lists:filter(fun is_prime/1, lists:seq(1,20))).

primes_test_() ->
  Test =
  fun(N, D) ->
      {M, Primes} = primes(N, D),
      {M, length(Primes), lists:sum(Primes)}
  end,
  [
   ?_assertEqual({2,13,67061}, Test(4, 0)),
   ?_assertEqual({3, 9, 22275}, Test(4, 1)),
   ?_assertEqual({3, 1, 2221}, Test(4, 2)),
   ?_assertEqual({3, 12, 46214}, Test(4, 3)),
   ?_assertEqual({3, 2, 8888}, Test(4, 4)),
   ?_assertEqual({3, 1, 5557}, Test(4, 5)),
   ?_assertEqual({3, 1, 6661}, Test(4, 6)),
   ?_assertEqual({3, 9, 57863}, Test(4, 7)),
   ?_assertEqual({3, 1, 8887}, Test(4, 8)),
   ?_assertEqual({3, 7, 48073}, Test(4, 9))
  ].

runs_test_() ->
  [
   ?_assertEqual([10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 31, 41, 51, 61, 71, 81, 91], runs(2, 1, 1))
  ].

-endif.

