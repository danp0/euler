-module(repunit1).

-export(
   [
    a/1,
    a1/1,
    ceil/1,
    fermat_factor/1,
    find/1,
    gcd/2,
    main/0,
    r/1,
    rdiv/2,
    sieve/1
   ]
  ).

-on_load(init/0).

-ifdef(TEST).

-define(LIBREPUNIT, "../priv/librepunit").

-else.

-define(LIBREPUNIT, "./priv/librepunit").

-endif.

%%
%% 10^n - 1 = 9(10^n-1 + 10^n-2 + ... + 10 + 1)
%% k  r(k)
%% 0  10^0-1/9   0      0
%% 1  10^1-1/9   1      1
%% 2  10^2-1/9   11     11
%% 3  10^3-1/9   111    3*37
%% 4  10^4-1/9   1111   11*101  (10-1)(10+1)(10^2+1)/9
%% 5  10^5-1/9   11111  41*271
%% 6  10^6-1/9   111111 3*7*11*13*37 (10^3-1)(10^3+1)/9 = r(3)*(10^3+1)
%% 7
%% 8
%% 9  10^9-1/9   1...11 3^2*37*333667 
%%
%% a(1): 1   1
%% a(3): 3   111
%% a(7): 6
%% a(9): 9
%% a(11): 2
%% a(13): 6
%% a(17): 16
%% a(19): 18
%% a(21): 6
%% a(23): 22
%% a(27): 27
%% a(29): 28
%% a(31): 15
%% a(33): 6
%% a(37): 3
%% a(39): 6
%% a(41): 5
%% a(43): 21
%% a(47): 46
%% a(49): 42
%% a(51): 48
%% a(53): 13
%% a(57): 18
%% a(59): 58
%% a(61): 60
%% a(63): 18
%% a(67): 33
%% a(69): 66
%% a(71): 35
%% a(73): 8
%% a(77): 6
%% a(79): 13
%% a(81): 81
%% a(83): 41
%% a(87): 84
%% a(89): 44
%% a(91): 6
%% a(93): 15
%% a(97): 96
%% a(99): 18
%%

a(N) ->
  a(1, N).

a(I, N) when I > N ->
  undefined;
a(I, N) ->
  case rdiv(I, N) of
    {_, 0} ->
      I;
    _ ->
      a(I+1, N)
  end.

a1(N) ->
  a1(1, N).

a1(I, N) when I > N ->
  undefined;
a1(I, N) ->
  case r(I) rem N =:= 0 of
    true ->
      I;
    false ->
      a1(I+1, N)
  end.

ceil(N) when N < 0 ->
  trunc(N);
ceil(N) ->
  T = trunc(N),
  case N - T == 0 of
    true ->
      T;
    false ->
      T + 1
  end.

fermat_factor(N) ->
  fermat_factor([N], []).

fermat_factor([], Ack) ->
  lists:sort(Ack);
fermat_factor([1], []) ->
  [1];
fermat_factor([2], []) ->
  [2];
fermat_factor([N|T], Ack) when N rem 2 =:= 0 ->
  fermat_factor([N div 2|T], [2 | Ack]);
fermat_factor([N|T], Ack) ->
  A = ceil(math:sqrt(N)),
  B = A*A - N,
  F = fermat_factor(A, B, N),
  case F of
    [] ->
      fermat_factor(T, Ack);
    [N] ->
      fermat_factor(T, [N|Ack]);
    [F1, F2] ->
      fermat_factor([F1, F2|T], Ack)
  end.

fermat_factor(A, B, N) ->
  BSqrt = trunc(math:sqrt(B)),
  case B =:= BSqrt*BSqrt of
    true ->
      F1 = A - BSqrt,
      F2 = A + BSqrt,
      lists:filter(fun(F) -> F =/= 1 end, [F1, F2]);
    false ->
      fermat_factor(A+1, (A+1)*(A+1) - N, N)
  end.

find(N) ->
  case N rem 2 =:= 0 of
    true ->
      find(N+1, N);
    false ->
      find(N, N)
  end.

find(I, N) ->
  case I rem 5 =/= 0 of
    true ->
      case a(I) > N of
        true ->
          I;
        false ->
          find(I+2, N)
      end;
    false ->
      find(I+2, N)
  end.

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

init() ->
  ok = erlang:load_nif(?LIBREPUNIT, 0).

main() ->
  io:format("repunit...~n", []),
  %%io:format("~p~n", [sieve(100)]),
  %%lists:foreach(
  %%  fun(N) ->
  %%      io:format("~p: ~w~n", [N, fermat_factor(N)])
  %%  end,
  %%  lists:seq(1,100)),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("~p: ~p: ~w~n", [I, r(I), fermat_factor(r(I))])
  %%  end,
  %%  lists:seq(1,16)),
  %% lists:foreach(
  %%   fun(I) ->
  %%       io:format("a(~p): ~p : ~p~n", [I, a(I), a1(I)]),
  %%       {D, R} = rdiv(I,13),
  %%       io:format("r(~p) / 13: ~p: ~p~n", [I, {D,R}, 13*D + R])
  %%   end,
  %%   [I || I <- lists:seq(1, 20), gcd(I, 10) =:= 1]),
  %%io:format("~p~n", [rdiv(3,3)]),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("~p: ~p~n", [I, find(I)])
  %%  end,
  %%  [10, 100, 250, 1000, 2500, 10000, 100000]),
  io:format("~p~n", [find(1000000)]),
  ok.

r(N) ->
  list_to_integer(lists:duplicate(N, $1)).

rdiv(R, N) ->
  rdiv(R-1, N, 1, 0).

rdiv(0, N, Rem, Ack) when Rem < N ->
  {Ack, Rem rem N};
rdiv(0, N, Rem, Ack) ->
  {10*Ack + Rem div N, Rem rem N};
rdiv(R, N, Rem, Ack) when Rem < N ->
  RemNext = 10*Rem + 1,
  AckNext =
  case RemNext < N of
    true ->
      10*Ack;
    false ->
      Ack
  end,
  rdiv(R-1, N, RemNext, AckNext);
rdiv(R, N, Rem, Ack) ->
  rdiv(R, N, Rem rem N, 10*Ack + Rem div N).

sieve(N) ->
  Primes =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{fixed, true}, {default, true}]))),
  lists:reverse(
    array:foldl(
      fun
        (I, true, Acc) ->
          [I|Acc];
        (_I, false, Acc) ->
          Acc
      end,
      [],
      sieve(2, N, Primes))).

sieve(I, N, Primes) when I*I > N ->
  Primes;
sieve(I, N, Primes) ->
  Next =
  case array:get(I, Primes) of
    true ->
      lists:foldl(
        fun(J, Array) ->
            array:set(J, false, Array)
        end,
        Primes,
        lists:seq(I*I, N, I));
    false ->
      Primes
  end,
  sieve(I+1, N, Next).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

a_test_() ->
  [
   ?_assertEqual(
      [1,3,6,9,2,6,16,18,6,22,27],
      [a(I) || I <- [1,3,7,9,11,13,17,19,21,23,27]])
  ].

a1_test_() ->
  [
   ?_assertEqual(
      [1,3,6,9,2,6,16,18,6,22,27], 
      [a1(I) || I <- [1,3,7,9,11,13,17,19,21,23,27]])
  ].

ceil_test_() ->
  [
   ?_assertEqual(-1, ceil(-1)),
   ?_assertEqual(-1, ceil(-1)),
   ?_assertEqual(0, ceil(0)),
   ?_assertEqual(1, ceil(1)),
   ?_assertEqual(2, ceil(1.1))
  ].

fermat_factor_test() ->
  [
   ?_assertEqual([1], fermat_factor(1)),
   ?_assertEqual([2], fermat_factor(2)),
   ?_assertEqual([3], fermat_factor(3)),
   ?_assertEqual([2,2,5,5], fermat_factor(100)),
   ?_assertEqual([3,37], fermat_factor(111))
  ].

gcd_test_() ->
  [
   ?_assertEqual(2, gcd(2, 0)),
   ?_assertEqual(1, gcd(2, 3)),
   ?_assertEqual(1, gcd(3, 2)),
   ?_assertEqual(10, gcd(20, 30)),
   ?_assertEqual(10, gcd(30, 20))
  ].

r_test_() ->  
  [
   ?_assertEqual(1, r(1)),
   ?_assertEqual(11, r(2)),
   ?_assertEqual(111, r(3))
  ].

rdiv_test_() ->
  Test =
  fun(R, N) ->
      {Div, Rem} = rdiv(R, N),
      r(R) =:= N*Div + Rem
  end,
  [
   ?_assert(Test(10, 1)),
   ?_assert(Test(10, 2)),
   ?_assert(Test(10, 3)),
   ?_assert(Test(10, 4)),
   ?_assert(Test(10, 5)),
   ?_assert(Test(10, 6)),
   ?_assert(Test(10, 7)),
   ?_assert(Test(10, 8)),
   ?_assert(Test(10, 9)),
   ?_assert(Test(10, 10))
  ].

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
