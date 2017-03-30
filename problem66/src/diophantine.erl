-module(diophantine).

-export(
   [
    a/1,
    a_next/1,
    a_seq/2,
    hk/1,
    hk_next/1,
    hk_seq/2,
    find/1,
    find_max/1,
    is_diophantine/1,
    is_square/1,
    main/0,
    solve/1
   ]
  ).

-record(
   a_param,
   {
    s,
    a0,
    mn,
    dn,
    an
   }
  ).

-record(
   hk_param,
   {
    a,
    h1,
    k1,
    h2,
    k2
   }
  ).

a(S) ->
  M0 = 0,
  D0 = 1,
  A0 = trunc(math:sqrt(S)),
  #a_param{s=S, a0=A0, mn=M0, dn=D0, an=A0}.

a_next(#a_param{s=S, a0=A0, mn=Mn, dn=Dn, an=An}) ->
  M = Dn*An - Mn,
  D = (S - M*M) div Dn,
  A = (A0 + M) div D,
  #a_param{s=S, a0=A0, mn=M, dn=D, an=A}.

a_seq(N, S) ->
  a_seq(N, a(S), []).

a_seq(1, A, Acc) ->
  lists:map(
    fun(#a_param{an = An}) ->
        An
    end,
    lists:reverse([A|Acc]));
a_seq(N, A, Acc) ->
  a_seq(N-1, a_next(A), [A|Acc]).

hk(S) ->
  #hk_param{a=a(S), h1=1, k1=0, h2=0, k2=1}.

hk_next(#hk_param{a=#a_param{an=An}=A, h1=H1, k1=K1, h2=H2, k2=K2}) ->
  H = An*H1 + H2,
  K = An*K1 + K2,
  #hk_param{a=a_next(A), h1=H, k1=K, h2=H1, k2=K1}.

hk_seq(N, S) ->
  hk_seq(N, hk(S), []).

hk_seq(0, HK, Acc) ->
  lists:map(
    fun(#hk_param{h1=H1, k1=K1}) ->
        {H1,K1}
    end,
    tl(lists:reverse([HK|Acc])));
hk_seq(N, HK, Acc) ->
  hk_seq(N-1, hk_next(HK), [HK|Acc]).

find(UpTo) ->
  Ds = [N || N <- lists:seq(2,UpTo), is_square(N) =:= false],
  lists:map(fun solve/1, Ds).

find_max(Ds) ->
  lists:foldl(
    fun({X, D, Y}, {Xm, Dm, Ym}) ->
        case X > Xm of
          true ->
            {X, D, Y};
          false ->
            {Xm, Dm, Ym}
        end
    end,
    {1, 0, 0},
    Ds).

is_diophantine({X, D, Y}) ->
  X*X - D*Y*Y =:= 1.

is_square(N) when N >= 1 ->
  Sqrt = trunc(math:sqrt(N)),
  if
    Sqrt * Sqrt =:= N ->
      Sqrt;
    true ->
      false
  end.

main() ->
  Ds = find(1000),
  {X, D, Y} = find_max(Ds),
  io:format("~p~n", [Ds]),
  io:format("max: ~p**2 - ~p * ~p**2 = 1~n", [X, D, Y]),
  ok.

solve(D) ->
  solve(D, hk_next(hk(D))).

solve(D, #hk_param{h1=H1, k1=K1} = HK) ->
  case is_diophantine({H1, D, K1}) of
    true ->
      {H1, D, K1};
    false ->
      solve(D, hk_next(HK))
  end.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

a_seq_test_() ->
  [
   ?_assertEqual([1,2], a_seq(2,2)),
   ?_assertEqual([1,1,2], a_seq(3,3)),
   ?_assertEqual([10,1,2,10,2,1,20,1,2,10], a_seq(10, 114))
  ].

find_test_() ->
  [
   ?_assertEqual(
      [{3,2,2}, {2,3,1}, {9,5,4}, {5,6,2}, {8,7,3}], find(7))
  ].

find_max_test_() ->
  [
    ?_assertEqual({9,5,4}, find_max(find(7)))
  ].

hk_seq_test_() ->
  [
   ?_assertEqual([{1,1}, {3,2}, {7,5}, {17,12}, {41,29}], hk_seq(5, 2)),
   ?_assertEqual([{2,1}, {3,1}, {5,2}, {8,3}], hk_seq(4, 7))
  ].

is_diophantine_test_() ->
  [
   ?_assert(is_diophantine({3,2,2})),
   ?_assert(is_diophantine({2,3,1})),
   ?_assertNot(is_diophantine({1,2,1}))
  ].

is_square_test_() ->
  [
   ?_assertEqual(1, is_square(1)),
   ?_assertNot(is_square(2)),
   ?_assertNot(is_square(3)),
   ?_assertEqual(2, is_square(4)),
   ?_assertEqual(3, is_square(9))
  ].

solve_test_() ->
  [
   ?_assertEqual({3,2,2}, solve(2)),
   ?_assertEqual({2,3,1}, solve(3)),
   ?_assertEqual({9,5,4}, solve(5)),
   ?_assertEqual({5,6,2}, solve(6)),
   ?_assertEqual({8,7,3}, solve(7))
  ].

-endif.
