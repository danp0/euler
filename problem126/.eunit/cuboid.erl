-module(cuboid).

-export(
   [
    build/1,
    c/1,
    cover/2,
    main/0
   ]
  ).

%%
%% cover({l,w,h}, 1) = lw + (2h(l+w) + 4h(1-1)) + lw
%% cover({l,w,h}, 2) = lw + 2h(l+w) + (2h(l+w) + 4h(2-1)) + 2h(l+w) + lw
%% ...
%% cover({l,w,h}, n) = 2lw + 2sum(1, n-1, 2(l+w) + 4(i-1)) + 2h(l+w) + 4h(n-1)
%%
%% cover({1,1,1}, 1) =   6 =  1 + 4 + 1 
%% cover({1,1,1}, 2) =  18 =  1 + 4 + 8 + 4 + 1
%% cover({1,1,1}, 3) =  38 =  1 + 4 + 8 + 12 + 8 + 4 + 1
%% cover({1,1,1}, 4) =  66 =  1 + 4 + 8 + 12 + 16 + 12 + 8 + 4 + 1
%%
%% cover({2,1,1}, 1) =  10 =  2 + 6 + 2
%% cover({2,1,1}, 2) =  26 =  2 + 6 + 10 + 6 + 2
%% cover({2,1,1}, 3) =  50 =  2 + 6 + 10 + 14 + 10 + 6 + 2
%% cover({2,1,1}, 4) =  82 =  2 + 6 + 10 + 14 + 18 + 14 + 10 + 6 + 2
%%
%% cover({2,2,1}, 1) =  16 =  4 + 8 + 4
%% cover({2,2,1}, 2) =  36 =  4 + 8 + 12 + 8 + 4
%%
%% cover({2,2,2}, 1) =  24 =  4 + 16 + 4
%% cover({2,2,2}, 2) =  48 =  4 + 8 + 24 + 8 + 4
%% cover({2,2,2}, 3) =  80 =  4 + 8 + 12 + 32 + 12 + 8 + 4
%% cover({2,2,2}, 4) = 120 =  4 + 8 + 12 + 16 + 40 + 16 + 12 + 8 + 4
%%
%% cover([{2,2,1}, {2,2,1}], 1) = 24 = 4 + 8 + 8 + 4
%% cover([{2,2,1}, {2,2,1}], 2) = 48 = 4 + 8 + 12 + 12 + 8 + 4
%%
%% cover({3,1,1}, 1) = 14  = 3 + 8 + 3
%% cover({3,1,1}, 2) = 34  = 3 + 8 + 12 + 8 + 3
%%
%% cover({3,2,1}, 1) = 22  = 6 + 10 + 6
%% cover({3,2,1}, 2) = 46  = 6 + 10 + 14 + 10 + 6
%% cover({3,2,1}, 3) = 78  = 6 + 10 + 14 + 18 + 14 + 10 + 6
%% cover({3,2,1}, 4) = 118 = 6 + 10 + 14 + 18 + 22 + 18 + 14 + 10 + 6
%%
%% cover({3,3,1}, 1) =  30 = 9 + 12 + 9
%% cover({3,3,1}, 2) =  58 = 9 + 12 + 16 + 12 + 9
%% cover({3,3,1}, 3) =  94 = 9 + 12 + 16 + 20 + 16 + 12 + 9
%%
%% cover({3,3,3}, 1) =  54 = 9 + 36 + 9
%% cover({3,3,3}, 2) =  90 = 9 + 12 + 48 + 12 + 9
%% cover({3,3,3}, 3) = 134 = 9 + 12 + 16 + 60 + 16 + 12 + 9 
%% cover({3,3,3}, 4) = 186 = 9 + 12 + 16 + 20 + 72 + 20 + 16 + 12 + 9 
%%
%% cover([{3,3,1}, {3,3,1}, {3,3,1}], 1) =  54 = 9 + 12 + 12 + 12 + 9
%% cover([{3,3,1}, {3,3,1}, {3,3,1}], 2) =  90 = 9 + 12 + 16 + 16 + 16 + 12 + 9
%% cover([{3,3,1}, {3,3,1}, {3,3,1}], 3) = 134 = 9 + 12 + 16 + 20 + 20 + 20 + 16 + 12 + 9
%%

build(Limit) ->
  build(Limit, 1, gb_trees:empty()).

build(Limit, L, Ack) ->
  Cover = cover({L, L, L}, 1),
  case Cover > Limit of
    true ->
      Ack;
    false ->
      build(Limit, L+1, build(Limit, L, L, Ack))
  end.

build(Limit, L, H, Ack) ->
  Cover = cover({L, H, L}, 1),
  case Cover > Limit of
    true ->
      Ack;
    false ->
      build(Limit, L, H+1, build(Limit, L, H, H, Ack))
  end.

build(Limit, L, H, W, Ack) ->
  Cover = cover({L, H, W}, 1),
  case Cover > Limit of
    true ->
      Ack;
    false ->
      build(Limit, L, H, W+1, build(Limit, L, H, W, 1, Ack))
  end.

build(Limit, L, H, W, N, Ack) ->
  Cover = cover({L, H, W}, N),
  case Cover > Limit of
    true ->
      Ack;
    false ->
      Count =
      case gb_trees:lookup(Cover, Ack) of
        none ->
          1;
        {value, Value} ->
          Value + 1
      end,
      build(Limit, L, H, W, N+1, gb_trees:enter(Cover, Count, Ack))
  end.

c(C) ->
  Cuboid = {1, 1, 1},
  c(C, cover(Cuboid, 1), Cuboid, 1, 0).

c(C, Cover, {L, 1, 1}, N, Ack) when Cover > C, L > (C div 4), N =:= 1 ->
  Ack;
c(C, Cover, Cuboid, _N, Ack) when Cover > C ->
  c(C, cover(next(Cuboid), 1), next(Cuboid), 1, Ack);
c(C, Cover, Cuboid, N, Ack) when Cover =:= C ->
  c(C, cover(Cuboid, N+1), Cuboid, N+1, Ack+1);
c(C, _Cover, Cuboid, N, Ack) ->
  c(C, cover(Cuboid, N+1), Cuboid, N+1, Ack).

cover({L, W, H}, N) ->
  2 * L * W + 
  2 * lists:sum([2 * (L + W) + 4 * (I - 1) || I <- lists:seq(1,N-1)]) +
  2 * H * (L + W) + 4 * H * (N - 1).

main() ->
  io:format("cuboid...~n", []),
  Tree = build(30000),
  List = 
  lists:sort(
    fun({_,C1}, {_,C2}) -> 
        C1 =< C2 
    end,
    lists:filter(fun({_, Count}) -> Count =:= 1000 end, gb_trees:to_list(Tree))),
  io:format("~p~n", [hd(List)]),
  %%lists:foreach(
  %%  fun(N) ->
  %%      io:format("c(~p): ~p~n", [N, c(N)])
  %%  end,
  %%  lists:seq(2,250,2)),
  %%io:format("c(~p): ~p~n", [1000, c(1000)]),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("{~p, 1, 1}: ~p~n", [I, cover({I,1,1}, 1)])
  %%  end,
  %%  lists:seq(1,20)),
  %%Cover =
  %%lists:sort(
  %%    [
  %%     {cover({L,W,H}, N), {L,W,H}, N} || 
  %%     N <- lists:seq(1,10), L <- lists:seq(1,80), W <- lists:seq(1,L), H <- lists:seq(1,W)
  %%    ]),
  %%lists:foreach(
  %%  fun({C, Dim, N}) ->
  %%      io:format("~p, ~p: ~p~n", [Dim, N, C])
  %%  end,
  %%  Cover),
  %%lists:foreach(
  %%  fun({L, W, H}) ->
  %%      lists:foreach(
  %%        fun(N) ->
  %%            io:format("~p, ~p: ~p~n", [{L,W,H}, N, cover({L,W,H}, N)])
  %%        end,
  %%        lists:seq(1,8))
  %%  end,
  %%  [{L, W, H} || L <- lists:seq(1,3), W <- lists:seq(1,L), H <- lists:seq(1,W)]),
  ok.

next({N, N, N}) ->
  {N+1, 1, 1};
next({L, N, N}) ->
  {L, N+1, 1};
next({L, W, H}) ->
  {L, W, H+1}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

build_test_() ->
  Tree = build(200),
  [
   ?_assertEqual({value, 2}, gb_trees:lookup(22, Tree)),
   ?_assertEqual({value, 4}, gb_trees:lookup(46, Tree)),
   ?_assertEqual({value, 5}, gb_trees:lookup(78, Tree)),
   ?_assertEqual({value, 8}, gb_trees:lookup(118, Tree)),
   ?_assertEqual({value, 10}, gb_trees:lookup(154, Tree))
  ].

c_test_() ->
  [
   ?_assertEqual(2, c(22)),
   ?_assertEqual(4, c(46)),
   ?_assertEqual(5, c(78)),
   ?_assertEqual(8, c(118)),
   ?_assertEqual(10, c(154))
  ].

cover_test_() ->
  [
   ?_assertEqual([ 6, 18,  38,  66], [cover({1,1,1}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([10, 26,  50,  82], [cover({2,1,1}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([24, 48,  80, 120], [cover({2,2,2}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([54, 90, 134, 186], [cover({3,3,3}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({1,2,3}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({1,3,2}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({2,1,3}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({2,3,1}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({3,1,2}, N) || N <- lists:seq(1,4)]),
   ?_assertEqual([22, 46,  78, 118], [cover({3,2,1}, N) || N <- lists:seq(1,4)])
  ].

-endif.
