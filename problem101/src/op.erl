-module(op).

-export(
   [
    dim/1,
    enter/2,
    get/2,
    get/3,
    identity/1,
    identity/2,
    init/0,
    lookup/1,
    lu/1,
    lu_evaluate/3,
    main/0,
    mul/2,
    op/1,
    pivot/2,
    pow/2,
    put/4,
    solve/2,
    zero/1,
    zero/3
   ]
  ).

-record(
   state,
   {
    matrices = gb_trees:empty()
   }
  ).

col(I, Matrix) ->
  [lists:nth(I, Row) || Row <- Matrix].

dim(Key) ->
  send({dim, Key, self()}).

enter(Key, Matrix) ->
  send({enter, Key, Matrix, self()}).

get(Key, I) ->
  send({get, Key, I, self()}).

get(Key, I, J) ->
  send({get, Key, I, J, self()}).

identity(N) ->
  [
   lists:flatten([lists:duplicate(I-1, 0), [1], lists:duplicate(N-I, 0)])
   || I <- lists:seq(1, N)
  ].

identity(Key, N) ->
  send({enter, Key, identity, N, self()}).

init() ->
  loop(#state{}).

lookup(Key) ->
  send({lookup, Key, self()}).

loop(State) ->
  NewState =
  receive
    {dim, Key, From} ->
      From !
      case gb_trees:lookup(Key, State#state.matrices) of
        none ->
          none;
        {value, Value} ->
          {array:size(Value), array:size(array:get(0, Value))}
      end,
      State;
    {enter, Key, identity, N, From} ->
      Matrix =
      array:from_list(
        [
         array:from_list(
           lists:flatten(
             [lists:duplicate(I-1, 0), [1], lists:duplicate(N-I, 0)]))
         || I <- lists:seq(1, N)
        ]),
      Tree = gb_trees:enter(Key, Matrix, State#state.matrices),
      From ! ok,
      State#state{matrices = Tree};
    {enter, Key, zero, M, N, From} ->
      Matrix =
      array:from_list(
        lists:duplicate(M, array:from_list(lists:duplicate(N, 0)))),
      Tree = gb_trees:enter(Key, Matrix, State#state.matrices),
      From ! ok,
      State#state{matrices = Tree};
    {enter, Key, Matrix, From} ->
      Matrix2 =
      array:from_list(
        lists:map(
          fun(Row) ->
              array:from_list(Row)
          end,
          Matrix)),
      Tree = gb_trees:enter(Key, Matrix2, State#state.matrices),
      From ! ok,
      State#state{matrices = Tree};
    {get, Key, I, From} ->
      From !
      case gb_trees:lookup(Key, State#state.matrices) of
        none ->
          none;
        {value, Value} ->
          array:to_list(array:get(I, Value))
      end,
      State;
    {get, Key, I, J, From} ->
      From !
      case gb_trees:lookup(Key, State#state.matrices) of
        none ->
          none;
        {value, Value} ->
          array:get(J, (array:get(I, Value)))
      end,
      State;
    {lookup, Key, From} ->
      From !
      case gb_trees:lookup(Key, State#state.matrices) of
        none ->
          none;
        {value, Value} ->
          lists:map(
            fun(Row) ->
                array:to_list(Row)
            end,
            array:to_list(Value))
      end,
      State;
    {put, Key, I, J, Element, From} ->
      {Result, Tree} =
      case gb_trees:lookup(Key, State#state.matrices) of
        none ->
          {false, State#state.matrices};
        {value, Value} ->
          {true, 
           gb_trees:enter(Key,
                          array:set(I, 
                                    array:set(J, Element, array:get(I, Value)),
                                    Value), State#state.matrices)}
      end,
      From ! Result,
      State#state{matrices = Tree};
    {stop, From} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

lu(A) ->
  {N, N} = dim(A),
  identity(l, N),
  zero(u, N, N),
  pivot(A, p),
  APrime = mul(p, A),
  enter(aprime, APrime),
  lists:foreach(
    fun(I) ->
        lists:foreach(
          fun(J) ->
              if
                J =< I ->
                  S1 = lists:sum(
                         [get(l, J, K) * get(u, K, I) || 
                          K <- lists:seq(0, J-1)]),
                  put(u, J, I, get(aprime, J, I) - S1),
                  true;
                true ->
                  false
              end,
              if
                J >= I ->
                  S2 = lists:sum(
                         [get(l, J, K) * get(u, K, I) || 
                          K <- lists:seq(0, I-1)]),
                  put(l, J, I, (get(aprime, J, I) - S2) / get(u, I, I)),
                  true;
                true ->
                  false
              end
          end,
          lists:seq(0, N - 1))
    end,
    lists:seq(0, N - 1)),
  ok.

lu_evaluate(A, X, B) ->
  {N, N} = dim(A),
  zero(X, N, 1),
  zero(y, N, 1),
  lu(A),
  enter(pb, mul(p, B)),
  %% Ax = b -> PAx = Pb -> LUx = Pb
  %% Forward solve Ly = b
  lists:foreach(
    fun(I) ->
        put(y, I, 0, get(pb, I, 0)),
        lists:foreach(
          fun(J) ->
              put(y, I, 0, get(y, I, 0) - get(l, I, J) * get(y, J, 0))
          end,
          lists:seq(0, I-1)),
        put(y, I, 0, get(y, I, 0) / get(l, I, I))
    end,
    lists:seq(0, N-1)),
  %% Backward solve Ux = y
  lists:foreach(
    fun(I) ->
        put(X, I, 0, get(y, I, 0)),
        lists:foreach(
          fun(J) ->
           put(X, I, 0, get(X, I, 0) - get(u, I, J) * get(X, J, 0))   
          end,
          lists:seq(I+1, N-1)),
        put(X, I, 0, get(X, I, 0) / get(u, I, I))
    end,
    lists:seq(N-1, 0, -1)),
  ok.

main() ->
  start(),
  F =
  fun(X) ->
      1 - X + pow(X,2) - pow(X,3) + pow(X,4) - pow(X,5) + 
      pow(X,6) - pow(X,7) + pow(X,8) - pow(X,9) + pow(X,10)
  end,
  Fits = solve(F, 10),
  io:format("~p~n~p~n", [Fits, lists:sum(Fits)]),
  ok = stop(),
  ok.

mul(M1, M2) when is_atom(M1), is_atom(M2) ->
  {L1, L2} = dim(M1),
  {L2, L3} = dim(M2),
  [
   [
    lists:sum([get(M1, I, K) * get(M2, K, J) || K <- lists:seq(0, L2-1)]) 
    || J <- lists:seq(0,L3-1)
   ] 
   || I <- lists:seq(0, L1-1)
  ];
mul(M1, M2) ->
  C2 =
  [col(I, M2) || I <- lists:seq(1, length(M1))],
  split(
    length(M1),
    [
     lists:sum(
       lists:map(
         fun({E1, E2}) ->
             E1 * E2
         end, 
         lists:zip(R1, R2))) || 
     R1 <- M1, R2 <- C2
    ]).

op(Seq) ->
  A =
  [[pow(I, J) || 
    J <- lists:seq(0, length(Seq) - 1)] || I <- lists:seq(1, length(Seq))],
  B =
  [[E] || E <- Seq],
  enter(a, A),
  enter(b, B),
  lu_evaluate(a, x, b),
  X = lookup(x),
  fun(N) ->
      Pow = [[pow(N, I) || I <- lists:seq(0, length(X) - 1)]],
      [[Product]] = mul(Pow, X),
      Product
  end.

pivot(Matrix, Pivot) ->
  {N, N} = dim(Matrix),
  enter(Pivot, identity(N)),
  lists:foreach(
    fun(I) ->
        MaxJ =
        lists:foldl(
          fun(J, Max) ->
              case abs(get(Matrix, J, I)) > abs(get(Matrix, Max, I)) of
                true ->
                  J;
                false ->
                  Max
              end
          end,
          I,
          lists:seq(I, N-1)),
        case MaxJ =/= I of
          true ->
            PivotJ = get(Pivot, MaxJ),
            PivotI = get(Pivot, I),
            lists:foreach(
              fun(K) ->
                  put(Pivot, MaxJ, K, lists:nth(K+1, PivotI)),
                  put(Pivot, I, K, lists:nth(K+1, PivotJ))
              end,
              lists:seq(0, N-1)),
            true;
          false ->
            false
        end
    end,
    lists:seq(0, N-1)),
  ok.

pow(_A, 0) ->
  1;
pow(A, 1) ->
  A;
pow(A, 2) ->
  A * A;
pow(A, B) when B rem 2 =:= 1 ->
  A * pow(A, B-1);
pow(A, B) ->
  pow(pow(A, B div 2), 2).

put(Key, I, J, Element) ->
  send({put, Key, I, J, Element, self()}).

send(Msg) ->
  ?MODULE ! Msg,
  receive
    Result ->
      Result
  end.

solve(F, N) ->
  Seq = [F(I) || I <- lists:seq(1,N)],
  lists:map(
    fun(J) ->
        OP = op(lists:sublist(Seq, J)),
        OP(J+1)
    end,
    lists:seq(1, N)).

split(N, L) ->
  split(N, L, []).

split(_N, [], Acc) ->
  lists:reverse(Acc);
split(N, L, Acc) ->
  {L1, L2} = lists:split(N, L),
  split(N, L2, [L1|Acc]).

start() ->
  register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
  send({stop, self()}).

zero(N) ->
  lists:duplicate(N, lists:duplicate(N, 0)).

zero(Key, M, N) ->
  send({enter, Key, zero, M, N, self()}).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

dim_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(ok, enter(a, [[1,2], [3,4]])),
    ?_assertEqual({2,2}, dim(a))
   ]
  }.

enter_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(ok, enter(a, [[1, 2], [3, 4]])),
    ?_assertEqual([[1,2], [3,4]], lookup(a))
   ]
  }.

get_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(ok, enter(a, [[1, 2], [3, 4]])),
    ?_assertEqual([1, 2], get(a, 0)),
    ?_assertEqual([3, 4], get(a, 1)),
    ?_assertEqual(1, get(a, 0, 0)),
    ?_assertEqual(2, get(a, 0, 1)),
    ?_assertEqual(3, get(a, 1, 0)),
    ?_assertEqual(4, get(a, 1, 1))
   ]
  }.

identity_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([[1]], identity(1)),
    ?_assertEqual([[1,0], [0,1]], identity(2)),
    ?_assertEqual([[1,0,0], [0,1,0], [0,0,1]], identity(3)),
    ?_assertEqual(ok, identity(i, 1)),
    ?_assertEqual([[1]], lookup(i)),
    ?_assertEqual(ok, identity(i, 2)),
    ?_assertEqual([[1,0], [0,1]], lookup(i)),
    ?_assertEqual(ok, identity(i, 3)),
    ?_assertEqual([[1,0,0], [0,1,0], [0,0,1]], lookup(i))
   ]
  }.

lookup_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(ok, enter(a, [[1, 2], [3, 4]])),
    ?_assertEqual([[1,2], [3,4]], lookup(a)),
    ?_assertEqual(none, lookup(b))
   ]
  }.

lu_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(ok, 
                      enter(a, 
                            [
                             [1.0,3.0,5.0], 
                             [2.0,4.0,7.0], 
                             [1.0,1.0,0.0]])),
        ?_assertEqual(ok, 
                      enter(b, 
                            [[11.0,9.0,24.0,2.0], 
                             [1.0,5.0,2.0,6.0], 
                             [3.0,17.0,18.0,1.0], 
                             [2.0,5.0,7.0,1.0]])),
        ?_assertEqual(ok, lu(a)),
        ?_assertEqual(mul(p,a), mul(l,u)),
        ?_assertEqual(ok, lu(b)),
        ?_assertEqual(mul(p,b), mul(l,u))
       ]
   end
  }.

lu_evaluate_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       [
        ?_assertEqual(ok, enter(a, [[1,3,5], [2,4,7], [1,1,0]])),
        ?_assertEqual(ok, enter(b, [[1.0], [2.0], [3.0]])),
        ?_assertEqual(ok, lu_evaluate(a, x, b)),
        ?_assertEqual(lookup(b), mul(a, x))
       ]
   end
  }.

mul_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       I = [[1,0], [0,1]],
       M1 = [[1,1],[1,1]],
       M2 = [[1,2], [3,4]],
       M3 = [[1], [2]],
       enter(i, I),
       enter(m1, M1),
       enter(m2, M2),
       enter(m3, M3),
       [
        ?_assertEqual(M2, mul(identity(2), M2)),
        ?_assertEqual(M2, mul(M2, identity(2))),
        ?_assertEqual([[7,10], [15,22]], mul(M2, M2)),
        ?_assertEqual([[2,2], [2,2]], mul(m1, m1)),
        ?_assertEqual([[4,6], [4,6]], mul(m1, m2)),
        ?_assertEqual([[3,3], [7,7]], mul(m2, m1)),
        ?_assertEqual(M2, mul(i, m2)),
        ?_assertEqual(M2, mul(m2, i)),
        ?_assertEqual([[7,10], [15,22]], mul(m2, m2)),
        ?_assertEqual([[3], [3]], mul(m1, m3))
       ]
   end
  }.

op_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       OP1 = op([1]),
       OP2 = op([1,8]),
       OP3 = op([1,8,27]),
       [
        ?_assertEqual([1.0,1.0], [OP1(I) || I <- lists:seq(1,2)]),
        ?_assertEqual([1.0,8.0,15.0], [OP2(I) || I <- lists:seq(1,3)]),
        ?_assertEqual([1.0,8.0,27.0,58.0], [OP3(I) || I <- lists:seq(1,4)])
       ]
   end
  }.

pivot_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       A = [[1,4], [2,3]],
       B = [[1,3,5], [2,4,7], [1,1,0]],
       C = [[11,9,24,2], [1,5,2,6], [3,17,18,1], [2,5,7,1]],
       enter(a, A),
       pivot(a, pa),
       enter(b, B),
       pivot(b, pb),
       enter(c, C),
       pivot(c, pc),
       [
        ?_assertEqual([[0,1], [1,0]], lookup(pa)),
        ?_assertEqual([[0,1,0], [1,0,0], [0,0,1]], lookup(pb)),
        ?_assertEqual([[1,0,0,0], [0,0,1,0], [0,1,0,0], [0,0,0,1]], lookup(pc))
       ]
   end
  }.

pow_test_() ->
  [
   ?_assertEqual([1, 2, 4, 8, 16, 32, 64], [pow(2, I) || I <- lists:seq(0, 6)])
  ].

put_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual(ok, enter(a, [[0, 0], [0, 0]])),
    ?_assert(put(a, 0, 0, 1)),
    ?_assert(put(a, 0, 1, 2)),
    ?_assert(put(a, 1, 0, 3)),
    ?_assert(put(a, 1, 1, 4)),
    ?_assertEqual([[1,2], [3,4]], lookup(a))
   ]
  }.

solve_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   fun(_) ->
       Fits = solve(fun(X) -> pow(X,3) end, 3),
       [
        ?_assertEqual([1.0, 15.0, 58.0], Fits)
       ]
   end
  }.

zero_test_() ->
  {
   setup,
   fun() -> start() end,
   fun(_) -> stop() end,
   [
    ?_assertEqual([[0]], zero(1)),
    ?_assertEqual([[0,0], [0,0]], zero(2)),
    ?_assertEqual([[0,0,0], [0,0,0], [0,0,0]], zero(3)),
    ?_assertEqual(ok, zero(z, 1, 1)),
    ?_assertEqual([[0]], lookup(z)),
    ?_assertEqual(ok, zero(z, 2, 2)),
    ?_assertEqual([[0,0], [0,0]], lookup(z)),
    ?_assertEqual(ok, zero(z, 3, 1)),
    ?_assertEqual([[0], [0], [0]], lookup(z)),
    ?_assertEqual(ok, zero(z, 1, 3)),
    ?_assertEqual([[0,0,0]], lookup(z))
   ]
  }.

-endif.
