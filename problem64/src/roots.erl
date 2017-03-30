-module(roots).

-export(
   [
    a_until/1,
    main/0
   ]
  ).

a_until(S) ->
  M0 = 0,
  D0 = 1,
  A0 = trunc(math:sqrt(S)),
  a_until(0, A0, S, M0, D0, A0, []).

a_until(I, A0, S, M, D, A, Acc) ->
  case lists:member({M,D,A}, Acc) of
    true ->
      Acc2 = lists:reverse(Acc),
      {Prefix, Suffix} = lists:splitwith(fun(E) -> E =/= {M,D,A} end, Acc2),
      Strip = fun({_,_,An}) -> An end,
      {S, {lists:map(Strip, Prefix), lists:map(Strip, Suffix)}};
    false ->
      M1 = D*A - M,
      D1 = (S - M1*M1) div D,
      if
        D1 =:= 0 ->
          {sqrt, A};
        true ->
          A1 = (A0 + M1) div D1,
          a_until(I+1, A0, S, M1, D1, A1, [{M,D,A} | Acc])
      end
  end.

main() ->
  io:format("roots...~n", []),
  L =
  lists:reverse(
    lists:foldl(
      fun(S, Acc) ->
          Seq = a_until(S),
          case Seq of
            {sqrt, _} ->
              Acc;
            Seq ->
              [Seq|Acc]
          end
      end,
      [],
      lists:seq(2,10000))
   ),
  OddPeriods =
  lists:foldl(
    fun({_S, {_Prefix, Suffix}}, Sum) ->
        case length(Suffix) rem 2 =:= 1 of
          true ->
            Sum + 1;
          false ->
            Sum
        end
    end,
    0,
    L),
  io:format("odd periods: ~p~n", [OddPeriods]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

a_until_test_() ->
  [
   ?_assertEqual({2, {[1], [2]}}, a_until(2)),
   ?_assertEqual({3, {[1], [1,2]}}, a_until(3)),
   ?_assertEqual({5, {[2], [4]}}, a_until(5))
  ].

-endif.
