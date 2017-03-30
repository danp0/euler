-module(pentagon).

-export(
   [
    generate/1,
    main/0,
    n_of/1,
    pentagon/1,
    walk/1
   ]
  ).

generate(K) ->
  fun(N) ->
      (3*K*K + 6*K*N - K) div 2
  end.

main() ->
  io:format("pentagon...~n", []),
  lists:foreach(
    fun(N) ->
      case walk(N) of
        false ->
          false;
        Ok ->
          io:format("~p: ~p~n", [N, Ok])
      end
    end,
    lists:seq(1,10000)),
  ok.

n_of(Pn) ->
  N = trunc((1 + math:sqrt(1 + 24*Pn))/6),
  case Pn =:= pentagon(N) of
    true ->
      N;
    false ->
      false
  end.

pentagon(N) ->
  N*(3*N-1) div 2.

walk(Width) ->
  walk(Width, generate(Width),1).

walk(_Width, _F, 10000) ->
  false;
walk(Width, F, N) ->
  Pdiff = F(N),
  case n_of(Pdiff) of
    false ->
      walk(Width,F,N+1);
    Nth ->
      Pn = pentagon(N),
      Pn1 = pentagon(N+Width),
      Psum = Pn + Pn1,
      case n_of(Psum) of
        false ->
          walk(Width,F,N+1);
        Nth2 ->
          {{N, Pn}, {N+Width, Pn1}, {Nth, Pdiff}, {Nth2, Psum}}
      end
  end.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

generate_test_() ->
  F1 = generate(1),
  F2 = generate(2),
  F3 = generate(3),
  [
   ?_assertEqual(pentagon(10) - pentagon(9), F1(9)),
   ?_assertEqual(pentagon(11) - pentagon(9), F2(9)),
   ?_assertEqual(pentagon(12) - pentagon(9), F3(9))
  ].

n_of_test_() ->
  [
   ?_assertEqual(lists:seq(1,10),
                 [n_of(Pn) || Pn <- [1,5,12,22,35,51,70,92,117,145]]),
   ?_assertNot(n_of(2)),
   ?_assertNot(n_of(3)),
   ?_assertNot(n_of(4)),
   ?_assertNot(n_of(6))
  ].

pentagon_test_() ->
  [
   ?_assertEqual([1,5,12,22,35,51,70,92,117,145], 
                 [pentagon(N) || N <- lists:seq(1,10)])
  ].

-endif.
