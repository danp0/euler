-module(triangle).

-export(
   [
    count_sums/1,
    euclid/1,
    euclid/2,
    gcd/2,
    main/0
   ]
  ).

%%
%% a = k(m^2 - n^2)
%% b = 2kmn
%% c = k(m^2 + n^2)
%% a + b + c = 2km^2 + 2kmn
%%
%% m > n, m - n odd, m and n coprime
%%
%%
count_sums(E) ->
  Sums =
  lists:sort(
    lists:map(
      fun({K,M,N}) ->
          2 * K * M * (M+N)
      end,
      E)
   ),
  count_sums(Sums, gb_trees:empty()).

count_sums([], Mapping) ->
  Mapping;
count_sums([S|T], Mapping) ->
  Count =
  case gb_trees:lookup(S, Mapping) of
    none ->
      1;
    {value, Value} ->
      Value + 1
  end,
  count_sums(T, gb_trees:enter(S, Count, Mapping)).

euclid(Sum) ->
  euclid(1, Sum, []).

euclid(K, Sum) ->
  HalfSum = Sum div (2*K),
  case HalfSum >= 2 of
    true ->
      [{K,M,N} || 
       M <- lists:seq(2, HalfSum), 
       N <- lists:seq(1, min(M - 1, max(0, (HalfSum div M) - M))), 
       (M - N) rem 2 =:= 1, 
       gcd(M,N) =:= 1
      ];
    false ->
      []
  end.

euclid(K, Sum, Acc) ->
  E = euclid(K, Sum),
  case length(E) =:= 0 of
    true ->
      lists:sort(lists:flatten(Acc));
    false ->
      euclid(K+1, Sum, [E | Acc])
  end.

gcd(A,A) ->
  A;
gcd(A,B) when A > B ->
  gcd(A-B,B);
gcd(A,B) ->
  gcd(A,B-A).

main() ->
  N = 1500000,
  E = euclid(N),
  Counts = count_sums(E),
  Singles =
  length(lists:filter(fun(C) -> C =:= 1 end, gb_trees:values(Counts))),
  io:format("~p~n", [Singles]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

euclid_test_() ->
  [
   ?_assertEqual([{1,2,1}, {2,2,1}], euclid(24))
  ].

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1,1)),
   ?_assertEqual(2, gcd(2,8)),
   ?_assertEqual(2, gcd(8,2)),
   ?_assertEqual(3, gcd(9,21)),
   ?_assertEqual(3, gcd(21,9))
  ].

-endif.
