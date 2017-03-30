-module(intersection).

-export(
   [
    hexagon/1,
    main/0,
    pentagon/1,
    triangle/1,
    walk/3
   ]
  ).

hexagon(N) when N >= 1 ->
  N*(2*N - 1).

main() ->
  io:format("~p~n", [walk(1,1,1)]),
  io:format("~p~n", [walk(2,1,1)]),
  io:format("~p~n", [walk(286,165,143)]),
  ok.

pentagon(N) when N >= 1 ->
  N*(3*N - 1) div 2.

triangle(N) when N >= 1 ->
  N*(N + 1) div 2.

walk(Nt, Np, Nh) ->
  walk({{Nt, triangle(Nt)}, {Np, pentagon(Np)}, {Nh, hexagon(Nh)}}).

walk({{_,N}, {_,N}, {_,N}} = Ngon) ->
  Ngon;
walk({{N1,Tn}, {N2,Pn}, {N3,Hn}}) ->
  if
    Tn =< Pn andalso Tn =< Hn ->
      walk({{N1+1,Tn + N1 + 1}, {N2,Pn}, {N3,Hn}});
    Pn =< Tn andalso Pn =< Hn ->
      walk({{N1,Tn}, {N2+1,Pn + 3*N2 + 1}, {N3,Hn}});
    Hn =< Tn andalso Hn =< Pn ->
      walk({{N1,Tn}, {N2,Pn}, {N3+1,Hn + 4*N3 + 1}})
  end.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

hexagon_test_() ->
  [
   ?_assertEqual([1,6,15,28,45], [hexagon(N) || N <- lists:seq(1,5)])
  ].

pentagon_test_() ->
  [
   ?_assertEqual([1,5,12,22,35], [pentagon(N) || N <- lists:seq(1,5)])
  ].

triangle_test_() ->
  [
   ?_assertEqual([1,3,6,10,15], [triangle(N) || N <- lists:seq(1,5)])
  ].


-endif.
