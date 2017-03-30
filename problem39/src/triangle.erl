-module(triangle).

-export(
   [
    find/0,
    find/1,
    generate_triangle_fun/1,
    main/0
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

find() ->
  Triangles =
  [{P, find(P)} || P <- lists:seq(4, 1000, 2)],
  Triangles1 = 
  lists:filter(
    fun({_P, L}) -> length(L) =/= 0 end, 
    Triangles),
  lists:sort(
    fun({_P1, L1}, {_P2, L2}) -> length(L1) =< length(L2) end, 
    Triangles1).

find(Perimeter) ->
  Tfun = generate_triangle_fun(Perimeter),
  Seq = lists:seq(1,Perimeter-2),
  [{A, B, Perimeter - (A+B)} || A <- Seq, B <- Seq, A < B, Tfun(A,B)].

generate_triangle_fun(Perimeter) ->
  PerimeterSquared = Perimeter * Perimeter,
  fun(A, B) ->
      PerimeterSquared - 2*Perimeter*(A + B) + 2*A*B =:= 0
  end.
      
main() ->
  io:format("triangle...~n", []),
  io:format("~p~n", [find()]),
  ok.

%%
%% unit test
%%
generate_triangle_fun_test_() ->
  Tfun = generate_triangle_fun(120),
  [
   ?_assert(Tfun(20,48)),
   ?_assert(Tfun(24,45)),
   ?_assert(Tfun(30,40))
  ].
