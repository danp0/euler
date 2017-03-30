-module(triangle).

-export(
   [
    distance_squared/2,
    find/2,
    is_right_triangle/3,
    main/0
   ]
  ).

distance_squared({X1, Y1}, {X2, Y2}) ->
  Dx = X2 - X1,
  Dy = Y2 - Y1,
  Dx*Dx + Dy*Dy.

find(ToX, ToY) ->
  lists:usort(
    [lists:sort([{X1, Y1}, {X2, Y2}]) ||
     X1 <- lists:seq(0,ToX), 
     Y1 <- lists:seq(0,ToY),
     X2 <- lists:seq(0,ToX),
     Y2 <- lists:seq(0,ToY),
     {X1, Y1} =/= {X2, Y2},
     {X1, Y1} =/= {0, 0},
     {X2, Y2} =/= {0, 0},
     is_right_triangle({0,0}, {X1,Y1}, {X2,Y2})]).

is_right_triangle(P1, P2, P3) ->
  [A, B, C] = 
  lists:sort(
    [
     distance_squared(P1, P2), 
     distance_squared(P1, P3), 
     distance_squared(P2, P3)]),
  A + B =:= C.
  
main() ->
  Triangles = find(50,50),
  io:format("~p~n", [length(Triangles)]),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

distance_squared_test_() ->
  [
   ?_assertEqual(2, distance_squared({0, 0}, {1, 1})),
   ?_assertEqual(2, distance_squared({1, 1}, {0, 0})),
   ?_assertEqual(8, distance_squared({0, 1}, {2, 3}))
  ].

is_right_triangle_test_() ->
  [
   ?_assert(is_right_triangle({0,0}, {1,0}, {0,1})),
   ?_assertNot(is_right_triangle({0,0}, {1,2}, {2,0}))
  ].

-endif.
