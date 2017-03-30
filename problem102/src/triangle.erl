-module(triangle).

-export(
   [
    count/2,
    main/0,
    origin_in_triangle_barycentric/3,
    origin_in_triangle_dot_product/3,
    origin_in_triangle_parametric/3,
    read_triangles/1
   ]
  ).

-define(TRIANGLES, "p102_triangles.txt").

count(Test, Triangles) ->
  length(
    lists:filter(
      fun({P1, P2, P3}) ->
          Test(P1, P2, P3)
      end,
      Triangles)).

main() ->
  Triangles = read_triangles(?TRIANGLES),
  io:format("~p~n", [count(fun origin_in_triangle_barycentric/3, Triangles)]),
  io:format("~p~n", [count(fun origin_in_triangle_parametric/3, Triangles)]),
  io:format("~p~n", [count(fun origin_in_triangle_dot_product/3, Triangles)]),
  ok.

origin_in_triangle_barycentric({X1, Y1}, {X2, Y2}, {X3, Y3}) ->
  Denominator = ((Y2 - Y3) * (X1 - X3)) + ((X3 - X2) * (Y1 - Y3)),
  A = (X2*Y3 - X3*Y2) / Denominator,
  B = (X3*Y1 - X1*Y3) / Denominator,
  C = 1 - A - B,
  A >= 0 andalso A =< 1 andalso
  B >= 0 andalso B =< 1 andalso
  C >= 0 andalso C =< 1.

subtract({X1, Y1}, {X2, Y2}) ->
  {X1 - X2, Y1 - Y2}.

cross({X1, Y1}, {X2, Y2}) ->
  X1 * Y2 - X2 * Y1.

origin_in_triangle_dot_product(P1, P2, P3) ->
  Side =
  fun({PX1, PY1}, {PX2, PY2}) ->
      PX1*(PY1 - PY2) + PY1*(PX2 - PX1)
  end,
  Cross = cross(subtract(P2, P1), subtract(P3, P2)),
  [O1, O2, O3] =
  if
    Cross > 0 ->
      [P3, P2, P1];
    true ->
      [P1, P2, P3]
  end,
  Side(O1, O2) >= 0 andalso
  Side(O2, O3) >= 0 andalso
  Side(O3, O1) >= 0.

origin_in_triangle_parametric({X1, Y1}, {X2, Y2}, {X3, Y3}) ->
  Denominator = X1*(Y2-Y3) + Y1*(X3-X2) + X2*Y3 - Y2*X3,
  T1 = (X3*Y1 - X1*Y3)/Denominator,
  T2 = (X1*Y2 - X2*Y1)/Denominator,
  S = T1 + T2,
  T1 >= 0 andalso T1 =< 1 andalso T2 >= 0 andalso T2 =< 1 andalso S =< 1.

read_triangles(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  Lines = re:split(Binary, "\n", [{return, list}, trim]),
  lists:map(
    fun(Line) ->
        [X1, Y1, X2, Y2, X3, Y3] =
        [list_to_integer(X) || 
         X <- re:split(Line, ",", [{return, list}, trim])],
        {{X1, Y1}, {X2, Y2}, {X3, Y3}}
    end,
    Lines).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

origin_in_triangle_test_() ->
  {A1, A2, A3} = {{-340, 495}, {-153, -910}, {835, -947}},
  {B1, B2, B3} = {{-175,  41}, {-421, -714}, {574, -645}},
  [
   ?_assert(origin_in_triangle_barycentric(A1, A2, A3)),
   ?_assertNot(origin_in_triangle_barycentric(B1, B2, B3)),
   ?_assert(origin_in_triangle_parametric(A1, A2, A3)),
   ?_assertNot(origin_in_triangle_parametric(B1, B2, B3)),
   ?_assert(origin_in_triangle_dot_product(A1, A2, A3)),
   ?_assertNot(origin_in_triangle_dot_product(B1, B2, B3))
  ].

-endif.
