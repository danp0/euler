-module(fibo).

-export(
   [
    sum/4,
    main/0
   ]
  ).

sum(F1, _F2, Max, Sum) when F1 >= Max ->
  Sum;
sum(F1, F2, Max, Sum) ->
  case F1 rem 2 =:= 0 of
    true ->
      sum(F2, F1 + F2, Max, Sum + F1);
    false ->
      sum(F2, F1 + F2, Max, Sum)
  end.

main() ->
  io:format("~p~n", [sum(1, 2, 4000000, 0)]).
