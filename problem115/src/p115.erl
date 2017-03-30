-module(p115).

-export(
   [
    result/2
   ]
  ).

iter([R,B|T]) ->
  [Blast|BT] = lists:reverse(T),
  [R+Blast,R+B,B|lists:reverse(BT)].

loop(Bound, N, Curr) ->
  [R,B|_] = Curr,
  if
    R+B > Bound ->
      N;
    true ->
      loop(Bound, N+1, iter(Curr))
  end.

result(M, Bound) ->
  loop(Bound, 0, [0,1|lists:duplicate(M-1,0)]).

