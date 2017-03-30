-module(triangle).

-export(
   [
    find/1,
    is_square/1,
    main/0
   ]
  ).

%%
%%    ^
%% c /|\ c 
%%  / | \
%% / b|  \
%%  -----
%%  a   a
%%
%%  a^2 + b^2 = c^2
%%  |c - 2a| =< 1
%%  P = 2a + 2c = 2(a + c) < 1,000,000,000
%%      a + c < 500,000,000
%%  A = ab
%%
%%  c = 2a
%%  b^2 = 3a^2
%%  a < 500,000,000 / 3 
%%
%%  c = 2a - 1
%%  b^2 = 3a^2 - 4a + 1 = (3a - 1)(a - 1)
%%  a < 500,000,001 / 3
%%
%%  c = 2a + 1
%%  b^2 = 3a^2 + 4a + 1 = (3a + 1)(a + 1)
%%  a < 499,999,999 / 3
%%

find(Max) ->
  find(1, Max, []).

find(N, N, Acc) ->
  lists:flatten(Acc);
find(A, N, Acc) ->
  ThreeASqr = 3 * A * A,
  TwoA = 2 * A,
  FourA = 4 * A,
  Acc2 =
  lists:foldl(
    fun
      ({false, _C}, Acc0) ->
        Acc0;
      ({0, _}, Acc0) ->
        Acc0;
      ({B, C}, Acc0) ->
        [{A,B,C} | Acc0]
    end,
    Acc,
    [{is_square(ThreeASqr), TwoA}, 
     {is_square(ThreeASqr - FourA + 1), TwoA - 1}, 
     {is_square(ThreeASqr + FourA + 1), TwoA + 1}]),
  find(A+1, N, Acc2).

is_square(N) ->
  Sqrt = trunc(math:sqrt(N)),
  case N =:= (Sqrt * Sqrt) of
    true ->
      Sqrt;
    false ->
      false
  end.

main() ->
  Triangles = find(500000001 div 3),
  io:format("~p~n", [Triangles]),
  io:format("~p~n", 
            [
             lists:sum(lists:map(fun({A, _, C}) -> 2*(A+C) end, Triangles))
            ]
           ),
  ok.

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_square_test_() ->
  [
   ?_assertEqual(1, is_square(1)),
   ?_assertEqual(false, is_square(2)),
   ?_assertEqual(2, is_square(4)),
   ?_assertEqual(4, is_square(16)),
   ?_assertEqual(5, is_square(25))
  ].

-endif.
