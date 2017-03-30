-module(triplet).

-export(
   [
    main/0,
    triples/0,
    triplet/0,
    triplet2/0
   ]
  ).

%%
%% a^2 + b^2 = c^2
%%
%% a + b + c = 1000
%%
%% a^2 = (c - b)(c + b)
%%
main() ->
  io:format("triplet...~n", []),
  square:start(),
  {Time, [{A0, B0, C0}|_]} = timer:tc(triplet, triplet, []),
  io:format("triplet: ~p microseconds~n", [Time]),
  io:format("triplet: ~p + ~p + ~p = ~p~n", [A0, B0, C0, A0 + B0 + C0]),
  io:format("triplet: ~p + ~p = ~p~n", [square:sqr(A0), square:sqr(B0), square:sqr(C0)]),
  io:format("triplet: ~p * ~p * ~p = ~p~n", [A0, B0, C0, A0 * B0 * C0]),
  {Time1, [{A1, B1, C1}|_]} = timer:tc(triplet, triplet, []),
  io:format("triplet: ~p microseconds~n", [Time1]),
  io:format("triplet: ~p, ~p, ~p~n", [A1, B1, C1]),
  {Time2, [{A2, B2, C2}|_]} = timer:tc(triplet, triplet2, []),
  io:format("triplet: ~p microseconds~n", [Time2]),
  io:format("triplet: ~p, ~p, ~p~n", [A2, B2, C2]),
  square:stop(),
  [{A, B, C}|_] = lists:filter(                                                     
                    fun({A, B, C}) -> A*A + B*B =:= C*C end,                                        
                    [{A, B, 1000 - (A + B)} || A <- lists:seq(1, 998), B <- lists:seq(1, 999 - A)]),
  io:format("~p~n", [A * B *C]).

triples() ->
  [{A, B, 1000 - (A + B)} || A <- lists:seq(1, 998), B <- lists:seq(1, 999 - A)].

triplet() ->
  Triples = triples(),
  lists:filter(
    fun({A, B, C}) -> 
        square:sqr(C) =:= square:sqr(A) + square:sqr(B)
    end,
    Triples).

triplet2() ->
  Triples = triples(),
  lists:filter(
    fun({A, B, C}) ->
        C * C =:= A * A + B * B
    end,
    Triples).

