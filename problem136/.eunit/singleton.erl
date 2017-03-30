-module(singleton).

-export(
   [
    count/1,
    main/0
   ]
  ).
%%
%% n =
%% (x+2m)^2 - (x+m)^2 - x^2 =
%% x^2 + 4mx + 4m^2 - (x^2 + 2mx + m^2) - x^2 =
%% 3m^2 + 2mx - x^2 =
%% (3m - x)(m + x)
%%
%% u = 3m - x
%% v = m + x
%%
%% m = (u+v)/4
%% x = (3v-u)/4
%%

count(Upto) ->
  count(1, Upto, gb_trees:empty()).

count(U, Upto, Ack) when U >= Upto ->
  gb_trees:to_list(Ack);
count(U, Upto, Ack) ->
  count(U+1, Upto, count(U, 1, Upto, Ack)).

count(U, V, Upto, Ack) when U*V >= Upto ->
  Ack;
count(U, V, Upto, Ack) ->
  Next =
  if
    3*V > U andalso
    (U+V) rem 4 =:= 0 andalso
    (3*V-U) rem 4 =:= 0 ->
      N = U*V,
      case gb_trees:lookup(N, Ack) of
        none ->
          gb_trees:enter(N, 1, Ack);
        {value, Value} ->
          gb_trees:enter(N, Value + 1, Ack)
      end;
    true ->
      Ack
  end,
  count(U, V+1, Upto, Next).

main() ->
  io:format("singleton...~n", []),
  Count = count(50000000),
  io:format("~p~n", 
            [length(lists:filter(fun({_N, C}) -> C =:= 1 end, Count))]),
  ok.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

count_test_() ->
  [
   ?_assertEqual(
      25,
      length(
        lists:filter(
          fun
            ({_N, 1}) -> true;
            ({_N, _C}) -> false
          end,
          count(100)
        )
       )
     )
  ].

-endif.
