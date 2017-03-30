-module(root).

-export(
   [
    expand/1,
    main/0
   ]
  ).

expand(N) when N >= 1 ->
  expand(N, 3, 2, []).

expand(0, _A, _B, Acc) ->
  lists:reverse(Acc);
expand(N, A, B, Acc) ->
  expand(N-1, A+2*B, A+B, [{A,B}|Acc]).

main() ->
  Expansions = expand(1000),
  Filtered = 
  lists:filter(
    fun({A,B}) ->
        length(integer_to_list(A)) > length(integer_to_list(B))
    end,
    Expansions),
  io:format("~p~n", [length(Filtered)]),
  ok.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

expand_test_() ->
  [
   ?_assertEqual([{3,2}], expand(1)),
   ?_assertEqual([{3,2}, {7,5}], expand(2)),
   ?_assertEqual(
      [{3,2}, {7,5}, {17,12}, {41,29}, 
       {99,70}, {239,169}, {577,408}, {1393,985}], 
      expand(8))
  ].

-endif.
