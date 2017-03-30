-module(names).

-export(
   [
    alpha_value/1,
    main/0,
    read_names/0,
    score_names/1
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

alpha_value(Name) ->
  lists:foldl(
    fun(C, Sum) when C >= $A andalso C =< $Z ->
        Sum + (C - $A) + 1
    end,
    0,
    Name).

main() ->
  io:format("names...~n", []),
  Names = read_names(),
  io:format("~p~n", [score_names(Names)]).

read_names() ->
  {ok, Binary} = file:read_file("names.txt"),
  NamesStr = lists:filter(fun(C) -> [C] =/= "\"" end, binary_to_list(Binary)),
  Names = string:tokens(NamesStr, ","),
  lists:sort(Names).

score_names(Names) ->
  {Scores, _} = lists:mapfoldl(
    fun(N, I) ->
        {I * alpha_value(N), I+1}
    end,
    1,
    Names),
  lists:sum(Scores).

%%
%% unit tests
%%
alpha_value_test_() ->
  [
   ?_assertEqual(1, alpha_value("A")),
   ?_assertEqual(3, alpha_value("AB")),
   ?_assertEqual(6, alpha_value("ABC"))
  ].
