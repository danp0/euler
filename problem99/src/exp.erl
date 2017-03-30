-module(exp).

-export(
   [
    main/0,
    max/1,
    read/1
   ]
  ).

-define(BaseExpFile, "p099_base_exp.txt").

main() ->
  BaseExp = read(?BaseExpFile),
  io:format("~p~n", [max(BaseExp)]),
  ok.

max(Exponents) ->
  {_Log, Index} =
  lists:max(
    lists:zip(
      lists:map(fun({Base, Exp}) -> Exp * math:log(Base) end, Exponents),
      lists:seq(1, length(Exponents)))),
  Index.

read(Filename) ->
  {ok, Binary} = file:read_file(Filename),
  lists:map(
    fun(Line) ->
        [Base, Exp] = re:split(Line, ",", [{return, list}]),
        {list_to_integer(Base), list_to_integer(Exp)}
    end,
    re:split(Binary, "\n", [trim, {return, list}])).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

max_test_() ->
  [
   ?_assertEqual(5, max([{2,1}, {2,2}, {2,3}, {2,4}, {2,5}]))
  ].

-endif.
