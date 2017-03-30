-module(coin).

-export(
   [
    count/2,
    main/0
   ]
  ).

count(Total, Coins) when Total < 0 orelse Coins =:= [] ->
  0;
count(0, _Coins) ->
  1;
count(Total, [H|T] = Coins) ->
  count(Total, T) + count(Total - H, Coins).

main() ->
  Total = 200,
  Coins = [1,2,5,10,20,50,100,200],
  io:format("~p~n", [count(Total, Coins)]),
  ok.
