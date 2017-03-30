-module(pf).

-export(
   [
    fibonacci/1,
    fib9/1,
    find/2,
    head/2,
    main/0,
    tail/2
   ]
  ).

fibonacci(N) when N >= 1 ->
  fibonacci(1, N, {1,1}).

fibonacci(N, N, {F1, _F2}) ->
  F1;
fibonacci(I, N, {F1, F2}) ->
  fibonacci(I+1, N, {F2, F1+F2}).

fib9({F1, F2}) ->
  {F1, {F2, (F1 + F2) rem 1000000000}}.

find(From, To) ->
  find(From, To, {1,1}).

find(From, To, _Seed) when From > To ->
  false;
find(From, To, Seed) ->
  {Fn, Next} = fib9(Seed),
  Fibonacci = integer_to_list(Fn),
  Found =
  case length(Fibonacci) >= 9 of
    true ->
      case "123456789" =:= lists:sort(Fibonacci) of
        true ->
          Head = head(integer_to_list(fibonacci(From)), 9),
          case "123456789" =:= lists:sort(Head) of
            true ->
              {true, From};
            false ->
              false
          end;
        false ->
          false
      end;
    false ->
      false
  end,
  case Found of
    false ->
      find(From + 1, To, Next);
    Result ->
      Result
  end.

head(List, Len) ->
  lists:sublist(List, Len).

main() ->
  case find(1, 1000000) of
    false ->
      io:format("not found~n", []);
    {true, Found} ->
      io:format("found: ~p~n", [Found])
  end,
  ok.

tail(List, Len) ->
  lists:sublist(List, length(List) - Len + 1, Len).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

fib9_test_() ->
  [
   ?_assertEqual({1, {1, 2}}, fib9({1,1})),
   ?_assertEqual(
      {fibonacci(539), 
       {fibonacci(540), fibonacci(541) rem 1000000000}}, 
      fib9({fibonacci(539),fibonacci(540)}))
  ].

fibonacci_test_() ->
  [
   ?_assertEqual(1, fibonacci(1)),
   ?_assertEqual(1, fibonacci(2)),
   ?_assertEqual(2, fibonacci(3)),
   ?_assertEqual(3, fibonacci(4)),
   ?_assertEqual(5, fibonacci(5)),
   ?_assertEqual(8, fibonacci(6)),
   ?_assertEqual(13, fibonacci(7)),
   ?_assertEqual(21, fibonacci(8)),
   ?_assertEqual(34, fibonacci(9)),
   ?_assertEqual(fibonacci(5000), fibonacci(4998) + fibonacci(4999))
  ].

head_test_() ->
  [
   ?_assertEqual("1", head("12345", 1)),
   ?_assertEqual("12", head("12345", 2)),
   ?_assertEqual("123", head("12345", 3)),
   ?_assertEqual("1234", head("12345", 4)),
   ?_assertEqual("12345", head("12345", 5))
  ].

tail_test_() ->
  [
   ?_assertEqual("5", tail("12345", 1)),
   ?_assertEqual("45", tail("12345", 2)),
   ?_assertEqual("345", tail("12345", 3)),
   ?_assertEqual("2345", tail("12345", 4)),
   ?_assertEqual("12345", tail("12345", 5))
  ].

-endif.
