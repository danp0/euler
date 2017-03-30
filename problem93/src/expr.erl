-module(expr).

-export(
   [
    consecutive/1,
    eval/1,
    gcd/2,
    generate/1,
    main/0,
    reduce/1,
    sequence/1
   ]
  ).

consecutive(L) ->
  lists:reverse(
    lists:foldl(
      fun
        (E, []) ->
          [E];
        (E, [{A,B}|T]) when E =:= B+1 ->
          [{A,B+1}|T];
        (E, [A|T]) when E =:= A+1 ->
          [{A,A+1}|T];
        (E, Acc) ->
          [E|Acc]
      end,
      [],
      L)).

eval(Expr) ->
  reduce(evaluate(Expr)).

evaluate(N) when is_number(N) ->
  {N, 1};
evaluate({A, B}) ->
  {A, B};
evaluate({'*', E1, E2}) ->
  multiply(evaluate(E1), evaluate(E2));
evaluate({'/', E1, E2}) ->
  divide(evaluate(E1), evaluate(E2));
evaluate({'+', E1, E2}) ->
  add(evaluate(E1), evaluate(E2));
evaluate({'-', E1, E2}) ->
  subtract(evaluate(E1), evaluate(E2)).

generate([A, B]) ->
  lists:flatten(
    [
     [{Operator, A, B}, {Operator, B, A}] || 
     Operator <- ['*', '/', '+', '-']]);
generate([A, B, C]) ->
  lists:flatten(
    [
     [{Operator, E1, E2}, {Operator, E2, E1}] || 
     Operator <- ['*', '/', '+', '-'], 
     E1 <- [A, B, C],
     E2 <- generate([A, B, C] -- [E1])]);
generate([A, B, C, D]) ->
  lists:flatten(
    [
     [{Operator, E1, E2}, {Operator, E2, E1}] ||
     Operator <- ['*', '/', '+', '-'],
     E1 <- [A, B, C, D],
     E2 <- generate([A, B, C, D] -- [E1])]).

multiply({A1, B1}, {A2, B2}) ->
  {A1*A2, B1*B2}.

divide({A1, B1}, {A2, B2}) ->
  multiply({A1, B1}, {B2, A2}).

add({A1, B1}, {A2, B2}) ->
  {A1*B2 + A2*B1, B1*B2}.

subtract({A1, B1}, {A2, B2}) ->
  {A1*B2 - A2*B1, B1*B2}.

gcd(A, 0) ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).

main() ->
  Eval =
  lists:foldl(
    fun({{A,B},L1}, {{C,D},L2}) ->
        case B > D of
          true ->
            {{A,B},L1};
          false ->
            {{C,D},L2}
        end
    end,
    {{0,0}, [0,0,0,0]},
    [
     {hd(sequence([A,B,C,D])),[A,B,C,D]} ||
     A <- lists:seq(1,   9),
     B <- lists:seq(A+1, 9),
     C <- lists:seq(B+1, 9),
     D <- lists:seq(C+1, 9)
    ]),
  io:format("~p~n", [Eval]),
  ok.

reduce(N) when is_number(N) ->
  N;
reduce({A, B}) ->
  Gcd = gcd(A, B),
  A1 = A div Gcd,
  B1 = B div Gcd,
  case B1 =:= 1 of
    true ->
      A1;
    false ->
      {A1, B1}
  end.

sequence(Digits) ->
  consecutive(
    lists:usort(
      lists:filter(
        fun(E) -> is_number(E) andalso E > 0 end, 
        lists:map(fun eval/1, generate(Digits))))
   ).

%%
%% unit tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

consecutive_test_() ->
  [
   ?_assertEqual([], consecutive([])),
   ?_assertEqual([1], consecutive([1])),
   ?_assertEqual([{1,2},5], consecutive([1,2,5])),
   ?_assertEqual([{1,2},{5,7},10], consecutive([1,2,5,6,7,10]))
  ].

eval_test_() ->
  [
   ?_assertEqual(16, eval({'*', 2, 8})),
   ?_assertEqual({1,2}, eval({'/', 8, 16})),
   ?_assertEqual(2, eval({'+', 1, 1})),
   ?_assertEqual(0, eval({'-', 1, 1})),
   ?_assertEqual(8, eval({'/', {'*', 4, {'+', 1, 3}}, 2})),
   ?_assertEqual(14, eval({'*', 4, {'+', 3, {'/', 1, 2}}})),
   ?_assertEqual(19, eval({'-', {'*', 4, {'+', 2, 3}}, 1})),
   ?_assertEqual(36, eval({'*', 3, {'*', 4, {'+', 2, 1}}}))
  ].

gcd_test_() ->
  [
   ?_assertEqual(1, gcd(1,1)),
   ?_assertEqual(8, gcd(8, 16)),
   ?_assertEqual(8, gcd(16, 8))
  ].

reduce_test_() ->
  [
   ?_assertEqual(1, reduce(1)),
   ?_assertEqual(1, reduce({2,2})),
   ?_assertEqual({1,2}, reduce({1,2})),
   ?_assertEqual({1,2}, reduce({5, 10})),
   ?_assertEqual(2, reduce({10, 5})),
   ?_assertEqual(-2, reduce({-2, 1})),
   ?_assertEqual(-2, reduce({2, -1})),
   ?_assertEqual(2, reduce({-2, -1}))
  ].

-endif.
