-module(disc).

-export(
   [
    bstr/2,
    count_digits/2,
    fact/1,
    main/0,
    p/1,
    pow/2
   ]
  ).

%%
%% p(1) = 1/2!
%% p(2) = 1/3!
%% p(3) = p(011) + p(101) + p(110) + p(111)
%%      = (1 + 2 + 3 + 1)/4!
%%      = 7/4!
%% p(4) = p(0111) + p(1011) + p(1101) + p(1110) + p(1111)
%%      = (1 + 2 + 3 + 4 + 1)/5!
%%      = 11/5!
%% p(5) = 
%% p(00111) 1*2 2
%% p(01011) 1*3 3
%% p(01101) 1*4 4
%% p(01110) 1*5 5
%% p(01111) 1   1
%% p(10011) 2*3 6
%% p(10101) 2*4 8
%% p(10110) 2*5 10
%% p(10111) 2   2
%% p(11001) 3*4 12
%% p(11010) 3*5 15
%% p(11011) 3   3
%% p(11100) 4*5 20
%% p(11101) 4   4
%% p(11110) 5   5
%% p(11111) 1   1 = 101/6!
%%

bstr(N, Width) ->
  lists:flatten(io_lib:format("~*.2.0B", [Width, N])).

count_digits(Str, Digit) ->
  length(Str) - length([D || D <- Str, D =/= Digit]).

fact(N) ->
  fact(N, 1).

fact(0, Ack) ->
  Ack;
fact(1, Ack) ->
  Ack;
fact(N, Ack) ->
  fact(N-1, N*Ack).

main() ->
  lists:foreach(
    fun(I) ->
        {A, B} = p(I),
        io:format("p(~p): ~p/~p, ~p~n", [I, A, B, B div A])
    end,
    lists:seq(15,15)),
  ok.

p(N) when N >= 1 ->
  {p(0, pow(2, N) - 1, N, 0), fact(N+1)}.

p(I, To, _N, Ack) when I > To ->
  Ack;
p(I, To, N, Ack) ->
  Binary = bstr(I, N),
  Zeros = count_digits(Binary, $0),
  Next =
  case N > 2 * Zeros of
    true ->
      lists:foldl(
        fun
        ({$0, D}, Prod) ->
            D * Prod;
        ({$1, _D}, Prod) ->
            Prod
        end,
        1,
        lists:zip(Binary, lists:seq(1,N)));
    false ->
      0
  end,
  p(I+1, To, N, Ack + Next).

pow(_A, 0) ->
  1;
pow(A, 1) ->
  A;
pow(A, N) when N rem 2 =:= 1 ->
  A * pow(A, N-1);
pow(A, N) ->
  A2 = pow(A, N div 2),
  A2 * A2.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

bstr_test_() ->
  [
   ?_assertEqual(["000", "001", "010", "011", "100", "101", "110", "111"],
                 [bstr(I,3) || I <- lists:seq(0,7)])
  ].

count_digits_test_() ->
  [
   ?_assertEqual([0, 1, 1, 2, 1, 2, 2, 3],
                 [count_digits(bstr(I,3), $1) || I <- lists:seq(0,7)])
  ].

fact_test_() ->
  [
   ?_assertEqual([1, 1, 2, 6, 24, 120],
                 [fact(I) || I <- lists:seq(0,5)])
  ].

p_test_() ->
  [
   ?_assertEqual({1,2}, p(1)),
   ?_assertEqual({1,6}, p(2)),
   ?_assertEqual({7,24}, p(3)),
   ?_assertEqual({11,120}, p(4)),
   ?_assertEqual({101,720}, p(5))
  ].

pow_test_() ->
  [
   ?_assertEqual([1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024],
                 [pow(2,N) || N <- lists:seq(0,10)])
  ].

-endif.
