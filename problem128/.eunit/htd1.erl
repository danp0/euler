-module(htd1).

-export(
   [
    init/1,
    is_prime/1,
    main/0,
    pd/1,
    ring/1,
    ring_around/1,
    ring_initial/1,
    ring_length/1,
    ring_max/1,
    ringof/1,
    sieve/1,
    start/1,
    stop/0
   ]
  ).

-record(
   state,
   {
    adjacent = gb_trees:empty(),
    primes = gb_sets:empty()
   }
  ).

%%
%% level  start  length  sequence
%% -----  -----  ------  --------
%% 1      1      1       1..1
%% 2      2      6       2..7
%% 3      8      12      8..19
%% 4      20     18      20..37
%% 5      38     24      38..61
%% 6      62     30      62..91
%% 7      92     36      92..127
%%
%% n    3 *    6(n-1)
%%      (n-1) *
%%      (n-2) +
%%      2
%%
%% ring of
%%
%% 3n^2 - 9n + 8 - x = 0
%% 
%% x = 9 +- sqrt(81 - 12(8-x))/6
%%
%% n^2 - 3n + 2
%% 
%% 0  0   2 n^2 - 3n + 2  = 0
%% 6  2   3 n^2 - 3n + 0  = 0
%% 18 6   4 n^2 - 3n - 4  = 0
%% 36 12  5 n^2 - 3n - 10 = 0
%% 60 20  6 n^2 - 3n - 18 = 0
%% 90 30  7 n^2 - 3n - 28 = 0
%% 
%% 
%% next in ring + previous in ring + ring - 1 + 3 ring + 1
%%
%% 1: [1]
%% 2: [2,3,4,5,6,7]
%% 3: [8,9,10,11,12,13,14,15,16,17,18,19]
%% 4: [20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37]
%% 5: [38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61]
%%
%%             38
%%          39    61
%%       40    20    60
%%    41    21    37    59
%% 42    22     8    36    58
%%    23     9    19    35
%% 43    10     2    18    57                   
%%    24     3     7    34   
%% 44    11     1    17    56
%%    25     4     6    33   
%% 45    12     5    16    55
%%    26    13    15    32
%% 46    27    14    31    54
%%    47    28    30    53
%%       48    29    52
%%          49    51
%%             50
%%
%%
%%
%%             3,1
%%         3,2     3,12
%%     3,3     2,1     3,11
%%         2,2     2,6
%%     3,4     1,1     3,10
%%         2,3     2,5
%%     3,5     2,4     3,9
%%         3,6     3,8
%%             3,7
%%
%%                                         0, 2,-2
%%
%%                                         0, 1,-1
%%                                 -1,1,0           1,1,-2
%%                                         0, 0, 0
%%                                 -1,0,1           1,0,-1
%%                                         0,-1, 1
%%                                -1,-1,2           1,-1,0
%%                                         0,-2, 2
%%
%%
%%
%%                                             1
%%                   2         (3 4)           5           (6 7)
%%                  8 9     (10 11 12)     13 14 15     (16 17 18)    19
%%               20 21 22 (23 24 25 26) 27 28 29 30 31 (32 33 34 35) 36 37
%%
%% 2: 6
%% 3: 3 3 3 3 3 3
%% 4: 3 2 3 2 3 2 3 2 3 2 3 2
%% 5: 3 2 2 3 2 2 3 2 2 3 2 2 3 2 2 3 2 2
%%

init(NPrime) ->
  io:format("init: ~p~n", [NPrime]),
  loop(#state{primes = gb_sets:from_list(sieve(NPrime))}).

is_prime(N) ->
  send({is_prime, N}).

loop(State) ->
  NewState =
  receive
    {From, {add, C1, C2}} ->
      Entry =
      case gb_trees:lookup(C1, State#state.adjacent) of
        none ->
          [C2];
        {value, Value} ->
          [C2|Value]
      end,
      Adjacent = gb_trees:enter(C1, Entry, State#state.adjacent),
      From ! ok,
      State#state{adjacent=Adjacent};
    {From, {is_prime, N}} ->
      From ! gb_sets:is_member(N, State#state.primes),
      State;
    {From, keys} ->
      From ! gb_trees:keys(State#state.adjacent),
      State;
    {From, {lookup, C}} ->
      From ! gb_trees:lookup(C, State#state.adjacent),
      State;
    {From, sort} ->
      Adjacent = 
      gb_trees:map(fun(_K, V) -> lists:sort(V) end, State#state.adjacent),
      From ! ok,
      State#state{adjacent=Adjacent};
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NewState).

adjacent(1, _Prev, _Next) ->
  ok;
adjacent(I, Prev, Next) ->
  [L|_T] = lists:reverse(Next),
  Next1 = [L | Next],
  below(I, Prev, Next1),
  next_to(hd(Next), Next).

below(2, Prev, Next) ->
  below(1, [6], Prev, Next);
below(I, Prev, Next) ->
  below(1, [3 | lists:duplicate(I-3,2)], Prev, Next).

below(_I, _Seq, [], _Next) ->
  ok;
below(_I, _Seq, _Prev, []) ->
  ok;
below(I, Seq, Prev, Next) when I > length(Seq) ->
  below(1, Seq, Prev, Next);
below(I, Seq, [P|T], Next) ->
  Repeat = lists:nth(I, Seq),
  case Repeat > length(Next) of
    true ->
      ok;
    false ->
      Sublist = lists:sublist(Next, Repeat),
      lists:foreach(
        fun(E) ->
            send({add, P, E}),
            send({add, E, P})
        end,
        Sublist),
      below(I+1, Seq, T, lists:sublist(Next, Repeat, length(Next)))
  end.

next_to(Head, [E]) ->
  send({add, E, Head}),
  send({add, Head, E});
next_to(Head, [H|T]) ->
  send({add, H, hd(T)}),
  send({add, hd(T), H}),
  next_to(Head, T).

main() ->
  io:format("hexagonal tile difference...~n", []),
  lists:foreach(
    fun(I) ->
        io:format("~p: ~w~n", [I, ring(I)])
    end,
    lists:seq(1,20)),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("~p: ~p~n", [I, ringof(I)])
  %%  end,
  %%  lists:seq(1,20)),
  %%start(100),
  %%ring_around(10),
  %%send(sort),
  %%io:format("~p~n", [send(keys)]),
  %%ring_around(6,6,ring(5)),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("~p: ~p~n", [I, send({lookup, I})])
  %%  end,
  %%  lists:seq(1,100)),
  %%lists:foreach(
  %%  fun(I) ->
  %%      io:format("is_prime(~p): ~p~n", [I, is_prime(I)])
  %%  end,
  %%  lists:seq(1,10)),
  %%ok = stop(),
  N = 1000,
  PD = pd(N),
  %io:format("~p: ~p~n", [N, PD]),
  PD3 = lists:filter(fun({_,E}) -> E =:= 3 end, PD),
  %{Indexes,_} = lists:unzip(PD3),
  io:format("~p: ~p~n", [N, PD3]),
  %io:format("~p~n", [diff(Indexes)]),
  %io:format("~p~n", [length(PD3)]),
  ok.

%diff(L) ->
%  diff(L, []).

%diff([], Ack) ->
%  lists:reverse(Ack);
%diff([_E], Ack) ->
%  diff([], Ack);
%diff([H1, H2 | T], Ack) ->
%  diff([H2 | T], [H2 - H1 | Ack]).

pd(N) ->
  Ring = ringof(N) + 1,
  Max = ring_max(Ring),
  start(Max),
  ring_around(Ring),
  send(sort),
  PD = pd(1,N, []),
  stop(),
  PD.

pd(I, N, Ack) when I > N ->
  lists:reverse(Ack);
pd(I, N, Ack) ->
  {value, Around} = send({lookup, I}),
  Primes = 
  length(lists:filter(fun is_prime/1, [abs(I-A) || A <- Around])),
  pd(I+1, N, [{I, Primes} | Ack]).

ring(1) ->
  [1];
ring(N) ->
  Initial = ring_initial(N),
  Length = ring_length(N),
  lists:seq(Initial, Initial + Length - 1).

ring_around(Upto) ->
  ring_around(1, Upto, []).

ring_around(I, Upto, _Prev) when I > Upto ->
  ok;
ring_around(I, Upto, Prev) ->
  Next = ring(I),
  adjacent(I, Prev, Next),
  ring_around(I+1, Upto, Next).

ring_initial(1) ->
  1;
ring_initial(Ring) ->
  3 * (Ring-1) * (Ring-2) + 2.

ring_length(1) ->
  1;
ring_length(Ring) ->
  6 * (Ring-1).

ring_max(Ring) ->
  ring_initial(Ring) + ring_length(Ring) - 1.

ringof(1) ->
  1;
ringof(X) ->
  Sqrt = math:sqrt(81 - 12*(8-X)),
  trunc((9+Sqrt)/6).

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive 
    Result ->
      Result
  end.

sieve(N) ->
  Initial =
  array:set(
    0,
    false,
    array:set(
      1, 
      false, 
      array:new(N+1, [{fixed, true}, {default, true}]))),
  Sieve = sieve(2, trunc(math:sqrt(N)), N, Initial),
  lists:reverse(
    array:foldl(
      fun
        (_I, false, Primes) ->
          Primes;
        (I, true, Primes) ->
          [I|Primes]
      end,
      [],
      Sieve)).

sieve(I, SqrtN, N, Sieve) when I > SqrtN; I > N ->
  Sieve;
sieve(I, SqrtN, N, Sieve) ->
  Next =
  case array:get(I, Sieve) of
    false ->
      Sieve;
    true ->
      lists:foldl(
        fun(J, S) ->
            array:set(J, false, S)
        end,
        Sieve,
        lists:seq(I*I, N, I))
  end,
  sieve(I+1, SqrtN, N, Next).

start(NPrime) ->
  register(?MODULE, spawn(?MODULE, init, [NPrime])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ring_test_() ->
  [
   ?_assertEqual(lists:seq(1,1),   ring(1)),
   ?_assertEqual(lists:seq(2,7),   ring(2)),
   ?_assertEqual(lists:seq(8,19),  ring(3)),
   ?_assertEqual(lists:seq(20,37), ring(4))
  ].

ringof_test_() ->
  [
   ?_assertEqual(1, ringof(1)),
   ?_assert(lists:all(fun(X) -> X =:= 2 end, [ringof(X) || X <- ring(2)])),
   ?_assert(lists:all(fun(X) -> X =:= 3 end, [ringof(X) || X <- ring(3)]))
  ].

ring_around_test_() ->
  {
   setup,
   fun() ->
       start(10)
   end,
   fun(_) ->
       stop()
   end,
   fun(_) ->
       ring_around(5),
       send(sort),
       [
        ?_assertEqual({value, [2,3,4,5,6,7]}, send({lookup, 1})),
        ?_assertEqual({value, [1,3,7,8,9,19]}, send({lookup, 2})),
        ?_assertEqual({value, [3,4,10,12,24,25]}, send({lookup, 11})),
        ?_assertEqual({value, [8,21,37,38,39,61]}, send({lookup, 20})),
        ?_assertEqual({value, [8,9,20,22,39,40]}, send({lookup, 21}))
       ]
   end
  }.

ring_initial_test_() ->
  [
   ?_assertEqual([1,2,8,20,38,62,92], [ring_initial(I) || I <- lists:seq(1,7)])
  ].

ring_length_test_() ->
  [
   ?_assertEqual([1,6,12,18,24,30,36], [ring_length(I) || I <- lists:seq(1,7)])
  ].

ring_max_test_() ->
  [
   ?_assertEqual([1,7,19,37,61,91,127], [ring_max(I) || I <- lists:seq(1,7)])
  ].

sieve_test_() ->
  [
   ?_assertEqual([], sieve(1)),
   ?_assertEqual([2], sieve(2)),
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
