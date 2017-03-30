-module(cube1).

-export(
   [
    count_prime_cubes/1,
    find_prime_cubes/1,
    init/1,
    main/0,
    sieve/1,
    start/1,
    stop/0
   ]
  ).

-record(
   state,
   {
    cubes = gb_trees:empty(),
    primes
   }
  ).

%%
%% n^3 + n^2p = c^3
%% n^3(1 + p/n) = c^3
%% n^3((n + p)/n) = c^3
%% n(n+p)^1/3/n^1/3 = c
%%
%% n+p = x^3
%% n = y^3
%% p = x^3 - y^3
%%

count_prime_cubes(Upto) ->
  count_prime_cubes(1, Upto, gb_sets:from_list(sieve(Upto)), 0).

count_prime_cubes(I, Upto, Primes, Count) ->
  Diff = (I+1)*(I+1)*(I+1) - I*I*I,
  case Diff >= Upto of
    true ->
      Count;
    false ->
      CountNext =
      case gb_sets:is_member(Diff, Primes) of
        true ->
          Count+1;
        false ->
          Count
      end,
      count_prime_cubes(I+1, Upto, Primes, CountNext)
  end.

cube(CubeRoot, Of, Cubes) ->
  Cube = CubeRoot * CubeRoot * CubeRoot,
  NextCubes =
  gb_trees:enter(Cube, CubeRoot, Cubes),
  case Cube > Of of
    true ->
      NextCubes;
    false ->
      cube(CubeRoot+1, Of, NextCubes)
  end.

cuberoot(Of, Cubes) ->
  {MaxCube, MaxCubeRoot} = gb_trees:largest(Cubes),
  Next =
  case MaxCube < Of of
    true ->
      cube(MaxCubeRoot+1, Of, Cubes);
    false ->
      Cubes
  end,
  CubeRoot =
  case gb_trees:lookup(Of, Next) of
    none ->
      none;
    {value, Value} ->
      Value
  end,
  {CubeRoot, Next}.

find_prime_cubes(Upto) ->
  find_prime_cubes(sieve(Upto), []).

find_prime_cubes([], Ack) ->
  lists:reverse(Ack);
find_prime_cubes([P|T], Ack) ->
  AckNext =
  case is_prime_cube(1, P) of
    {I, true} ->
      [{I, P} | Ack];
    false ->
      Ack
  end,
  find_prime_cubes(T, AckNext).

init(Upto) ->
  io:format("init: ~p~n", [Upto]),
  loop(#state{cubes = gb_trees:enter(1, 1, gb_trees:empty())}).

is_prime_cube(I, P) when I > 3*P ->
  false;
is_prime_cube(I, P) ->
  case send({cube, I*I*I + I*I*P}) of
    none ->
      is_prime_cube(I+1, P);
    _Root ->
      {I, true}
  end.

loop(State) ->
  NextState =
  receive
    {From, {cube, Of}} ->
      {CubeRoot, Cubes} = cuberoot(Of, State#state.cubes),
      From ! CubeRoot,
      #state{cubes = Cubes};
    {From, stop} ->
      From ! ok,
      exit(normal),
      State
  end,
  loop(NextState).

main() ->
  io:format("cube...~n", []),
  start(100),
  io:format("~p~n", [count_prime_cubes(1000000)]),
  ok = stop(),
  ok.

send(Msg) ->
  ?MODULE ! {self(), Msg},
  receive
    Result ->
      Result
  end.

sieve(N) ->
  Sieve =
  array:set(0, false,
            array:set(1, false,
                      array:new(N+1, [{default, true}, {fixed, true}]))),
  sieve(2, N, Sieve).

sieve(I, N, Sieve) when I*I > N ->
  lists:reverse(
    array:foldl(
      fun
        (J, true, Acc) ->
          [J|Acc];
      (_J, false, Acc) ->
          Acc
      end,
      [],
      Sieve));
sieve(I, N, Sieve) ->
  NextSieve =
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
  sieve(I+1, N, NextSieve).

start(Upto) ->
  register(?MODULE, spawn(?MODULE, init, [Upto])).

stop() ->
  send(stop).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cube_test_() ->
  {
   setup,
   fun() -> start(10) end,
   fun(_) -> stop() end,
   fun(_) ->
       Roots = 
       [R || R <- [send({cube, I}) || I <- lists:seq(1,30)], R =/= none],
       [
        ?_assertEqual([1,2,3], Roots)
       ]
   end
  }.

sieve_test_() ->
  [
   ?_assertEqual([2,3,5,7,11,13,17,19,23,29], sieve(30))
  ].

-endif.
