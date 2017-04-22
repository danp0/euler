-module(counting).

-export(
   [
    choose/3,
    count_sundays/0,
    count_sundays_with_library/0,
    day_of_year/3,
    days_from_year/4,
    days_in_month/2,
    days_in_year/1,
    main/0,
    is_leap_year/1,
    weekday/3
   ]
  ).

-include_lib("eunit/include/eunit.hrl").

%
% Count the number of Sundays that fell on the first of the month
% from 1 Jan 1901 to 31 Dec 2000.
%

choose(Condition, True, False) ->
  case Condition of
    true -> True;
    false -> False
  end.

count_sundays() ->
  lists:foldl(
    fun({Year, Month}, Days) ->
        case weekday(Year, Month, 1) of
          sunday ->
            Days + 1;
          _ ->
            Days
        end
    end,
    0,
    [{Y, M} || Y <- lists:seq(1901, 2000), M <- lists:seq(1, 12)]).

count_sundays_with_library() ->
  lists:foldl(
    fun({Year, Month}, Days) ->
        case calendar:day_of_the_week(Year, Month, 1) of
          7 ->
            Days + 1;
          _ ->
            Days
        end
    end,
    0,
    [{Y, M} || Y <- lists:seq(1901, 2000), M <- lists:seq(1, 12)]).

day_of_year(Year, Month, Day) ->
  lists:sum([days_in_month(M, Year) || M <- lists:seq(1, Month - 1)]) + Day.

days_from_year(StartingYear, Year, Month, Day) when Year >= StartingYear ->
  lists:sum([days_in_year(Y) || Y <- lists:seq(StartingYear, Year - 1)]) + 
  day_of_year(Year, Month, Day).

days_in_month(Month, Year) ->
  case Month of
    1  -> 31;
    2  -> choose(is_leap_year(Year), 29, 28);
    3  -> 31;
    4  -> 30;
    5  -> 31;
    6  -> 30;
    7  -> 31;
    8  -> 31;
    9  -> 30;
    10 -> 31;
    11 -> 30;
    12 -> 31
  end.

days_in_year(Year) ->
  choose(is_leap_year(Year), 366, 365).

main() ->
  io:format("sundays: ~p~n", [count_sundays()]),
  io:format("         ~p~n", [count_sundays_with_library()]).

is_leap_year(Year) ->
  (Year rem 4 =:= 0 andalso Year rem 100 =/= 0) orelse (Year rem 400 =:= 0).

weekday(Year, Month, Day) when Year >= 1900 ->
  Days = days_from_year(1900, Year, Month, Day),
  lists:nth((Days rem 7) + 1, 
            [sunday, monday, tuesday, wednesday, thursday, friday, saturday]).

%%
%% unit tests
%%
choose_test_() ->
  [
   ?_assertEqual(1, choose(true, 1, 2)),
   ?_assertEqual(2, choose(false, 1, 2))
  ].

day_of_year_test_() ->
  [
   ?_assertEqual(1,   day_of_year(1900, 1, 1)),
   ?_assertEqual(31,  day_of_year(1900, 1, 31)),
   ?_assertEqual(365, day_of_year(1900, 12, 31)),
   ?_assertEqual(32,  day_of_year(1904, 2, 1))
  ].

days_from_year_test_() ->
  [
   ?_assertEqual(1,   days_from_year(1900, 1900, 1, 1)),
   ?_assertEqual(366, days_from_year(1900, 1901, 1, 1)),
   ?_assertEqual(2 * 365 + 2, days_from_year(1900, 1902, 1, 2))
  ].

days_in_month_test_() ->
  [
   ?_assertEqual([31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], [days_in_month(Days, 1900) || Days <- lists:seq(1, 12)]),
   ?_assertEqual([31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], [days_in_month(Days, 1904) || Days <- lists:seq(1, 12)])
  ].

days_in_year_test_() ->
  [
   ?_assertEqual(365, days_in_year(1900)),
   ?_assertEqual(366, days_in_year(1904))
  ].

is_leap_year_test_() ->
  [
   ?_assertNot(is_leap_year(1900)),
   ?_assertNot(is_leap_year(1901)),
   ?_assert(is_leap_year(1904)),
   ?_assert(is_leap_year(1908)),
   ?_assert(is_leap_year(2000))
  ].

weekday_test_() ->
  [
   ?_assertEqual([monday, tuesday, wednesday, thursday, friday, saturday, sunday],
                 [weekday(1900, 1, Day) || Day <- lists:seq(1, 7)]),
   ?_assertEqual(monday, weekday(1900, 1, 8)),
   ?_assertEqual(sunday, weekday(2000, 12, 31))
  ].

