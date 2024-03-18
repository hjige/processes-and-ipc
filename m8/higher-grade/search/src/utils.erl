-module(utils).
-export([width/1, random/2]).

%% @doc Returns the number of digits in `N' where `N' has no leading zeroes.

-spec width(N) -> Width when
      N :: integer(),
      Width :: integer().

width(N) ->
    length(integer_to_list(N)).

%% @doc Returns a random number between `Min' and `Max'.

-spec random(Min, Max) -> R when
      Min :: integer(),
      Max :: integer(),
      R :: integer().

random(Min, Max) ->
    case Delta = Max - Min of
        0 ->
            Min;
        Delta ->
            Min - 1 + rand:uniform(Delta + 1)
    end.

