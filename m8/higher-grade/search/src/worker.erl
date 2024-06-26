-module(worker).

-export([start/4]).

%% @doc Starts a worker process. The worker will make random guesses between
%% `Min' and `Max'.

-spec start(Server, Master, Min, Max) -> Worker
  when Server :: pid(),
       Master :: pid(),
       Min :: number(),
       Max :: number(),
       Worker :: pid().
start(Server, Master, Min, Max) ->
  spawn(fun() -> loop(Server, Master, Min, Max, 1) end).

loop(Server, Master, Min, Max, Guesses) ->
  process_flag(trap_exit, true),
  Guess = utils:random(Min, Max),
  Server ! {guess, Guess, self()},
  master:log_guess(Master, Guess, Guesses, self()),

  receive
    {right, Guess} ->
      io:format("~p ~*.. B <== Found the winner ~n", [self(), utils:width(Max), Guess]),
      master:winner(Master, self()),
      exit(winner);
    {wrong, Guess} ->
      io:format("~p ~*.. B~n", [self(), utils:width(Max), Guess]),
      loop(Server, Master, Min, Max, Guesses + 1);
    {'EXIT', _From, loser} ->
      io:format("~p I lose :(~n", [self()]),
      master:loser(Master, self())
  end.
