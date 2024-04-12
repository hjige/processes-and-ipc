-module(master).

-export([start/3, stop/1, log_guess/4, winner/2, loser/2]).

init() ->
    maps:new().

%% @doc Starts the server and `NumWorkers' workers. The server is started with a
%% random secret number between `Min' and `Max'.

-spec start(NumWorkers, Min, Max) -> Master when
      NumWorkers :: integer(),
      Min :: integer(),
      Max :: integer(),
      Master :: pid().

start(NumWorkers, Min, Max) ->
    Secret = utils:random(Min, Max),
    io:format("The secret is ~p~n", [Secret]),
    Server = server:start(Secret),
    Master = spawn(fun() -> loop(NumWorkers, init()) end),
    
    WorkerPIDs = [worker:start(Server, Master, Min, Max) || _ <- lists:seq(1, NumWorkers)],
    
    % Initialize each workers stats on the Master
    lists:map(fun(PID) -> Master ! {add_worker, PID} end, WorkerPIDs),
        
    % Create a supervisor to terminate all workers once one has won.
    spawn(fun() -> supervise_workers(WorkerPIDs) end),

    Master.

%% @doc Stops the `Master'.

-spec stop(Master) -> stop when 
      Master :: pid().

stop(Master) ->
    Master ! stop.

loop(0, Map) ->
    io:format("DONE ~n Final statistics: ~n~n~p~n", [Map]);

loop(CountDown, Map) ->
    receive
        {guess, _Master} ->
            loop(CountDown, Map);
        {add_worker, Worker} ->
            loop(CountDown, maps:put(Worker, {0, 0, searching}, Map));
        {guess, Guess, Guesses, Worker} ->
            loop(CountDown, maps:put(Worker, {Guesses, Guess, searching}, Map));
        {winner, Worker, _Master} -> 
            % maps:map(fun(K, _V) -> K ! {'EXIT', Worker, loser} end, Map),
            {Count, Guess, _Status} = maps:get(Worker, Map),
            % io:format("Master registered winner~n"),
            loop(CountDown - 1, maps:put(Worker, {Count, Guess, winner}, Map));
        {loser, Worker, _Master} ->
            {Count, Guess, _Status} = maps:get(Worker, Map),
            loop(CountDown - 1, maps:put(Worker, {Count, Guess, loser}, Map));
        print ->
            io:format("~p~n", [Map]),
            loop(CountDown, Map);
        stop  ->
            ok;
        Msg ->
            io:format("master:loop/2 Unknown message ~p~n", [Msg]),
            loop(CountDown, Map)
    end.

%% @doc Logs the worker's current number of guesses.
-spec log_guess(Master, Guess, Guesses, Worker) -> ok when
    Master :: pid(),
    Guess :: integer(),
    Guesses :: integer(),
    Worker :: pid().

log_guess(Master, Guess, Guesses, Worker) ->
    Master ! {guess, Guess, Guesses, Worker},
    ok.

%% @doc Notifies the Master that this Worker has guessed the correct number.

-spec winner(Master, Worker) -> ok when
    Master :: pid(),
    Worker :: pid().

winner(Master, Worker) ->
    Master ! {winner, Worker, Master},
    ok.

%% @doc Notifies the Master that this Worker has lost.

-spec loser(Master, Worker) -> ok when
  Master ::pid(),
  Worker :: pid().

loser(Master, Worker) ->
  Master ! {loser, Worker, Master},
  ok.

%% @doc Supervises workers, when one worker terminates, each other workers are terminated.

-spec supervise_workers(WorkerPIDs) -> ok when
    WorkerPIDs :: [pid()].

supervise_workers(WorkerPIDs) ->
    process_flag(trap_exit, true),
    lists:map(fun(PID) -> link(PID) end, WorkerPIDs),
    
    io:format("Supervisor created and linked to all workers!~n"),

    receive
        {'EXIT', _From, winner} ->
            exit(loser)
    end,

    ok.