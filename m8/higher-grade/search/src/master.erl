-module(master).

-export([start/3, stop/1]).

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
    Server = server:start(Secret),
    Master = spawn(fun() -> loop(NumWorkers, init()) end),

    [worker:start(Server, Master, Min, Max) || _ <- lists:seq(1, NumWorkers)],

    Master ! foo,
    Master ! bar,

    Master.

%% @doc Stops the `Master'.

-spec stop(Master) -> stop when 
      Master :: pid().

stop(Master) ->
    Master ! stop.

loop(0, Map) ->
    io:format("DONE ~p~n", [Map]);

loop(CountDown, Map) ->
    receive
        {guess, _Master} ->
            loop(CountDown, Map);
        print ->
            io:format("~p~n", [Map]),
            loop(CountDown, Map);
        stop  ->
            ok;
        Msg ->
            io:format("master:loop/2 Unknown message ~p~n", [Msg]),
            loop(CountDown, Map)
    end.
