-module(server).

-export([start/1, stop/1]).

%% @doc Starts a new `Server' with a `Secret' number.

-spec start(Secret) -> Server when
      Secret :: integer(),
      Server :: pid().

start(Secret) ->
    spawn(fun() -> loop(Secret) end).

%% @doc Stops the `Server'.

-spec stop(Server) -> stop when
      Server :: pid.

stop(Server) ->
    Server ! stop.

loop(Secret) when Secret >= 1->
    receive
        {guess, Secret, From} ->
            From ! {right, Secret};
        {guess, N, From} ->
            From ! {wrong, N};
        stop ->
            ok;
        Msg ->
            io:format("server:loop/1 Unhandled message: ~p~n", [Msg])
    end,
    loop(Secret).
