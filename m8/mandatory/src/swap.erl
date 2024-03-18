%% @doc Hot code swapping example. Erlang supports change of code in a running
%% system. Code replacement is done on the module level. See <a
%% href="http://erlang.org/doc/reference_manual/code_loading.html" target="_blank">Compilation
%% and Code Loading</a> in the Erlang user guide for more information.

-module(swap).

-export([start/0]).

%% To change from old version to new version, a process must make a
%% fully qualified function call a function in the module. This function must be
%% exported.
-export([loop/0]).

%% @doc Starts a new process executing the loop/0 function. In Erlang systems it
%% is common for a module to provide a start function that initialises the
%% system, for example by spawning a new process.
start() ->
    spawn(fun() -> loop() end).

%% @doc Calculates a result. 

result(X) ->
    2*X.

%% @doc The recursive process receive loop function.
loop() ->
    receive
        swap ->
            io:format("Hot code swapping!~n"),
            %% To change from old module version to new module version, a process must make a
            %% fully qualified function call.
            ?MODULE:loop();
        X when is_integer(X) ->
            io:format("Result = ~w~n", [result(X)]),
            loop();
        X  ->
            io:format("Unhandled message: ~p~n", [X]),
            loop()
    end.
