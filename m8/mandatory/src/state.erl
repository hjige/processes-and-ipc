%% @doc An example of a stateful counter process that keeps track of an integer
%% counter that can be incremented and decremented.
%%
%% A process uses a recursive function to receive messages. A common practice is
%% to name this function loop.
%%
%% In Erlang systems it is common for a module to provide one or more start
%% functions that initializes the system, for example by spawning a new process.
%% In this example start/0 and start/1 can be used to create a new counter.
%%
%% The state of the counter process is defined by the values of the arguments to
%% the counter process receive loop/1 function. When receiving a message, the
%% process can change state by using a different value for the argument to the
%% recursive call to the process receive loop/1 function.
%%
%% It is common to provide a set of functions that hide the underlying message
%% passing. In addition to start/0 and start/, the functions inc/1, dec/1,
%% show/1 and get/1 are provided as an interface to the stateful counter
%% process.

-module(state).
-export([start/0, start/1, inc/1, dec/1, show/1, get/1]).

 -type counter()::pid().
%% Type alias for Pid of a counter process.

-export_type([counter/0]).

%% @doc Creates a new counter with initial counter value 0.
-spec start() -> counter().

start() ->
    spawn(fun() -> loop(0) end).

%% @doc Spawns a new counter process with initial counter value N.
-spec start(N) -> counter() when N::integer().

start(N) ->
    spawn(fun() -> loop(N) end).

%% @doc Increases the value of counter C by one.
-spec inc(C) -> ok when C::counter().

inc(C) ->
    C ! inc,
    ok.

%% @doc Decreases the value of counter C by one.
-spec dec(C) -> ok when C::counter().

dec(C) ->
    C ! dec,
    ok.

%% @doc Prints the value of counter C.
-spec show(C) -> ok when C::counter().

show(C) ->
    C ! show,
    ok.

%% @doc Returns the current value of counter C.
-spec get(C) -> integer() when C::counter().

get(C) ->
    C ! {get, self()},
    receive
        {value, Value} ->
            Value
    end.

%% The recursive receive function for the  main process is often named loop.
%%
%% The state of a process is defined by the values of the arguments to the
%% process receive loop.
%%
%% The state of the process executing the loop/1 function is defined by the
%% current value of the argument N.
%%
%% When receiving a message, the process can change state by using a different
%% value for the argument N in the recursive call.
loop(N) when is_integer(N) ->
    receive
        inc ->
            %% Update the process state by using a new value for the argument
            %% to the process loop.
            loop(N+1);
        dec ->
            %% Update the process state by using a new value for the argument
            %% to the process loop.
            loop(N-1);
        show ->
            io:format("N = ~w~n", [N]),
            %% Don't update the process state.
            loop(N);
        {get, Pid}  ->
            Pid ! {value, N},
            loop(N)
    end.
