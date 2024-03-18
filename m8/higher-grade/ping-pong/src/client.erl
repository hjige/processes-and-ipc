%% @doc The client API. You don't need to edit or change anything in this
%% module. To be able to use the client API you must complete the implementation
%% of the server. You must understand the implementation of the client API in
%% order to understand the message protocol between the client and the server.

-module(client).
-export([ping/1, ping/2, put/2, put/3]).

%% @doc Check if the registered server has a matching `Pong' for the `Ping'.

-spec ping(Ping) -> {pong, Pong} | unknown | no_server when
      Ping :: term(),
      Pong :: term().

ping(Ping) ->
    case whereis(server) of
        undefined ->
            io:format("ping/1: No server registered.~n"),
            no_server;
        _Pid ->
            ping(server, Ping)
    end.

%% @doc Check if the server has a matching `Pong' for the `Ping'.

-spec ping(Server, Ping) -> {pong, Pong} | unknown when
      Server :: pid(),
      Ping :: term(),
      Pong :: term().

ping(Server, Ping) ->
    Server ! {ping, Ping, self()},
    receive
        {pong, Pong} ->
            io:format("~s - ~s~n", [Ping, Pong]);
        unknown ->
            io:format("~s - ????~n", [Ping]),
            unknown;
        Msg ->
            io:format("ping/2: Unknown message: ~p~n", [Msg])
    after 500  ->
            io:format("ping/2: Timeout~n")
        end.

%% @doc Adds the pair `Ping' and `Pong' to the registered stateful server. If `Ping'
%% already exist on the server, the related pong is updated to `Pong'.

-spec put(Ping, Pong) -> ok | no_server | error when
      Ping :: term(),
      Pong :: term().

put(Ping, Pong) ->
    case whereis(server) of
        undefined ->
            io:format("put/2: No server registered.~n"),
            no_server;
        Pid ->
            put(Pid, Ping, Pong)
    end.

%% @doc Adds the pair `Ping' and `Pong' to the stateful server. If `Ping'
%% already exist on the server, the related pong is updated to `Pong'.

-spec put(Server, Ping, Pong) -> ok | error when 
      Server :: pid(),
      Ping :: term(),
      Pong :: term().

put(Server, Ping, Pong) ->
    Server ! {put, Ping, Pong, self()},
    receive
        {put, Ping, Pong, ok} ->
            ok;
        Msg ->
            io:format("put/3: Unknown message: ~p~n", [Msg]),
            error
    end.
