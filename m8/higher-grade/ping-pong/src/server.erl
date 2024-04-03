%% @doc A server that keeps track of  <a target="_blank"
%% href="https://www.rd.com/culture/ablaut-reduplication/">ablaut
%% reduplication</a> pairs. You should implement two versions of the server. One
%% stateless server and one stateful server.
%%
%% <ul>
%% <li>
%% The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.
%% </li>
%% <li>
%% The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/maps.html">Map</a>.
%% </li>
%% </ul>
%% <p>
%% You should also implement process supervision of the server.
%% <ul>
%% <li>
%% The supervisor should <a target="_blank"
%% href="https://erlang.org/doc/reference_manual/processes.html#registered-processes">register</a>
%% the server process under the name `server'.
%% </li>
%% <li>
%% The name of a registered process can be used instead of the Pid when sending
%% messages to the process.
%% </li>
%% <li>
%% The supervisor should restart the server if the server terminates due to an
%% error.
%% </li>
%% </ul>
%% </p>

-module(server).
-export([start/2, update/0, update/1, stop/0, stop/1, loop/0, loop/1]).

%% @doc The initial state of the stateful server.

-spec pairs() -> map().

pairs() ->
    #{ping => pong,
      tick => tock,
      hipp => hopp,
      ding => dong}.

%% @doc Starts the server.

-spec start(Stateful, Supervised) -> Server when
      Stateful :: boolean(),
      Supervised :: boolean(),
      Server :: pid().

start(false, false) ->
    spawn(fun() -> loop() end);
start(false, true) ->
    spawn(fun() -> supervisor(false) end);
start(true, false) ->
    spawn(fun() -> loop(pairs()) end);
start(true, true) ->
    spawn(fun() -> supervisor(true) end).

%% @doc The server supervisor. The supervisor must trap exit, spawn the server
%% process, link to the server process and wait the server to terminate. If the
%% server terminates due to an error, the supervisor should make a recursive
%% call to it self to restart the server.
-spec supervisor(Stateful) -> ok when
      Stateful :: boolean().

supervisor(Stateful) ->
    io:format("~nSupervisor with PID ~p started~n", [self()]),
    
    process_flag(trap_exit, true),
    
    start_server(Stateful),
    
    supervisor_loop(Stateful),
    ok.

%% @doc Starts a server process and registers it to 
-spec start_server(Stateful) -> ok when
    Stateful :: boolean().

start_server(Stateful) ->
  case Stateful of
      true ->
          PID = spawn_link(fun() -> loop(pairs()) end);
      false ->
          PID = spawn_link(fun() -> loop() end)
  end,

  io:format("Process with PID ~p started~n", [PID]),
  register(server, PID),
  io:format("PID ~p registered to atom 'server'~n", [PID]),
  ok.

    
%% @doc The server supervisor recieve message loop.
supervisor_loop(Stateful) ->
    receive
        {'EXIT', PID, Reason} ->
            io:format("Server ~w terminated with reason ~w!~n", [PID, Reason]),
            case Reason of
                simulated_bug -> 
                    %% Restart server
                    start_server(Stateful),
                    supervisor_loop(Stateful);
                _unchecked_reason ->
                    ok
            end;
        _ ->
            supervisor_loop(Stateful)
    end.

%% @doc Terminates the supervised server.

-spec stop() -> ok | error.

stop() ->
    stop(server).

-spec stop(Server) -> ok | error when
      Server :: pid().

%% @doc Terminates the unsupervised server.

stop(Server) ->
    Server ! {stop, self()},
    receive
        {stop, ok} ->
            ok;
        Msg ->
            io:format("stop/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc Makes the supervised server perform a hot code swap.

-spec update() -> ok | error.

update() ->
    update(server).

%% @doc Makes the unsupervised server perform a hot code swap.

-spec update(Server) -> ok | error when
      Server :: pid().

update(Server) ->
    Server ! {update, self()},
    receive
        {update, ok} ->
            ok;
        Msg ->
            io:format("update/1: Unknown message: ~p~n", [Msg]),
            error
    end.

%% @doc The process loop for the stateless server. The stateless server keeps
%% track of a static number of ablaut reduplication pairs. Each pair is handled
%% by a separate message receive pattern.

-spec loop() -> {stop, ok}.

loop() ->
    receive
        {ping, blipp, From} ->
            exit(simulated_bug),
            From ! {pong, blopp},
            loop();
        {ping, ding, From} ->
            From ! {pong, dong},
            loop();
        {ping, dddding, From} ->
            From ! {pong, ddddong},
            loop();
        {ping, ping, From} ->
            From ! {pong, pong},
            loop();
        {ping, tick, From} ->
            From ! {pong, tock},
            loop();
        {ping, king, From} ->
            From ! {pong, kong},
            loop();
        {stop, From} ->
            From ! {stop, ok};
        {update, From}  ->
            %% TODO: Trigger a hot code swap.
            From ! {update, ok},
            server:loop();
        Msg ->
            io:format("loop/0: Unknown message: ~p~n", [Msg]),
            loop()
    end.


%% @doc The process loop for the statefull server. The stateful server keeps
%% track of dynamic number of ablaut reduplication pairs using a <a
%% target="_blank" href="https://erlang.org/doc/man/Pairss.html">Map</a>.

-spec loop(Pairs) -> {stop, ok} when
      Pairs :: map().

loop(Pairs) ->
    receive
        {ping, flip, From} ->
            exit(simulated_bug);
        {ping, Ping, From} ->
            %% TODO: send correct reply.
            case maps:is_key(Ping, Pairs) of
                true ->
                    From ! {pong, maps:get(Ping, Pairs)},
                    loop(Pairs);
                false ->
                    io:format("loop2/0: Bad Key: ~p~n", [Ping]),
                    From ! "unknown key!",
                    loop(Pairs)
            end;
                        
            % Pong = (catch maps:get(Ping, Pairs)),
            % case Pong of 
            %     {'EXIT', {{badkey, _Key}, _Stacktrace}} ->
            %         io:format("loop2/0: Bad Key: ~p~n", [Ping]),
            %         From ! "unknown key!";
            %     {'EXIT', _} ->
            %         io:format("loop2/0: Bad Map: ~n");
            %     _Value ->
            %         From ! {pong, Pong}
            % end,
            % loop(Pairs);
        
        %% TODO: Handle the update, put and stop actions. 
        {put, Ping, Pong, From} ->
            From ! {put, Ping, Pong, ok},
            loop(maps:put(Ping, Pong, Pairs));
        {update, From} ->
            From ! {update, ok},
            server:loop(Pairs);
        {stop} ->
            tbi;
        Msg ->
            io:format("loop2/0: Unknown message: ~p~n", [Msg]),
            loop(Pairs)
    end.
