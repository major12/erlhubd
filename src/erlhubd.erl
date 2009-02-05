-module(erlhubd).
-export([start/0, loop/1]).

start() ->
    case tcp_server:start(?MODULE, 4111, {?MODULE, loop}) of
      {ok, _Pid} ->
            io:format("[M] Server started~n");
        {error, Reason} ->
            io:format("[M] Error starting server: ~p~n", [Reason]),
            Wait = 10000 + random:uniform(10000),
            io:format("[M] Retry in ~.2f seconds~n", [Wait / 1000]),
            receive after Wait ->
                start()
            end
    end.

loop(Socket) ->
    client:start(Socket).
