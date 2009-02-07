-module(erlhubd).
-export([start/0, upgrade/0, loop/1]).

start() ->
    pool:start(),
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

upgrade() ->
    code:purge(client),
    code:load_file(client),
    code:purge(client_adc),
    code:load_file(client_adc),
    code:purge(client_nmdc),
    code:load_file(client_nmdc),
    code:purge(erlhubd),
    code:load_file(erlhubd),
    code:purge(pool),
    code:load_file(pool),
    code:purge(tcp_server),
    code:load_file(tcp_server),
    ok.

loop(Socket) ->
    client:start(Socket).
