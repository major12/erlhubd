-module(erlhubd_app).
-author('keymone <keymone@gmail.com>').

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    io:format("Starting erlhubd application~n"),
    clients_pool:start(),
    erlhubd_sup:start_link().

stop(_State) ->
    ok.
