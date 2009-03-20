-module(erlhubd).
-export([start/0, stop/0]).

-behaviour(application).
-export([start/2, stop/1]).

start() ->
    application:start(erlhubd).

stop() ->
    application:stop(erlhubd).

start(_Type, _StartArgs) ->
    erlhubd_sup:start_link().

stop(_State) ->
    ok.