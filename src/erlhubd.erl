-module(erlhubd).
-export([start/0, loop/1]).

start() ->
    io:format("starting tcp server.~n"),
	tcp_server:start(?MODULE, 4111, {?MODULE, loop}),
	io:format("tcp server started.~n").

loop(Socket) ->
    io:format("accepted connection. closed.~n"),
	gen_tcp:close(Socket).
