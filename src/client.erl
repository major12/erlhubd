-module(client).
-export([start/2]).

start(Receiver, Sender) ->
	io:format("[C] Client connected~n"),
	loop(Receiver, Sender).

loop(Receiver, Sender) ->
	receive
		{Receiver, Message} ->
			io:format("[C] Message from receiver: ~p~n", [Message]),
			Sender ! {self(), Message},
			loop(Receiver, Sender);
		Any ->
			io:format("[C] Unknown message: ~p~n", [Any]),
			loop(Receiver, Sender)
	end.
