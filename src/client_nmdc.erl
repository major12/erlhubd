-module(client_nmdc).
-export([start/2]).

start(Socket, Buffer) ->
    io:format("[NC] NMDC initializing~n"),
    Receiver = spawn_link(client, receiver, [Socket, self(), $|, Buffer]),
    Sender = spawn_link(client, sender, [Socket, self()]),
    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    receive
        {Receiver, Message} ->
        	io:format("[NC] Message from receiver: ~p~n", [Message]),
        	Sender ! {self(), Message},
        	loop(Receiver, Sender);
        Any ->
        	io:format("[NC] Unknown message: ~p~n", [Any]),
        	loop(Receiver, Sender)
    end.
