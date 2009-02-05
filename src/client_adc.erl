-module(client_adc).
-export([start/2]).

start(Socket, Buffer) ->
    io:format("[AC] ADC initializing~n"),
    Receiver = spawn_link(client, receiver, [Socket, self(), $\n, Buffer]),
    Sender = spawn_link(client, sender, [Socket, self()]),
    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    receive
        {Receiver, Message} ->
        	io:format("[AC] Message from receiver: ~p~n", [Message]),
        	Sender ! {self(), Message},
        	loop(Receiver, Sender);
        Any ->
        	io:format("[AC] Unknown message: ~p~n", [Any]),
        	loop(Receiver, Sender)
    end.
