-module(erlhubd).
-export([start/0, loop/1, sender/1]).

start() ->
	tcp_server:start(?MODULE, 4111, {?MODULE, loop}).

loop(Socket) ->
	Sender = spawn_link(?MODULE, sender, [self()]),
	Client = spawn_link(client, start, [self(), Sender]),
	Sender ! {self(), Socket, Client},
	receiver(Socket, Client).

receiver(Socket, Client) ->
	receiver(Socket, Client, <<>>).

receiver(Socket, Client, Buffer) ->
	{ok, Data}  = gen_tcp:recv(Socket, 0),
	{Msg, Next} = split_message(Data),
	Client ! {self(), <<Buffer/binary, Msg/binary>>},
	receiver(Socket, Client, Next).

sender(Receiver) ->
	receive
		{Receiver, Socket, Client} ->
			sender(Socket, Client);
		Any -> 
			io:format("[S] Not initialized yet: ~p~n", [Any]),
			sender(Receiver)
	end.

sender(Socket, Client) ->
	receive
		{Client, Message} ->
			io:format("[S] Sending message: ~p~n", [Message]),
			gen_tcp:send(Socket, Message),
			sender(Socket, Client);
		Any ->
			io:format("[S] Unknown message: ~p~n", [Any]),
			sender(Socket, Client)
	end.

split_message(Binary) ->
	split_message(<<>>, Binary).

split_message(First, <<$\n, Second/binary>>) ->
	{First, Second};
split_message(First, <<>>) ->
	{First, <<>>};
split_message(First, <<B:8, Second/binary>>) ->
	split_message(<<First/binary, B:8>>, Second).
