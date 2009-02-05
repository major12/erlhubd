-module(erlhubd).
-export([start/0, loop/1, sender/1]).

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
	Sender = spawn_link(?MODULE, sender, [self()]),
	Client = spawn_link(client, start, [self(), Sender]),
	Sender ! {self(), Socket, Client},
	receiver(Socket, Client).

receiver(Socket, Client) ->
	receiver(Socket, Client, <<>>).

receiver(Socket, Client, Buffer) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			{Msg, Next} = split_message(Data),
			DataSize    = size(Data),
			SplitSize   = size(Msg) + size(Next),
			if
				DataSize =:= SplitSize + 1 ->
					Client ! {self(), <<Buffer/binary, Msg/binary>>},
					receiver(Socket, Client, Next);
				true ->
					receiver(Socket, Client, <<Buffer/binary, Msg/binary>>)
			end;
		Error ->
			io:format("[M] Error: ~p~n", [Error])
	end.

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
			gen_tcp:send(Socket, <<Message/binary, $\n>>),
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
