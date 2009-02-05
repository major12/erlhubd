-module(client).
-export([start/1, receiver/4, sender/2]).

start(Socket) ->
    io:format("[*C] Client connected~n"),
    guess_protocol(Socket).

receiver(Socket, Client, Splitter, Buffer) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {Msg, Next} = split_message(Data, Splitter),
            DataSize    = size(Data),
            SplitSize   = size(Msg) + size(Next),
            if
                DataSize =:= SplitSize + 1 ->
                	Client ! {self(), <<Buffer/binary, Msg/binary>>},
                	receiver(Socket, Client, Splitter, Next);
                true ->
                    receiver(Socket, Client, Splitter, <<Buffer/binary, Msg/binary>>)
            end;
        Error ->
            io:format("[AC] Receiver error: ~p~n", [Error])
    end.

sender(Socket, Client) ->
    receive
        {Client, Message} ->
            io:format("[*C] Sending message: ~p~n", [Message]),
            gen_tcp:send(Socket, Message),
            sender(Socket, Client);
        Any ->
            io:format("[*C] Unknown message: ~p~n", [Any]),
            sender(Socket, Client)
    end.

split_message(Binary, Splitter) ->
    split_message(<<>>, Splitter, Binary).

split_message(First, Splitter, <<Splitter, Second/binary>>) ->
    {First, Second};
split_message(First, _, <<>>) ->
    {First, <<>>};
split_message(First, Splitter, <<B:8, Second/binary>>) ->
    split_message(<<First/binary, B:8>>, Splitter, Second).

guess_protocol(Socket) ->
    case gen_tcp:recv(Socket, 1, 200) of
        {ok, Data} ->
            client_adc:start(Socket, Data);
        _ ->
            client_nmdc:start(Socket, <<>>)
    end.
