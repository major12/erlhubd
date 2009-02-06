-module(client).
-export([start/1, receiver/4, sender/2]).

start(Socket) ->
    io:format("[ C] Client connected~n"),
    guess_protocol(Socket).

receiver(Socket, Client, Splitter, Buffer) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Next = send_messages(Client, <<Buffer/binary, Data/binary>>, Splitter),
            receiver(Socket, Client, Splitter, Next);
        Error ->
            io:format("[ C] Receiver error: ~p~n", [Error])
    end.

sender(Socket, Client) ->
    receive
        {Client, die} ->
            gen_tcp:close(Socket),
            {ok, dead};
        {Client, Message} ->
            io:format("[ C] Sending message: ~p~n", [Message]),
            gen_tcp:send(Socket, Message),
            sender(Socket, Client);
        Any ->
            io:format("[ C] Unknown message: ~p~n", [Any]),
            sender(Socket, Client)
    end.

send_messages(Client, Binary, Splitter) ->
    send_messages(Client, <<>>, Splitter, Binary).

send_messages(Client, First, Splitter, <<Splitter, Second/binary>>) ->
    Client ! {self(), First},
    send_messages(Client, <<>>, Splitter, Second);
send_messages(_, First, _, <<>>) ->
    First;
send_messages(Client, First, Splitter, <<B:8, Second/binary>>) ->
    send_messages(Client, <<First/binary, B:8>>, Splitter, Second).

guess_protocol(Socket) ->
    case gen_tcp:recv(Socket, 1, 200) of
        {ok, Data} ->
            client_adc:start(Socket, Data);
        _ ->
            client_nmdc:start(Socket, <<>>)
    end.
