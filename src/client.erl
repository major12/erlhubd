-module(client).
-export([init/1, receiver/4, sender/2]).

init(Socket) ->
    io:format("[ C] Client connected~n"),
    guess_protocol(Socket).

receiver(Socket, Client, Splitter, Buffer) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Next = send_messages(Client, <<Buffer/binary, Data/binary>>, Splitter),
    receiver(Socket, Client, Splitter, Next).

sender(Socket, Client) ->
    receive
        {Client, die} ->
            gen_tcp:close(Socket),
            {ok, dead};
        {Client, skip} ->
            sender(Socket, Client);
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
            client_adc:init(Socket, Data);
        _ ->
            client_nmdc:init(Socket, <<>>)
    end.
