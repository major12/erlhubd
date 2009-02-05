-module(client_nmdc).
-export([start/2]).

start(Socket, Buffer) ->
    io:format("[NC] NMDC initializing~n"),
    Receiver = spawn_link(client, receiver, [Socket, self(), $|, Buffer]),
    Sender = spawn_link(client, sender, [Socket, self()]),
    Lock = create_lock(),
    put(lock, Lock),
    Key = create_key(),
    put(key, Key),
    Sender ! {self(), packet('$Lock', {Lock, Key})},
    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    receive
        {Receiver, Message} ->
            io:format("[NC] Message from receiver: ~s~n", [binary_to_list(Message)]),
            Sender ! {self(), Message},
            loop(Receiver, Sender);
        Any ->
            io:format("[NC] Unknown message: ~p~n", [Any]),
            loop(Receiver, Sender)
    end.

create_lock() ->
    create_bin(80 + random:uniform(54), <<>>).

create_key() ->
    create_bin(16, <<>>).

create_bin(0, Binary) ->
    Binary;
create_bin(Size, Binary) ->
    create_bin(Size - 1, <<Binary/binary, (97 + random:uniform(25)):8>>).

packet('$Lock', {Lock, Key}) ->
    <<"$Lock ", Lock/bytes, " Pk=", Key/bytes, "|">>.
