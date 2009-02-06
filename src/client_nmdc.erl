-module(client_nmdc).
-export([start/2]).

start(Socket, Buffer) ->
    io:format("[NC] NMDC initializing~n"),
    Receiver = spawn_link(client, receiver, [Socket, self(), $|, Buffer]),
    Sender = spawn_link(client, sender, [Socket, self()]),
    Lock = create_lock(),
    put(lock, Lock),
    Key = create_key(),
    put(skey, Key),
    Sender ! {self(), packet('$Lock', {Lock, Key})},
    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    receive
        {Receiver, <<"$Key ", Rest/binary>>} ->
            io:format("[NC] Got key: "),
            ok = handle_key(Rest),
            loop(Receiver, Sender);
        {Receiver, <<"$ValidateNick ", Rest/binary>>} ->
            io:format("[NC] Got nick: "),
            handle_nick(Receiver, Sender, Rest);
        {Receiver, Message} ->
            io:format("[NC] Unhandled message: ~s~n", [binary_to_list(Message)]),
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
    <<"$Lock ", Lock/bytes, " Pk=", Key/bytes, "|">>;
packet('$ValidateDenide', Nick) ->
    <<"$ValidateDenide ", Nick/bytes, "|">>;
packet('$Hello', Nick) ->
    <<"$Hello ", Nick/bytes, "|">>.

handle_key(Data) ->
    put(ckey, Data),
    io:format("~p~n", [Data]),
    ok.

handle_nick(_, S, <<>>) ->
    S ! {self(), packet('$ValidateDenide', <<>>)},
    S ! {self(), die},
    {error, bad_nick};
handle_nick(R, S, D) ->
    put(nick, D),
    S ! {self(), packet('$Hello', D)},
    loop(R, S).