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
        {Receiver, Message} ->
            process(Receiver, Sender, Message);
        Any ->
            io:format("[NC] Unknown message: ~p~n", [Any]),
            loop(Receiver, Sender)
    end.

process(Receiver, Sender, Data) ->
    parse(Receiver, Sender, [], Data).

parse(Receiver, Sender, Opcode, <<>>) ->
    handle(Receiver, Sender, list_to_atom(lists:reverse(Opcode)), <<>>);
parse(Receiver, Sender, Opcode, <<" ", Data/binary>>) ->
    handle(Receiver, Sender, list_to_atom(lists:reverse(Opcode)), Data);
parse(Receiver, Sender, Opcode, <<B:8, Data/binary>>) ->
    parse(Receiver, Sender, [B|Opcode], Data).

handle(R, S, '$Key', Rest) ->
    handle_key(R, S, Rest);
handle(R, S, '$ValidateNick', Rest) ->
    handle_nick(R, S, Rest);
handle(R, S, O, D) ->
    io:format("[NC] Unhandled message ~s ~s~n", [O, binary_to_list(D)]),
    loop(R, S).

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

handle_key(R, S, Data) ->
    put(ckey, Data),
    io:format("[NC] Got key: ~p~n", [Data]),
    loop(R, S).

handle_nick(_, S, <<>>) ->
    dead_end(S, packet('$ValidateDenide', <<>>), "Empty nick");
handle_nick(R, S, D) ->
    case catch clients_pool:add(self(), D) of
        true ->
            put(nick, D),
            io:format("[NC] Nick accepted~n"),
            S ! {self(), packet('$Hello', D)},
            loop(R, S);
        false ->
            dead_end(S, packet('$ValidateDenide', D), "Duplicate nick");
        Error ->
            io:format("[NC] Error: ~p~n", [Error]),
            dead_end(S, packet('$ValidateDenide', D), "Unknown reason")
    end.

dead_end(Sender, Packet, Message) ->
    io:format("[NC] ~s~n", [Message]),
    Sender ! {self(), Packet},
    Sender ! {self(), die},
    dead.
