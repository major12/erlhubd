-module(client_nmdc).
-export([init/2]).

init(Socket, Buffer) ->
    io:format("[NC] NMDC initializing~n"),
    Receiver = spawn_link(client, receiver, [Socket, self(), $|, Buffer]),
    Sender = spawn_link(client, sender, [Socket, self()]),
    put(state, initialized),
    Lock = create_lock(),
    put(lock, Lock),
    Key = create_key(),
    put(skey, Key),
    Sender ! {self(), packets:lock(Lock, Key)},
    loop(Receiver, Sender).

loop(Receiver, Sender) ->
    receive
        {packet, Data} ->
            Sender ! {self(), Data},
            loop(Receiver, Sender);
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
    put(ckey, Rest),
    loop(R, S);
handle(R, S, '$ValidateNick', Rest) ->
    handle_nick(R, S, Rest);
handle(R, S, '$Version', Rest) ->
    put(version, Rest),
    loop(R, S);
handle(R, S, '$GetNickList', _) ->
    io:format("[NC] Nick list requested~n"),
    Self = self(),
    ok = clients_pool:foreach(fun(E) -> S ! {Self, packets:my_info(E)}, ok end),
    loop(R, S);
handle(R, S, '$MyINFO', Data) ->
    Nick = get(nick),
    NickBin = list_to_binary(Nick),
    Size = size(NickBin),
    <<"$ALL ", NickBin:Size/bytes, " ", MyInfo/bytes>> = Data,
    put(my_info, MyInfo),
    ok = clients_pool:update(Nick, my_info, MyInfo),
    ok = clients_pool:broadcast({packet, packets:my_info(Nick, MyInfo)}),
    io:format("[NC] MyINFO: ~s~n", [MyInfo]),
    handle_my_info(R, S, get(state));
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

handle_nick(_, S, <<>>) ->
    dead_end(S, packets:validate_denide(<<>>), "Empty nick");
handle_nick(R, S, NickBin) ->
    Nick = binary_to_list(NickBin),
    case catch clients_pool:add(self(), Nick) of
        true ->
            put(nick, Nick),
            io:format("[NC] Nick accepted~n"),
            S ! {self(), packets:hello(Nick)},
            loop(R, S);
        false ->
            dead_end(S, packets:validate_denide(Nick), "Duplicate nick");
        Error ->
            io:format("[NC] Error: ~p~n", [Error]),
            dead_end(S, packets:validate_denide(Nick), "Clients pool failure")
    end.

handle_my_info(R, S, initialized) ->
    S ! {self(), packets:hub_name()},
    S ! {self(), packets:message(bot:client(), bot:greeting())},
    loop(R, S);
handle_my_info(R, S, _) ->
    loop(R, S).

dead_end(Sender, Packet, Message) ->
    clients_pool:delete(get(nick)),
    io:format("[NC] ~s~n", [Message]),
    Sender ! {self(), Packet},
    Sender ! {self(), die},
    dead.
