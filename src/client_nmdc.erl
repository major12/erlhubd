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

process(Receiver, Sender, <<"$", Data/binary>>) ->
    parse(Receiver, Sender, [], Data);
process(Receiver, Sender, <<"<", Data/binary>>) ->
    parse_chat(Receiver, Sender, [], Data);
process(Receiver, Sender, Data) ->
    io:format("[NC] Bad message: ~n  ~s~nDisconnect? ~n", [Data]),
    loop(Receiver, Sender).

parse(Receiver, Sender, Opcode, <<>>) ->
    handle(Receiver, Sender, list_to_atom(lists:reverse(Opcode)), <<>>);
parse(Receiver, Sender, Opcode, <<" ", Data/binary>>) ->
    handle(Receiver, Sender, list_to_atom(lists:reverse(Opcode)), Data);
parse(Receiver, Sender, Opcode, <<B:8, Data/binary>>) ->
    parse(Receiver, Sender, [B|Opcode], Data).

parse_chat(R, S, _, <<>>) ->
    io:format("[NC] Bad or empty chat message. Disconnect?~n"),
    loop(R, S);
parse_chat(R, S, Nick, <<"> ", MessageData/binary>>) ->
    handle_chat(R, S, lists:reverse(Nick), MessageData);
parse_chat(R, S, Nick, <<B:8, MessageData/binary>>) ->
    parse_chat(R, S, [B|Nick], MessageData).

handle(R, S, 'Key', Rest) ->
    put(ckey, Rest),
    loop(R, S);
handle(R, S, 'ValidateNick', Rest) ->
    handle_nick(R, S, Rest);
handle(R, S, 'Version', Rest) ->
    put(version, Rest),
    loop(R, S);
handle(R, S, 'GetNickList', _) ->
    io:format("[NC] Nick list requested~n"),
    Self = self(),
    S ! {self(), packets:op_list(clients_pool:clients([op, chief, admin, master]))},
    S ! {self(), packets:nick_list(clients_pool:clients(all))},
    ok = clients_pool:foreach(fun(E) -> S ! {Self, packets:my_info(E)}, ok end),
    loop(R, S);
handle(R, S, 'MyINFO', Data) ->
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
    io:format("[NC] Unhandled control message $~s ~s~n", [O, binary_to_list(D)]),
    loop(R, S).

handle_chat(R, S, SenderNick, MessageData) ->
    io:format("[NC] Chat message sender=\"~s\" message=\"~s\"~n",[SenderNick, MessageData]),
    loop(R,S).


create_lock() ->
    create_bin(80 + random:uniform(54), <<>>).

create_key() ->
    create_bin(16, <<>>).

create_bin(0, Binary) ->
    Binary;
create_bin(Size, Binary) ->
    create_bin(Size - 1, <<Binary/binary, (97 + random:uniform(25)):8>>).

handle_nick(_, _, <<>>) ->
    exit(empty_nick);
handle_nick(R, S, NickBin) ->
    Nick = binary_to_list(NickBin),
    true = clients_pool:add(self(), Nick),
    put(nick, Nick),
    S ! {self(), packets:hello(Nick)},
    loop(R, S).

handle_my_info(R, S, initialized) ->
    S ! {self(), packets:hub_name()},
    S ! {self(), packets:message(bot:client(), bot:greeting())},
    loop(R, S);
handle_my_info(R, S, _) ->
    loop(R, S).
