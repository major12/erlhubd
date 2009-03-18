-module(client_nmdc).
-export([init/2]).

-import(helper, [read_nick/1, read_ip/1,
                 split/1, join/1]).

-include("records.hrl").

init(Socket, Buffer) ->
    io:format("[NC] NMDC initializing~n"),
    R = spawn_link(client, receiver, [Socket, self(), $|, Buffer]),
    S = spawn_link(client, sender, [Socket, self()]),
    State = #nmdc{ lock     = create_lock(),
                   skey     = create_key(),
                   receiver = R,
                   sender   = S },
    State#nmdc.sender ! { self(),
                          packets:lock(State#nmdc.lock, State#nmdc.skey) },
    loop(State).

loop(#nmdc{receiver = R, sender = S} = State) ->
    receive
        {packet, Data} ->
            S ! {self(), Data},
            loop(State);
        {R, Message} ->
            process(State, Message);
        Any ->
            io:format("[NC] Unknown message: ~p~n", [Any]),
            loop(State)
    end.

process(State, <<"$", Data/binary>>) ->
    parse(State, [], Data);
process(State, <<"<", Data/binary>>) ->
    parse_chat(State, [], Data);
process(State, Data) ->
    io:format("[NC] Bad message: ~n  ~s~nDisconnect? ~n", [Data]),
    loop(State).

parse(State, Opcode, <<>>) ->
    handle(State, list_to_atom(lists:reverse(Opcode)), <<>>);
parse(State, Opcode, <<" ", Data/binary>>) ->
    handle(State, list_to_atom(lists:reverse(Opcode)), Data);
parse(State, Opcode, <<B:8, Data/binary>>) ->
    parse(State, [B|Opcode], Data).

parse_chat(State, _, <<>>) ->
    io:format("[NC] Bad or empty chat message. Disconnect?~n"),
    loop(State);
parse_chat(State, Nick, <<"> ", MessageData/binary>>) ->
    handle_chat(State, lists:reverse(Nick), MessageData);
parse_chat(State, Nick, <<B:8, MessageData/binary>>) ->
    parse_chat(State, [B|Nick], MessageData).

handle(State, 'Key', Rest) ->
    loop(State#nmdc{ckey = Rest});

handle(State, 'ValidateNick', Rest) ->
    handle_nick(State, Rest);

handle(State, 'Version', Rest) ->
    loop(State#nmdc{version = Rest});

handle(#nmdc{sender = S} = State, 'GetNickList', _) ->
    io:format("[NC] Nick list requested~n"),
    Self = self(),
    S ! {Self, packets:op_list(clients_pool:clients([op, chief, admin, master]))},
    S ! {Self, packets:nick_list(clients_pool:clients(all))},
    loop(State);

handle(#nmdc{nick = Nick} = State, 'MyINFO', Data) ->
    ["$ALL", Nick | MyInfoList] = split(Data),
    MyInfo = list_to_binary(join(MyInfoList)),
    ok = clients_pool:update(Nick, my_info, MyInfo),
    ok = clients_pool:broadcast({packet, packets:my_info(Nick, MyInfo)}),
    io:format("[NC] MyINFO: ~s~n", [MyInfo]),
    handle_my_info(State#nmdc{my_info = MyInfo});

handle(#nmdc{sender = S, nick = Nick} = State, 'GetINFO', Data) ->
    [SomeNick, Nick] = split(Data),
    [SomeClient] = clients_pool:get(SomeNick),
    S ! {self(), packets:my_info(SomeClient)},
    loop(State);

handle(State, 'Supports', Rest) ->
    Supports = lists:map(fun(E) -> list_to_atom(E) end, split(Rest)),
    io:format("[NC] Supports ~p~n", [Supports]),
    loop(State#nmdc{supports = Supports});

handle(#nmdc{nick = MyNick} = State, 'ConnectToMe', Data) ->
    {ok, [Nick, Ip]} = ctm_extract(Data),
    [C] = clients_pool:get(Nick),
    C#client.pid ! {packet, packets:ctm(list_to_binary(MyNick ++ " " ++ Ip))},
	% hm, it was dirty...
    io:format("[NC] Connect to me ~s ~s~n", [Nick, Ip]),
    loop(State);

handle(#nmdc{nick = MyNick} = State, 'To:', Data) ->
    {ok, [MyNick, Nick], Message} = to_extract(Data),
    [C] = clients_pool:get(Nick),
    C#client.pid ! {packet, packets:to(list_to_binary(MyNick),list_to_binary(Nick),Message)},
    io:format("[NC] To ~s ~s ~s~n", [MyNick, Nick, Message]),
    loop(State);

handle(State, O, D) ->
    io:format("[NC] Unhandled control message $~s ~s~n", [O, binary_to_list(D)]),
    loop(State).

handle_chat(#nmdc{nick = Nick} = State, Nick, Message) ->
    NickBin = list_to_binary(Nick),
    ok = clients_pool:broadcast({packet, <<"<", NickBin/binary, "> ", Message/binary, "|">>}),
    io:format("[NC] Chat ~s: ~s~n",[Nick, Message]),
    loop(State).

create_lock() ->
    create_bin(80 + random:uniform(54), <<>>).

create_key() ->
    create_bin(16, <<>>).

create_bin(0, Binary) ->
    Binary;
create_bin(Size, Binary) ->
    create_bin(Size - 1, <<Binary/binary, (97 + random:uniform(25)):8>>).

handle_nick(_, <<>>) ->
    exit(empty_nick);
handle_nick(#nmdc{sender = S} = State, NickBin) ->
    Nick = binary_to_list(NickBin),
    true = clients_pool:add(self(), Nick),
    S ! {self(), packets:hello(Nick)},
    loop(State#nmdc{nick = Nick}).

handle_my_info(#nmdc{sender = S, state = initialized} = State) ->
    S ! {self(), packets:hub_name()},
    S ! {self(), packets:message(bot:client(), bot:greeting())},
    loop(State#nmdc{state = logged_in});
handle_my_info(State) ->
    loop(State).

ctm_extract(Data) ->
    {ok, Nick, Rest1} = read_nick(Data),
    case (catch read_ip(Rest1)) of
        {ok, Ip, <<>>} ->
            {ok, [Nick, Ip]};
        _ ->
            {ok, Nick2, Rest2} = read_nick(Rest1),
            {ok, Ip, <<>>} = read_ip(Rest2),
            {ok, [Nick2, Ip]}
    end.

to_extract(Data) ->
	{ok, ReceiverNick1, Rest1} = read_nick(Data),
	{ok, "From:", Rest2} = read_nick(Rest1),
	{ok, SenderNick1, Rest3} = read_nick(Rest2),
	{ok, SenderNick2, Message} = read_nick(Rest3),
% check is '$<'.SenderNick1.'>'==SenderNick2 if connection is client-to-hub	
	{ok, [SenderNick1, ReceiverNick1], Message}.
	
