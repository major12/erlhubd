-module(nmdc_client).
-export([init/2]).
-import(nmdc_helper, [create_lock/0, create_key/0]).
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
            NewState = process(State, Message),
            loop(NewState);
        Any ->
            io:format("[NC] Unknown message: ~p~n", [Any]),
            loop(State)
    end.

process(State, <<"$", Data/binary>>) ->
    parse(State, [], Data);
process(State, <<"<", Data/binary>>) ->
    parse_chat(State, [], Data);
process(State, Data) ->
    io:format("[NC] Bad message: ~n  '~s'~nDisconnect? ~n", [Data]),
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

handle(State, Command, Data) ->
    {M, F} = nmdc_commands:handler(Command),
    io:format("[NC] Handling ~p with ~p:~p~n", [Command, M, F]),
    M:F(State, Data).

handle_chat(#nmdc{nick = Nick} = State, Nick, Message) ->
    NickBin = list_to_binary(Nick),
    ok = clients_pool:broadcast({packet, <<"<", NickBin/binary, "> ", Message/binary, "|">>}),
    io:format("[NC] Chat ~s: ~s~n",[Nick, Message]),
    loop(State).
