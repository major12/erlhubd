-module(clients_pool).
-export([start/0, init/0,
         add/2, update/3, foreach/1, delete/1, broadcast/1]).

-include("records.hrl").

start() ->
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn(?MODULE, init, []),
            register(?MODULE, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init() ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, {keypos, 3}, private, set]),
    loop().

loop() ->
    receive
        change_code ->
            ?MODULE:loop();
        die ->
            dead;
        {broadcast, Message} ->
            ets:foldl(fun(#client{pid=P}, ok) ->
                          P ! Message,
                          ok
                      end, ok, ?MODULE),
            loop();
        {From, add, {Pid, Nick}} ->
            Reply = ets:insert_new(?MODULE, #client{pid=Pid, nick=Nick}),
            if Reply -> link(Pid); true -> ok end,
            From ! {self(), add, Reply},
            loop();
        {From, foreach, Fun} ->
            Reply = ets:foldl(fun(E, ok) -> Fun(E), ok end, ok, clients_pool),
            From ! {self(), foreach, Reply},
            loop();
        {From, update, {Nick, Field, Value}} ->
            case ets:lookup(?MODULE, Nick) of
                [C] ->
                    Reply = if
                        C#client.pid =:= From ->
                            Pos = index(Field, record_info(fields, client)) + 1,
                            ets:update_element(clients_pool, Nick, {Pos, Value}),
                            ok;
                        true ->
                            {error, no_access}
                    end,
                    From ! {self(), update, Reply},
                    loop();
                _ ->
                    From ! {self(), update, {error, not_found}},
                    loop()
            end,
            loop();
        {From, delete, Nick} ->
            Reply = ets:delete(clients_pool, Nick),
            From ! {self(), delete, Reply},
            loop();
        {'EXIT', Pid, Reason} ->
            case ets:match(?MODULE, {client, Pid, '$1', '_'}) of
                [[Nick]] ->
                    ets:delete(?MODULE, Nick),
                    io:format("[CP] ~s ~p died: ~p~n", [Nick, Pid, Reason]),
                    loop();
                _ ->
                    io:format("[CP] ~p died: ~p~n", [Pid, Reason]),
                    loop()
            end
    end.

add(Pid, Nick) ->
    rpc(add, {Pid, Nick}).

foreach(Fun) ->
    rpc(foreach, Fun).

update(Nick, Field, Value) ->
    rpc(update, {Nick, Field, Value}).

delete(Nick) ->
    rpc(delete, Nick).

broadcast(Data) ->
    send(broadcast, Data).

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    0;
index(E, [H|_], I) when E =:= H ->
    I;
index(E, [_|T], I) ->
    index(E, T, I+1).

rpc(Action, Params) ->
    Pid = whereis(?MODULE),
    Pid ! {self(), Action, Params},
    receive
        {Pid, Action, Reply} -> Reply
    end.

send(Action, Params) ->
    Pid = whereis(?MODULE),
    Pid ! {Action, Params},
    ok.
