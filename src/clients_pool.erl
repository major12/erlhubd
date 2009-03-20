-module(clients_pool).
-export([start/0, init/0,
         add/2, update/3, delete/1, get/1,
         broadcast/1, foreach/1, clients/1]).

-include("records.hrl").

start() ->
    case whereis(?MODULE) of
        undefined ->
            Pid = spawn_link(?MODULE, init, []),
            register(?MODULE, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

init() ->
    process_flag(trap_exit, true),
    Storage = ets:new(storage, [{keypos, 3}, private, set]),
    loop(Storage).

loop(Storage) ->
    receive
        {broadcast, Message} ->
            ets:foldl(fun(#client{pid=P}, ok) ->
                          P ! Message,
                          ok
                      end, ok, Storage),
            loop(Storage);
        {From, add, {Pid, Nick}} ->
            Reply = ets:insert_new(Storage, #client{pid=Pid, nick=Nick}),
            if Reply -> link(Pid); true -> ok end,
            From ! {self(), add, Reply},
            loop(Storage);
        {From, foreach, Fun} ->
            Reply = ets:foldl(fun(E, ok) -> catch Fun(E) end, ok, Storage),
            From ! {self(), foreach, Reply},
            loop(Storage);
        {From, update, {Nick, Field, Value}} ->
            case ets:lookup(Storage, Nick) of
                [C] ->
                    Reply = if
                        C#client.pid =:= From ->
                            Pos = index(Field, record_info(fields, client)) + 1,
                            ets:update_element(Storage, Nick, {Pos, Value}),
                            ok;
                        true ->
                            {error, no_access}
                    end,
                    From ! {self(), update, Reply},
                    loop(Storage);
                _ ->
                    From ! {self(), update, {error, not_found}},
                    loop(Storage)
            end;
        {From, delete, Nick} ->
            Reply = ets:delete(Storage, Nick),
            From ! {self(), delete, Reply},
            loop(Storage);

        % TODO: use role weight to select clients
        {From, clients, all} ->
            Reply = ets:foldl(fun(E, L) -> [E|L] end, [], Storage),
            From ! {self(), clients, Reply},
            loop(Storage);
        {From, clients, Roles} ->
            Reply = ets:foldl(fun(E, L) ->
                                  #client{role = Role} = E,
                                  Member = lists:member(Role, Roles),
                                  if
                                      Member ->
                                          [E|L];
                                      true ->
                                          L
                                  end
                              end, [], Storage),
            From ! {self(), clients, Reply},
            loop(Storage);

        {From, get, Nick} ->
            Reply = ets:lookup(Storage, Nick),
            From ! {self(), get, Reply},
            loop(Storage);
        
        {'EXIT', Pid, Reason} ->
            case ets:match(Storage, {client, Pid, '$1', '_'}) of
                [[Nick]] ->
                    ets:delete(Storage, Nick),
                    io:format("[CP] ~s ~p died: ~p~n", [Nick, Pid, Reason]),
                    loop(Storage);
                _ ->
                    io:format("[CP] ~p died: ~p~n", [Pid, Reason]),
                    loop(Storage)
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

clients(Roles) ->
    rpc(clients, Roles).

get(Nick) ->
    rpc(get, Nick).

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

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    0;
index(E, [H|_], I) when E =:= H ->
    I;
index(E, [_|T], I) ->
    index(E, T, I+1).
