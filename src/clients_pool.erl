-module(clients_pool).
-export([start/0, init/0, add/2, update/3, foreach/1, delete/1]).

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
                          P ! {Message},
                          ok
                      end, ok, pool),
            loop();
        {From, add, {Pid, Nick}} ->
            Reply = ets:insert_new(?MODULE, #client{pid=Pid, nick=Nick}),
            From ! {self(), add, Reply},
            loop();
        {From, foreach, Fun} ->
            Reply = ets:foldl(fun(E, ok) -> Fun(E), ok end, ok, clients_pool),
            From ! {self(), foreach, Reply},
            loop();
        {From, update, {Nick, Field, Value}} ->
            C = ets:lookup(clients_pool, Nick),
            Reply = if
                C#client.pid =:= self() ->
                    Pos = index(Field, record_info(fields, client)),
                    ets:update_element(clients_pool, {Pos, Value}),
                    ok;
                true ->
                    {error, no_access}
            end,
            From ! {self(), update, Reply},
            loop();
        {From, delete, Nick} ->
            Reply = ets:delete(clients_pool, Nick),
            From ! {self(), delete, Reply},
            loop()
    end.

add(Pid, Nick) ->
    rpc(add, {Pid, Nick}).

foreach(Fun) ->
    rpc(foreach, Fun).

update(Nick, Field, Value) ->
    rpc(update, {Nick, Field, Value}).

delete(Nick) ->
    rpc(delete, Nick).

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    0;
index(E, [H|_], I) when E =:= H ->
    I;
index(E, [_|T], I) ->
    index(E, T, I+1).

rpc(Action, Params) ->
    Pid = whereis(clients_pool),
    Pid ! {self(), Action, Params},
    receive
        {Pid, Action, Reply} -> Reply
    end.
