-module(clients_pool).
-export([start/0, init/0, add/2, update/3, foreach/1, delete/1]).

-include("records.hrl").

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

init() ->
    ets:new(?MODULE, [named_table, {keypos, 3}, public, set]),
    loop().

loop() ->
    receive
        die ->
            dead;
        {broadcast, Message} ->
            ets:foldl(fun(#client{pid=P}, ok) ->
                          P ! {Message},
                          ok
                      end, ok, pool)
    end.

add(Pid, Nick) ->
    ets:insert_new(?MODULE, #client{pid=Pid, nick=Nick}).

foreach(Fun) ->
    ets:foldl(fun(E, ok) -> Fun(E), ok end, ok, clients_pool),
    ok.

update(Nick, Field, Value) ->
    C = ets:lookup(clients_pool, Nick),
    if
        C#client.pid =:= self() ->
            Pos = index(Field, record_info(fields, client)),
            ets:update_element(clients_pool, {Pos, Value}),
            ok;
        true ->
            {error, no_access}
    end.

delete(Nick) ->
    ets:delete(clients_pool, Nick).

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    0;
index(E, [H|_], I) when E =:= H ->
    I;
index(E, [_|T], I) ->
    index(E, T, I+1).