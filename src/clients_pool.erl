-module(clients_pool).
-export([start/0, init/0, add/2]).

-record(client, {pid, nick}).

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