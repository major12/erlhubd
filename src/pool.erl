-module(pool).
-export([start/0]).

-record(client, {pid, nick}).

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

init() ->
    ets:new(pool, [named_table, {keypos, 3}, protected]),
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
