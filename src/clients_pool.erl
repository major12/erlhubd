-module(clients_pool).
-export([add/2, update/3, delete/1, get/1,
         broadcast/1, foreach/1, clients/1]).

-include("records.hrl").

-behavior(gen_server).
-export([start/0, init/1, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2]).

-record(server_state, {ets}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #server_state{}, []).

init(State) ->
    process_flag(trap_exit, true),
    Storage = ets:new(storage, [{keypos, 3}, private, set]),
    {ok, State#server_state{ets = Storage}}.


handle_cast({broadcast, Message}, #server_state{ets = Storage} = State) ->
    ets:foldl(fun(#client{pid=P}, ok) ->
                  P ! Message,
                  ok
              end, ok, Storage),
    {noreply, State}.

handle_call(stop, _Caller, #server_state{ets = Storage} = State) ->
    ets:delete(Storage),
    {stop, normal, stopped, State};


handle_call({add, {Pid, Nick}}, _Caller, #server_state{ets = Storage} = State) ->
    Reply = ets:insert_new(Storage, #client{pid=Pid, nick=Nick}),
    if Reply -> link(Pid); true -> ok end,
    {reply, Reply, State};


handle_call({foreach, Fun}, _Caller, #server_state{ets = Storage} = State) ->
    Reply = ets:foldl(fun(E, ok) -> catch Fun(E) end, ok, Storage),
    {reply, Reply, State};


handle_call({update, {Nick, Field, Value}}, {Pid, _}, #server_state{ets = Storage} = State) ->
    case ets:lookup(Storage, Nick) of
        [C] ->
            Reply = if
                C#client.pid =:= Pid ->
                    Pos = index(Field, record_info(fields, client)) + 1,
                    ets:update_element(Storage, Nick, {Pos, Value}),
                    ok;
                true ->
                    {error, no_access}
            end,
            {reply, Reply, State};
        _ ->
            {reply, {error, not_found}, State}
    end;


handle_call({delete, Nick}, _Caller, #server_state{ets = Storage} = State) ->
    Reply = ets:delete(Storage, Nick),
    {reply, Reply, State};


handle_call({clients, all}, _Caller, #server_state{ets = Storage} = State) ->
    Reply = ets:foldl(fun(E, L) -> [E|L] end, [], Storage),
    {reply, Reply, State};


handle_call({clients, Roles}, _Caller, #server_state{ets = Storage} = State) ->
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
    {reply, Reply, State};


handle_call({get, Nick}, _Caller, #server_state{ets = Storage} = State) ->
    Reply = ets:lookup(Storage, Nick),
    {reply, Reply, State};

handle_call(_Msg, _Caller, State) -> {noreply, State}.

handle_info({'EXIT', Pid, {Reason, [{client, receiver, 4}]}},
            #server_state{ets = Storage} = State) ->
    case ets:match(Storage, {client, Pid, '$1', '_'}) of
        [[Nick]] ->
            ets:delete(Storage, Nick),
            io:format("[NC] ~s ~p died: ~p~n", [Nick, Pid, Reason]);
        _ ->
            io:format("[NC] ~p died: ~p~n", [Pid, Reason])
    end,
    {noreply, State};

handle_info(_Msg, Library) ->
    {noreply, Library}.

terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

add(Pid, Nick) ->
    gen_server:call(?MODULE, {add, {Pid, Nick}}).

foreach(Fun) ->
    gen_server:call(?MODULE, {foreach, Fun}).

update(Nick, Field, Value) ->
    gen_server:call(?MODULE, {update, {Nick, Field, Value}}).

delete(Nick) ->
    gen_server:call(?MODULE, {delete, Nick}).

broadcast(Data) ->
    gen_server:cast(?MODULE, {broadcast, Data}).

clients(Roles) ->
    gen_server:call(?MODULE, {clients, Roles}).

get(Nick) ->
    gen_server:call(?MODULE, {get, Nick}).

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    0;
index(E, [H|_], I) when E =:= H ->
    I;
index(E, [_|T], I) ->
    index(E, T, I+1).
