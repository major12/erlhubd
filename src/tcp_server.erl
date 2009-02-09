-module(tcp_server).
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start/3]).

-define(TCP_OPTIONS, [binary, inet, {packet, 0}, {active, false}]).

-record(server_state, {port, loop, ip=any, lsocket=null}).

start(Name, Port, Loop) ->
    State = #server_state{port = Port, loop = Loop},
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            M:F(Socket);
        {error, Reason} ->
            io:format("[M] Couldn't accept connection: ~p~n", [Reason])
    end.
   
accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

handle_call(stop, _Caller, State) -> 
    gen_tcp:close(State#server_state.lsocket), 
    {stop, normal, stopped, State};
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
