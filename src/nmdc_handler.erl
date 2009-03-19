-module(nmdc_handler).
-compile(export_all).
-import(nmdc_helper, [read_nick/1, read_ip/1, split/1, 
                      join/1, ctm_extract/1, to_extract/1]).
-include("records.hrl").

key(State, Rest) ->
    State#nmdc{ckey = Rest}.

validate_nick(State, Rest) ->
    handle_nick(State, Rest).

version(State, Rest) ->
    State#nmdc{version = Rest}.

get_nick_list(#nmdc{sender = S} = State, _) ->
    io:format("[NC] Nick list requested~n"),
    Self = self(),
    S ! {Self, packets:op_list(clients_pool:clients([op, chief, admin, master]))},
    S ! {Self, packets:nick_list(clients_pool:clients(all))},
    State.

my_info(#nmdc{nick = Nick} = State, Data) ->
    ["$ALL", Nick | MyInfoList] = split(Data),
    MyInfo = list_to_binary(join(MyInfoList)),
    ok = clients_pool:update(Nick, my_info, MyInfo),
    ok = clients_pool:broadcast({packet, packets:my_info(Nick, MyInfo)}),
    io:format("[NC] MyINFO: ~s~n", [MyInfo]),
    handle_my_info(State#nmdc{my_info = MyInfo}).

get_info(#nmdc{sender = S, nick = Nick} = State, Data) ->
    [SomeNick, Nick] = split(Data),
    [SomeClient] = clients_pool:get(SomeNick),
    S ! {self(), packets:my_info(SomeClient)},
    State.

supports(State, Rest) ->
    Supports = lists:map(fun(E) -> list_to_atom(E) end, split(Rest)),
    io:format("[NC] Supports ~p~n", [Supports]),
    State#nmdc{supports = Supports}.

connect_to_me(#nmdc{nick = MyNick} = State, Data) ->
    {ok, [Nick, Ip]} = ctm_extract(Data),
    [C] = clients_pool:get(Nick),
    C#client.pid ! {packet, packets:ctm(list_to_binary(MyNick ++ " " ++ Ip))},
	% hm, it was dirty...
    io:format("[NC] Connect to me ~s ~s~n", [Nick, Ip]),
    State.

to(#nmdc{nick = MyNick} = State, Data) ->
    {ok, [MyNick, Nick], Message} = to_extract(Data),
    [C] = clients_pool:get(Nick),
    C#client.pid ! {packet, packets:to(list_to_binary(MyNick),list_to_binary(Nick),Message)},
    io:format("[NC] To ~s ~s ~s~n", [MyNick, Nick, Message]),
    State.

unknown_command(State, _) ->
    io:format("[NC] Unknown command!~n"),
    State.

handle_nick(_, <<>>) ->
    exit(empty_nick);
handle_nick(#nmdc{sender = S} = State, NickBin) ->
    Nick = binary_to_list(NickBin),
    true = clients_pool:add(self(), Nick),
    S ! {self(), packets:hello(Nick)},
    State#nmdc{nick = Nick}.

handle_my_info(#nmdc{sender = S, state = initialized} = State) ->
    S ! {self(), packets:hub_name()},
    S ! {self(), packets:message(bot:client(), bot:greeting())},
    State#nmdc{state = logged_in};
handle_my_info(State) ->
    State.
