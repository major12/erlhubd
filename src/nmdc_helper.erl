-module(nmdc_helper).
-compile(export_all).

-import(lists, [reverse/1]).

read_nick(Data) ->
    read_nick(Data, "").

read_nick(<<" ", Data/binary>>, To) ->
    {ok, To, Data};
read_nick(<<B:8, Data/binary>>, To) ->
    read_nick(Data, To ++ [B]).

read_ip(Data) ->
    read_ip(Data, "").

read_ip(<<B:8, Data/binary>>, Ip) when (B >= $0 andalso B =< $9) orelse
                                       B =:= $. orelse B =:= $: ->
    read_ip(Data, Ip ++ [B]);
read_ip(Data, Ip) ->
    {ok, Ip, Data}.

split(Binary) ->
    split(Binary, [], "").

split(<<>>, List, "") ->
    reverse(List);
split(<<>>, List, Option) ->
    reverse([reverse(Option)|List]);
split(<<" ", Bin/binary>>, List, "") ->
    split(Bin, List, "");
split(<<" ", Bin/binary>>, List, Option) ->
    split(Bin, [reverse(Option)|List], "");
split(<<B:8, Bin/binary>>, List, Option) ->
    split(Bin, List, [B|Option]).

join([]) ->
    [];
join([Head|Tail]) ->
    join(Tail, Head).

join([], Result) ->
    Result;
join([Head|Tail], Result) ->
    join(Tail, Result ++ Head).

ctm_extract(Data) ->
    {ok, Nick, Rest1} = read_nick(Data),
    case (catch read_ip(Rest1)) of
        {ok, Ip, <<>>} ->
            {ok, [Nick, Ip]};
        _ ->
            {ok, Nick2, Rest2} = read_nick(Rest1),
            {ok, Ip, <<>>} = read_ip(Rest2),
            {ok, [Nick2, Ip]}
    end.

to_extract(Data) ->
    {ok, ReceiverNick1, Rest1} = read_nick(Data),
    {ok, "From:", Rest2} = read_nick(Rest1),
    {ok, SenderNick1, Rest3} = read_nick(Rest2),
    {ok, SenderNick2, Message} = read_nick(Rest3),
    SenderNick2 = "$<" ++ SenderNick1 ++ ">",
    {ok, [SenderNick1, ReceiverNick1], Message}.

create_lock() ->
    create_bin(80 + random:uniform(54), <<>>).

create_key() ->
    create_bin(16, <<>>).

create_bin(0, Binary) ->
    Binary;
create_bin(Size, Binary) ->
    create_bin(Size - 1, <<Binary/binary, (97 + random:uniform(25)):8>>).
