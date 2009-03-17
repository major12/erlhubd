-module(helper).
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
