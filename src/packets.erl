-module(packets).
-compile(export_all).

-include("records.hrl").

lock(Lock, Key) ->
    <<"$Lock EXTENDEDPROTOCOL", Lock/bytes, " Pk=", Key/bytes, "|">>.

validate_denide(Nick) ->
    NickBin = list_to_binary(Nick),
    <<"$ValidateDenide ", NickBin/binary, "|">>.

hello(Nick) ->
    NickBin = list_to_binary(Nick),
    <<"$Hello ", NickBin/binary, "|">>.

my_info(#client{my_info = undefined}) ->
    skip;
my_info(#client{nick = Nick, my_info = Info}) ->
    NickBin = list_to_binary(Nick),
    <<"$MyINFO $ALL ", NickBin/binary, " ", Info/bytes, "|">>.

my_info(Nick, Info) ->
    NickBin = list_to_binary(Nick),
    <<"$MyINFO $ALL ", NickBin/binary, " ", Info/bytes, "|">>.

hub_name() ->
    <<"$HubName Test - Topic|">>.

message(#client{nick = Nick}, Message) ->
    NickBin    = list_to_binary(Nick),
    MessageBin = list_to_binary(Message),
    <<"<", NickBin/binary, "> ", MessageBin/binary, "|">>.

op_list(List) ->
    list(List, <<"$OpList ">>).

nick_list(List) ->
    list(List, <<"$NickList ">>).

ctm(Data) ->
    <<"$ConnectToMe ", Data/binary, "|">>.

% $To: ReceiverNick From: SenderNick $<SenderNick> Message
to(NickFrom, NickTo, Message) ->
    <<"$To: ", NickTo/binary, " From: ", NickFrom/binary, " $<", NickFrom/binary, "> ", Message/binary, "|">>.

list([], Packet) ->
    <<Packet/binary, "$$|">>;
list([#client{nick = Nick}], Packet) ->
    NickBin = list_to_binary(Nick),
    list([], <<Packet/binary, NickBin/binary>>);
list([#client{nick = Nick}|Rest], Packet) ->
    NickBin = list_to_binary(Nick),
    list(Rest, <<Packet/binary, NickBin/binary, "$$">>).
