-module(packets).
-compile(export_all).

-include("records.hrl").

lock(Lock, Key) ->
    <<"$Lock ", Lock/bytes, " Pk=", Key/bytes, "|">>.

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
