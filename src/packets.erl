-module(packets).
-compile(export_all).

-include("records.hrl").

lock(Lock, Key) ->
    <<"$Lock ", Lock/bytes, " Pk=", Key/bytes, "|">>.

validate_denide(Nick) ->
    io:format("denide ~p~n", [Nick]),
    NickBin = list_to_binary(Nick),
    io:format("benibe ~p~n", [NickBin]),
    <<"$ValidateDenide ", NickBin/binary, "|">>.

hello(Nick) ->
    NickBin = list_to_binary(Nick),
    <<"$Hello ", NickBin/binary, "|">>.

my_info(#client{my_info = undefined}) ->
    skip;
my_info(#client{nick = Nick, my_info = Info}) ->
    NickBin = list_to_binary(Nick),
    <<"$MyINFO $ALL ", NickBin/binary, " ", Info/bytes, "|">>.
