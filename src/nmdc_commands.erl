-module(nmdc_commands).
-export([handler/1]).

handler('Key') ->
    {nmdc_handler, key};
handler('Lock') ->
    {nmdc_handler, lock};
handler('ValidateNick') ->
    {nmdc_handler, validate_nick};
handler('Version') ->
    {nmdc_handler, version};
handler('GetNickList') ->
    {nmdc_handler, get_nick_list};
handler('MyINFO') ->
    {nmdc_handler, my_info};
handler('ConnectToMe') ->
    {nmdc_handler, connect_to_me};
handler('GetINFO') ->
    {nmdc_handler, get_info};
handler('To:') ->
    {nmdc_handler, to};
handler(_Unknown) ->
    {nmdc_handler, unknown_command}.