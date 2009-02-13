-module(bot).
-compile(export_all).

-include("records.hrl").

client() ->
    #client{ nick    = "erlhubd",
             my_info = <<"server bot$ $$test@test.com$0$">>}.

greeting() ->
    "greetings mortal!".
