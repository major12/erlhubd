-module(erlhubd_sup).
-author('keymone <keymone@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, upgrade/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade() ->
    {ok, {_, Specs}} = init([]),
    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),
    sets:fold(fun (Id, ok) -> 
                  supervisor:terminate_child(?MODULE, Id),
                  supervisor:delete_child(?MODULE, Id),
                  ok
              end, ok, Kill),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

init([]) ->
    P = {erlhubd_server, {tcp_server, start, [erlhubd_server, 4111, {client, init}]},
         permanent, 5000, worker, dynamic},
    {ok, {{one_for_one, 10, 10}, [P]}}.
