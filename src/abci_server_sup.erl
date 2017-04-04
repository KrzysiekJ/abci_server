-module(abci_server_sup).
-behaviour(supervisor).

-export(
   [init/1,
    start_link/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init({CallbackMod, Port}) ->
    ListenerSpec = abci_server:child_spec(CallbackMod, Port),
    {ok, {{one_for_one, 10, 10}, [ListenerSpec]}}.
