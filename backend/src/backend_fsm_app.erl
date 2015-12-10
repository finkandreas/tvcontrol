-module(backend_fsm_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    lager:debug("~p start/0", [?MODULE]),
    {ok, StartedApps} = application:ensure_all_started(backend_fsm),
    lager:notice("Started these applications for the backend_fsm: ~p~n", [StartedApps]).

start(_StartType, _StartArgs) ->
    lager:debug("~p start/2", [?MODULE]),
    backend_fsm_sup:start_link().

stop(_State) ->
    ok.
