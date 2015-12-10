-module(frontend_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    lager:debug("~p start/0", [?MODULE]),
    {ok, StartedApps} = application:ensure_all_started(frontend),
    lager:notice("Started these applications for the frontend: ~p", [StartedApps]).

start(_StartType, _StartArgs) ->
    lager:debug("~p start/2", [?MODULE]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, frontend, "static/index.html"}},
            {"/ws", websocket_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, frontend, "static/", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_http(my_http_listener, 100, [{port, 9090}], [{env, [{dispatch, Dispatch}]}]),
    frontend_sup:start_link().

stop(_State) ->
    lager:debug("~p stop", [?MODULE]),
    ok.
