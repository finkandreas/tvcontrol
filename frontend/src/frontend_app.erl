-module(frontend_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    {ok, StartedApps} = application:ensure_all_started(frontend),
    io:format("Started these applications for the frontend: ~p~n", [StartedApps]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, frontend, "static/index.html"}}
        ]}
    ]),
    {ok, _Pid} = cowboy:start_http(my_http_listener, 100, [{port, 9090}], [{env, [{dispatch, Dispatch}]}]),
    frontend_sup:start_link().

stop(_State) ->
    ok.
