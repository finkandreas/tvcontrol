-module(backend_fsm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    lager:debug("~p start_link", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    lager:debug("~p init", [?MODULE]),
    {ok, { #{strategy => one_for_one, intensity => 3, period => 3600}, [
      ?CHILD(backend_fsm, worker),
      ?CHILD(xcomm_srv, worker)
    ]} }.
