-module(frontend).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, add_websocket/1, del_websocket/1, send_msg/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {websockets = sets:new() }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    lager:debug("~p start_link", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_websocket(Ref) ->
    gen_server:cast(?MODULE, {add_websocket, Ref}).

del_websocket(Ref) ->
    gen_server:cast(?MODULE, {del_websocket, Ref}).

send_msg(Msg) ->
    gen_server:cast(?MODULE, {send_msg, Msg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    lager:debug("~p init", [?MODULE]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    lager:error("~p unknown call ~p", [?MODULE, _Request]),
    {reply, ok, State}.

handle_cast({add_websocket, Ref}, State) ->
    lager:debug("Registering websocket ~p", [Ref]),
    NewSet = sets:add_element(Ref, State#state.websockets),
    lager:debug("nbr of registered websockets ~p", [sets:size(NewSet)]),
    {noreply, State#state{websockets=NewSet}};
handle_cast({del_websocket, Ref}, State) ->
    lager:debug("Unregistering websocket ~p", [Ref]),
    NewSet = sets:del_element(Ref, State#state.websockets),
    lager:debug("nbr of remaining websockets ~p", [sets:size(NewSet)]),
    {noreply, State#state{websockets=NewSet}};
handle_cast({send_msg, Msg}, State) ->
    lager:debug("Sending a message to the websockets ~p", [Msg]),
    sets:fold(fun(Ws, null) -> Ws ! Msg, null end, null, State#state.websockets),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:error("~p unknown cast ~p", [?MODULE, _Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:error("~p unknown call ~p", [?MODULE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

