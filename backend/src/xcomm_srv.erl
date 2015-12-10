-module(xcomm_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, keypress/1, typetext/1, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

keypress(Key) ->
    gen_server:call(?MODULE, {keypress, {Key}}).

typetext(Text) ->
    gen_server:call(?MODULE, {typetext, {Text}}).

stop() ->
    gen_server:call(?MODULE, {stop, {stop}}).

start_link() ->
    lager:debug("~p start_link", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    lager:debug("~p init", [?MODULE]),
    PrivDir = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    process_flag(trap_exit, true),
    Port = open_port({spawn, filename:join(PrivDir, "xcomm")}, [{packet, 4}, binary, nouse_stdio, exit_status]),
    {ok, Port}.

handle_call({stop, {stop}}, _From, Port) ->
    lager:debug("~p stopping port", [?MODULE]),
    erlang:port_command(Port, term_to_binary({stop, {"stop"}})),
    erlang:port_close(Port),
    exit(normal);

handle_call({Cmd, Args}, _From, Port) when is_tuple(Args)->
    erlang:port_command(Port, term_to_binary({Cmd, Args})),
    {reply, ok, Port};

handle_call(_Req, _From, Port) ->
    lager:error("~p unknown call ~p", [?MODULE, _Req]),
    {reply, ok, Port}.


handle_cast(_Msg, Port) ->
    lager:error("~p unknown cast ~p", [?MODULE, _Msg]),
    {noreply, Port}.


handle_info({'EXIT', Port, normal}, Port) ->
    lager:debug("~p Port was normally closed", [?MODULE]),
    {noreply, Port};
handle_info({Port, {exit_status, ExitStatus}}, Port) ->
    lager:error("~p Port crashed with exit code ~p", [?MODULE, ExitStatus]),
    exit(port_crashed);
handle_info(_Info, Port) ->
    lager:error("~p unknown info ~p", [?MODULE, _Info]),
    {noreply, Port}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, Port, _Extra) ->
    {ok, Port}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

