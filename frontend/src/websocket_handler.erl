-module(websocket_handler).

-export([init/2, terminate/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
    lager:debug("~p init", [?MODULE]),
    frontend:add_websocket(self()),
    {cowboy_websocket, Req, Opts}.


%% Handle messages received from the client
websocket_handle({text, Msg}, Req, State) ->
    lager:debug("Received message from client ~p", [Msg]),
    DecodedMsg = jsx:decode(Msg),
    case DecodedMsg of
        [{<<"cmd">>, <<"keypress">>}, {<<"arg">>, Arg}] -> xcomm_srv:keypress(Arg);
        [{<<"cmd">>, Cmd}, {<<"arg">>, Arg}] -> lager:error("Received on websocket an unknown cmd ~p with argument ~p", [Cmd, Arg])
    end,
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    lager:error("~p unknown handle ~p", [?MODULE, _Data]),
    {ok, Req, State}.


%% Handle erlang messages and send data to the client
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, jsx:encode([{cmd, do_something},{arg,Msg}])}, Req, State};
websocket_info(_Info, Req, State) ->
    lager:error("~p unknown info ~p", [?MODULE, _Info]),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    frontend:del_websocket(self()),
    ok.
