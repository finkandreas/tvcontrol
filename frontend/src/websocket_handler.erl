-module(websocket_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

init(Req, Opts) ->
    frontend:add_websocket(self()),
    {cowboy_websocket, Req, Opts}.


%% Handle messages received from the client
websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.


%% Handle erlang messages and send data to the client
websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, jsx:encode([{cmd, do_something},{arg,Msg}])}, Req, State};
websocket_info(_Info, Req, State) ->
    io:format("websocket_info ~p~n", [_Info]),
    {ok, Req, State}.

