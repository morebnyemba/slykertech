%% Chat WebSocket Handler
-module(chat_websocket_handler).
-export([init/2, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle incoming message
    %% Call Django API for AI response
    Response = django_http:call_api(post, "/api/livechat/bridge/ai_response/", #{message => Msg}),
    {reply, {text, <<"AI response">>}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
