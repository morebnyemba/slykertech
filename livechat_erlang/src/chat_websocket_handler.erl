%% Chat WebSocket Handler
-module(chat_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

%% Allowed origins for CORS
-define(ALLOWED_ORIGINS, [
    <<"https://slykertech.co.zw">>,
    <<"https://www.slykertech.co.zw">>,
    <<"http://localhost:3000">>,
    <<"http://127.0.0.1:3000">>
]).

init(Req0, State) ->
    %% Get the Origin header
    Origin = cowboy_req:header(<<"origin">>, Req0, <<"">>),
    
    %% Check if Origin is allowed
    case lists:member(Origin, ?ALLOWED_ORIGINS) of
        true ->
            %% Origin is allowed, proceed with WebSocket upgrade
            {cowboy_websocket, Req0, State};
        false when Origin =:= <<"">> ->
            %% No origin header (direct connection), allow it
            {cowboy_websocket, Req0, State};
        false ->
            %% Origin not allowed, reject connection
            Req = cowboy_req:reply(403,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Forbidden: Invalid origin">>,
                Req0),
            {ok, Req, State}
    end.

websocket_init(State) ->
    %% Initialize WebSocket connection
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle incoming message
    %% Call Django API for AI response
    Response = django_http:call_api(post, "/api/livechat/bridge/ai_response/", #{message => Msg}),
    {reply, {text, <<"AI response">>}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
