%% Chat WebSocket Handler
-module(chat_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    %% Get the Origin header
    Origin = cowboy_req:header(<<"origin">>, Req0, <<"">>),
    
    %% Check if Origin is allowed using shared configuration
    case cors_config:is_origin_allowed(Origin) of
        true ->
            %% Origin is allowed, proceed with WebSocket upgrade
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
