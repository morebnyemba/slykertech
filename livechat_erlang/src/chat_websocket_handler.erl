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
    %% Send welcome message
    WelcomeMsg = jsx:encode(#{
        type => <<"system">>,
        message => <<"Connected to Slyker Tech Live Support">>
    }),
    {reply, {text, WelcomeMsg}, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle incoming message
    try
        Data = jsx:decode(Msg, [return_maps]),
        
        case maps:get(<<"type">>, Data, <<"message">>) of
            <<"message">> ->
                %% User message - call Django API for AI response
                MessageText = maps:get(<<"message">>, Data, <<"">>),
                VisitorName = maps:get(<<"visitor_name">>, Data, <<"Guest">>),
                
                case django_http:call_api(post, "/api/livechat/bridge/ai_response/", 
                    #{message => MessageText, visitor_name => VisitorName}) of
                    {ok, Response} ->
                        %% Send response back to client
                        ResponseMsg = jsx:encode(Response),
                        {reply, {text, ResponseMsg}, State};
                    {error, _Reason} ->
                        %% Fallback response
                        FallbackMsg = jsx:encode(#{
                            type => <<"system">>,
                            message => <<"Support team will respond shortly">>
                        }),
                        {reply, {text, FallbackMsg}, State}
                end;
            _ ->
                {ok, State}
        end
    catch
        _:_ ->
            %% JSON parse error
            ParseErrorMsg = jsx:encode(#{
                type => <<"system">>,
                message => <<"Invalid message format">>
            }),
            {reply, {text, ParseErrorMsg}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
