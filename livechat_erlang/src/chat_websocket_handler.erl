%% Chat WebSocket Handler
-module(chat_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req0, State) ->
    %% Get the Origin header for CORS validation
    Origin = cowboy_req:header(<<"origin">>, Req0, <<"">>),
    
    %% Log connection attempt
    io:format("LiveChat: New connection attempt from origin: ~p~n", [Origin]),
    
    %% Check if Origin is allowed
    case cors_config:is_origin_allowed(Origin) of
        true ->
            %% Origin is allowed, proceed with WebSocket upgrade
            {cowboy_websocket, Req0, State};
        false ->
            %% Origin not allowed, reject connection
            io:format("LiveChat: Rejected connection from unauthorized origin: ~p~n", [Origin]),
            Req = cowboy_req:reply(403,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Forbidden: Invalid origin">>,
                Req0),
            {ok, Req, State}
    end.

websocket_init(State) ->
    %% Initialize WebSocket connection
    io:format("LiveChat: WebSocket initialized~n", []),
    
    %% Send welcome message
    WelcomeMsg = jsx:encode(#{
        <<"type">> => <<"system">>,
        <<"message">> => <<"Connected to Slyker Tech Live Support">>
    }),
    
    {reply, {text, WelcomeMsg}, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle incoming message
    io:format("LiveChat: Received message: ~p~n", [Msg]),
    
    try
        Data = jsx:decode(Msg, [return_maps]),
        io:format("LiveChat: Decoded data: ~p~n", [Data]),
        
        case maps:get(<<"type">>, Data, <<"message">>) of
            <<"message">> ->
                %% User message - for now, send a simple response
                MessageText = maps:get(<<"message">>, Data, <<"">>),
                VisitorName = maps:get(<<"visitor_name">>, Data, <<"Guest">>),
                
                io:format("LiveChat: Message from ~p: ~p~n", [VisitorName, MessageText]),
                
                %% Create response
                ResponseMsg = jsx:encode(#{
                    <<"type">> => <<"message">>,
                    <<"message">> => <<"Thank you for your message. Our support team will assist you shortly.">>,
                    <<"sender">> => <<"Support Team">>
                }),
                
                {reply, {text, ResponseMsg}, State};
            _ ->
                io:format("LiveChat: Unknown message type~n", []),
                {ok, State}
        end
    catch
        Type:Error ->
            %% JSON parse error or other error
            io:format("LiveChat: Error processing message - ~p:~p~n", [Type, Error]),
            
            ParseErrorMsg = jsx:encode(#{
                <<"type">> => <<"system">>,
                <<"message">> => <<"Invalid message format">>
            }),
            {reply, {text, ParseErrorMsg}, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
