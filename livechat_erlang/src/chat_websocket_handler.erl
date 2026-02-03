%% Chat WebSocket Handler
-module(chat_websocket_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-define(PING_INTERVAL, 25000).

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

    %% Start heartbeat timer
    erlang:send_after(?PING_INTERVAL, self(), ping),
    
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
                Department = maps:get(<<"department">>, Data, <<"support">>),
                SessionId = maps:get(<<"session_id">>, Data, null),
                UserId = maps:get(<<"user_id">>, Data, null),
                
                io:format("LiveChat: Message from ~p (session: ~p, user: ~p): ~p~n", 
                         [VisitorName, SessionId, UserId, MessageText]),
                
                %% Build request payload for Django
                RequestPayload = #{
                    message => MessageText,
                    visitor_name => VisitorName,
                    department => Department
                },
                
                %% Add session_id if present
                RequestPayload2 = case SessionId of
                    null -> RequestPayload;
                    _ -> maps:put(session_id, SessionId, RequestPayload)
                end,
                
                %% Add user_id if present
                RequestPayload3 = case UserId of
                    null -> RequestPayload2;
                    _ -> maps:put(user_id, UserId, RequestPayload2)
                end,
                
                %% Call Django AI bridge for response
                case django_http:call_api(post, "/api/livechat/bridge/ai_response/", RequestPayload3) of
                    {ok, Body} ->
                        BodyBin = case is_binary(Body) of
                            true -> Body;
                            false -> iolist_to_binary(Body)
                        end,
                        Decoded = try jsx:decode(BodyBin, [return_maps]) of
                            Map -> {ok, Map}
                        catch
                            _:_ -> {error, invalid_json}
                        end,
                        case Decoded of
                            {ok, #{<<"type">> := _} = ResponseMap} ->
                                ResponseMsg = jsx:encode(ResponseMap),
                                {reply, {text, ResponseMsg}, State};
                            {ok, #{<<"response">> := ResponseText}} ->
                                ResponseMsg = jsx:encode(#{
                                    <<"type">> => <<"message">>,
                                    <<"message">> => ResponseText,
                                    <<"sender">> => <<"Support Team">>
                                }),
                                {reply, {text, ResponseMsg}, State};
                            _ ->
                                FallbackMsg = jsx:encode(#{
                                    <<"type">> => <<"system">>,
                                    <<"message">> => <<"Support team will respond shortly">>
                                }),
                                {reply, {text, FallbackMsg}, State}
                        end;
                    {error, Reason} ->
                        io:format("LiveChat: Django bridge error: ~p~n", [Reason]),
                        FallbackMsg = jsx:encode(#{
                            <<"type">> => <<"system">>,
                            <<"message">> => <<"Support team will respond shortly">>
                        }),
                        {reply, {text, FallbackMsg}, State}
                end;
            <<"ping">> ->
                PongMsg = jsx:encode(#{
                    <<"type">> => <<"pong">>,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }),
                {reply, {text, PongMsg}, State};
            <<"pong">> ->
                {ok, State};
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

websocket_info(ping, State) ->
    %% Send heartbeat ping
    PingMsg = jsx:encode(#{
        <<"type">> => <<"ping">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    }),
    erlang:send_after(?PING_INTERVAL, self(), ping),
    {reply, {text, PingMsg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
