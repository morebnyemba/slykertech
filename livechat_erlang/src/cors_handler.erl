%% CORS Preflight Handler for WebSocket connections
-module(cors_handler).
-export([init/2]).

init(Req0, State) ->
    %% Get the Origin header
    Origin = cowboy_req:header(<<"origin">>, Req0, <<"">>),
    
    %% Get the request method
    Method = cowboy_req:method(Req0),
    
    %% Check if this is an OPTIONS request (CORS preflight)
    case Method of
        <<"OPTIONS">> ->
            %% Check if Origin is allowed using shared configuration
            case cors_config:is_origin_allowed(Origin) of
                true when Origin =/= <<"">> ->
                    %% Send CORS headers for preflight
                    Req = cowboy_req:reply(204,
                        #{
                            <<"access-control-allow-origin">> => Origin,
                            <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                            <<"access-control-allow-headers">> => <<"Content-Type, Authorization, X-Requested-With">>,
                            <<"access-control-allow-credentials">> => <<"true">>,
                            <<"access-control-max-age">> => <<"86400">>
                        },
                        <<>>,
                        Req0),
                    {ok, Req, State};
                _ ->
                    %% Origin not allowed or empty
                    Req = cowboy_req:reply(403,
                        #{<<"content-type">> => <<"text/plain">>},
                        <<"Forbidden: Invalid origin">>,
                        Req0),
                    {ok, Req, State}
            end;
        _ ->
            %% Not an OPTIONS request, return method not allowed
            Req = cowboy_req:reply(405,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Method Not Allowed">>,
                Req0),
            {ok, Req, State}
    end.
