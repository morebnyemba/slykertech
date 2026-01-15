%% CORS Preflight Handler for WebSocket connections
-module(cors_handler).
-export([init/2]).

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
    
    %% Get the request method
    Method = cowboy_req:method(Req0),
    
    %% Check if this is an OPTIONS request (CORS preflight)
    case Method of
        <<"OPTIONS">> ->
            %% Check if Origin is allowed
            case lists:member(Origin, ?ALLOWED_ORIGINS) of
                true ->
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
                false ->
                    %% Origin not allowed
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
