%% CORS Configuration Module
%% Shared configuration for allowed origins across the livechat application
-module(cors_config).
-export([allowed_origins/0, is_origin_allowed/1]).

%% Define allowed origins for CORS
%% These should match the origins configured in Django's CORS_ALLOWED_ORIGINS
-spec allowed_origins() -> [binary()].
allowed_origins() ->
    [
        <<"https://slykertech.co.zw">>,
        <<"https://www.slykertech.co.zw">>,
        <<"http://localhost:3000">>,
        <<"http://127.0.0.1:3000">>
    ].

%% Check if an origin is allowed
%% @param Origin The origin header value to check
%% @return true if origin is allowed or empty (direct connection), false otherwise
-spec is_origin_allowed(binary()) -> boolean().
is_origin_allowed(<<>>) ->
    %% Allow direct connections without Origin header
    true;
is_origin_allowed(Origin) ->
    lists:member(Origin, allowed_origins()).
