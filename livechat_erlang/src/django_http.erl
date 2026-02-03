%% Django HTTP Client for Erlang
-module(django_http).
-export([call_api/3]).

call_api(Method, Path, Body) ->
    BaseUrl = case os:getenv("DJANGO_API_URL") of
        false -> "http://backend:8000";
        EnvUrl -> EnvUrl
    end,
    Url = BaseUrl ++ Path,
    Headers = [{"Content-Type", "application/json"}],
    RequestOptions = [{timeout, 30000}, {connect_timeout, 5000}],
    
    case Method of
        post ->
            JsonBody = jsx:encode(Body),
            io:format("LiveChat: Calling Django API: ~s~n", [Url]),
            case httpc:request(post, {Url, Headers, "application/json", JsonBody}, RequestOptions, []) of
                {ok, {{_, 200, _}, _RespHeaders, RespBody}} -> 
                    io:format("LiveChat: Django API success (200)~n"),
                    {ok, RespBody};
                {ok, {{_, Status, _}, _RespHeaders, RespBody}} -> 
                    io:format("LiveChat: Django API error (~p): ~s~n", [Status, RespBody]),
                    {error, {http_error, Status, RespBody}};
                {error, Reason} -> 
                    io:format("LiveChat: Django API connection error: ~p~n", [Reason]),
                    {error, Reason}
            end;
        get ->
            case httpc:request(get, {Url, Headers}, RequestOptions, []) of
                {ok, {{_, 200, _}, _RespHeaders, RespBody}} -> {ok, RespBody};
                {ok, {{_, Status, _}, _RespHeaders, RespBody}} -> {error, {http_error, Status, RespBody}};
                {error, Reason} -> {error, Reason}
            end
    end.
