%% Django HTTP Client for Erlang
-module(django_http).
-export([call_api/3]).

call_api(Method, Path, Body) ->
    BaseUrl = "http://backend:8000",
    Url = BaseUrl ++ Path,
    Headers = [{"Content-Type", "application/json"}],
    
    case Method of
        post ->
            JsonBody = jsx:encode(Body),
            httpc:request(post, {Url, Headers, "application/json", JsonBody}, [], []);
        get ->
            httpc:request(get, {Url, Headers}, [], [])
    end.
