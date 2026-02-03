%% Default HTTP Handler for undefined routes
-module(default_handler).
-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    io:format("LiveChat: Unhandled request - ~p ~p~n", [Method, Path]),
    
    Req = cowboy_req:reply(404,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{
            <<"error">> => <<"Not Found">>,
            <<"path">> => Path,
            <<"method">> => Method
        }),
        Req0),
    {ok, Req, State}.
