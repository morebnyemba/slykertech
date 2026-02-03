-module(livechat_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start HTTP listener on port 4001
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/ws/chat/[...]", chat_websocket_handler, []},
            {"/ws/[...]", chat_websocket_handler, []},
            {'_', default_handler, []}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 4001}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("LiveChat service started on port 4001~n", []),
    livechat_sup:start_link().

stop(_State) ->
    ok.
