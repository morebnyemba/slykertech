-module(livechat_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start HTTP listener on port 4001
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/ws", chat_websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 4001}],
        #{env => #{dispatch => Dispatch}}
    ),
    livechat_sup:start_link().

stop(_State) ->
    ok.
