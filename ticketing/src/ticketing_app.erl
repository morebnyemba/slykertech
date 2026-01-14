-module(ticketing_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start HTTP listener on port 4000
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/ws", ticket_websocket_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 4000}],
        #{env => #{dispatch => Dispatch}}
    ),
    ticketing_sup:start_link().

stop(_State) ->
    ok.
