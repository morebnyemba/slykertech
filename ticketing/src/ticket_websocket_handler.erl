%% Ticket WebSocket Handler
-module(ticket_websocket_handler).
-export([init/2, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle ticket updates
    {reply, {text, <<"Ticket updated">>}, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
