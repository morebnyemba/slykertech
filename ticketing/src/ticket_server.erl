-module(ticket_server).
-behaviour(gen_server).

-export([start_link/0, create_ticket/1, get_ticket/1, update_ticket/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tickets = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_ticket(TicketData) ->
    gen_server:call(?MODULE, {create_ticket, TicketData}).

get_ticket(TicketId) ->
    gen_server:call(?MODULE, {get_ticket, TicketId}).

update_ticket(TicketId, UpdateData) ->
    gen_server:call(?MODULE, {update_ticket, TicketId, UpdateData}).

init([]) ->
    {ok, #state{}}.

handle_call({create_ticket, TicketData}, _From, State) ->
    TicketId = erlang:unique_integer([positive, monotonic]),
    Ticket = maps:put(id, TicketId, TicketData),
    NewTickets = maps:put(TicketId, Ticket, State#state.tickets),
    {reply, {ok, Ticket}, State#state{tickets = NewTickets}};

handle_call({get_ticket, TicketId}, _From, State) ->
    Result = maps:get(TicketId, State#state.tickets, not_found),
    {reply, Result, State};

handle_call({update_ticket, TicketId, UpdateData}, _From, State) ->
    case maps:get(TicketId, State#state.tickets, not_found) of
        not_found ->
            {reply, {error, not_found}, State};
        Ticket ->
            UpdatedTicket = maps:merge(Ticket, UpdateData),
            NewTickets = maps:put(TicketId, UpdatedTicket, State#state.tickets),
            {reply, {ok, UpdatedTicket}, State#state{tickets = NewTickets}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
