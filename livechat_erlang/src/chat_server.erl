-module(chat_server).
-behaviour(gen_server).

-export([start_link/0, create_session/1, send_message/2, get_session/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sessions = #{}}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_session(SessionData) ->
    gen_server:call(?MODULE, {create_session, SessionData}).

send_message(SessionId, Message) ->
    gen_server:call(?MODULE, {send_message, SessionId, Message}).

get_session(SessionId) ->
    gen_server:call(?MODULE, {get_session, SessionId}).

init([]) ->
    {ok, #state{}}.

handle_call({create_session, SessionData}, _From, State) ->
    SessionId = erlang:unique_integer([positive, monotonic]),
    Session = maps:put(id, SessionId, SessionData),
    Session1 = maps:put(messages, [], Session),
    NewSessions = maps:put(SessionId, Session1, State#state.sessions),
    {reply, {ok, Session1}, State#state{sessions = NewSessions}};

handle_call({send_message, SessionId, Message}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, not_found) of
        not_found ->
            {reply, {error, session_not_found}, State};
        Session ->
            Messages = maps:get(messages, Session, []),
            UpdatedMessages = [Message | Messages],
            UpdatedSession = maps:put(messages, UpdatedMessages, Session),
            NewSessions = maps:put(SessionId, UpdatedSession, State#state.sessions),
            {reply, {ok, UpdatedSession}, State#state{sessions = NewSessions}}
    end;

handle_call({get_session, SessionId}, _From, State) ->
    Result = maps:get(SessionId, State#state.sessions, not_found),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
